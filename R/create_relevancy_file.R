#' Creates the relevancy file
#'
#' Walks an XLSForm and produces a data frame describing each relevance rule
#' together with the R expression to evaluate against a dataset. Works with
#' both Kobo and SurveyCTO XLSForms - column name differences
#' (`relevant`/`relevance`) are normalized internally.
#'
#' Each row gets a `convert_status` column:
#' \itemize{
#'   \item `"ok"` - rule converted to a parseable R expression.
#'   \item `"unsupported_function"` - rule uses an XPath function the
#'     converter doesn't translate (e.g. `count-selected`, `if`, `regex`,
#'     `coalesce`, `pulldata`, date helpers). `Rcondition` is `NA`.
#'   \item `"unsupported_operator"` - rule uses `div`/`mod`. `Rcondition` is
#'     `NA`.
#'   \item `"parse_error"` - converter produced a string but R could not
#'     parse it. `convert_error` carries the R parse-error message.
#'   \item `"empty"` - rule is blank.
#' }
#' [check_relevancy_rules()] skips every row where `convert_status` is not
#' `"ok"`.
#'
#' @param tool the XLSForm `survey` sheet (data frame).
#' @param ignore_reverse_check The list of question names to be ignored for
#'   reverse checking
#' @param ... For backwards compatibility: the previous argument `kobo_tool`
#'   is still accepted as a deprecated alias for `tool`.
#'
#' @return Data frame with columns `type`, `name`, `relevance_rule`,
#'   `relevant_question`, `relevant_value`, `Rcondition`, `Remarks`,
#'   `check_reverse`, `sheet`, `convert_status`, `convert_error`. The input
#'   for [check_relevancy_rules()].
#' @import stringr
#'
#' @export
create_relevancy_file <- function(tool = NULL, ignore_reverse_check = NULL, ...) {
  tool <- .deprecated_arg(tool, list(...), new_name = "tool", old_name = "kobo_tool")
  if (is.null(tool)) stop("`tool` is required.", call. = FALSE)

  tool <- .normalize_survey(tool)

  if (!all(c("type", "name", "relevant") %in% names(tool))) {
    stop("Required variable(s) not found in XLSForm")
  }

  # Build the field-types lookup *before* process_group_relevancies mutates
  # `tool$relevant` - we want the lookup to reflect the original tool layout.
  field_types <- .xlsform_field_types(tool)

  # Take care of group relevancy (merges open begin_group relevancies into
  # each child question's relevancy).
  tool <- process_group_relevancies(tool)

  # Add Repeat sheet name to Tool's row/question
  questions_sheet_classified <- add_repeat_sheet_names_to_questions(tool = tool)

  empty_cells = c(NA, "", "NA")
  to_be_excluded = c(unique(tool$type[grepl("[Bb]egin[_ ]", tool$type)]),
                     unique(tool$type[grepl("[Ee]nd[_ ]", tool$type)]),
                     "note", "start", "end", "deviceid", "xml-external", "audit", "background-audio")

  tool <- tool[!tool$relevant %in% empty_cells, ]
  tool <- tool[!tool$type %in% to_be_excluded, ]

  if (nrow(tool) == 0) {
    return(.empty_relevancy_file())
  }

  # Pre-classify every rule. Unsupported rules are not handed to the converter.
  convert_status <- vapply(tool$relevant, .classify_relevance, character(1),
                           USE.NAMES = FALSE)

  Rcondition <- rep(NA_character_, nrow(tool))
  convert_error <- rep(NA_character_, nrow(tool))

  ok_idx <- which(convert_status == "ok")
  if (length(ok_idx) > 0) {
    converted <- convert_relevancy_to_R(tool[ok_idx, , drop = FALSE],
                                        field_types = field_types)
    # Parse-test each converted expression. If it doesn't parse, demote to
    # parse_error so check_relevancy_rules() never feeds R bad input.
    for (j in seq_along(ok_idx)) {
      i <- ok_idx[j]
      expr <- converted[j]
      if (is.na(expr)) {
        # Converter chose to skip this rule (it slipped through classification
        # but the converter still bailed). Treat as unsupported.
        convert_status[i] <- "unsupported_function"
        next
      }
      parsed <- tryCatch(parse(text = expr),
                         error = function(e) e,
                         warning = function(w) w)
      if (inherits(parsed, "condition")) {
        convert_status[i] <- "parse_error"
        convert_error[i]  <- conditionMessage(parsed)
      } else {
        # New converter already produces R-source strings whose backslashes
        # survive parse(text=...) correctly. We deliberately do NOT do the
        # legacy gsub("\\\\", "\\\\\\\\", ...) doubling here - that hack was
        # needed by the old converter because it emitted single-quoted regex
        # literals, but it double-escapes our deparse1-produced strings.
        Rcondition[i] <- expr
      }
    }
  }

  relevancy_file <- data.frame(
    type           = unlist(lapply(strsplit(tool$type, " "), function(x) x[1])),
    name           = tool[, "name"] |> unlist() |> unname(),
    relevance_rule = tool$relevant,
    relevant_question = questions_from_relevancy(relevancy_string = tool$relevant),
    relevant_value    = choices_from_relevancy(relevancy_string = tool$relevant),
    Rcondition     = Rcondition,
    Remarks        = "",
    check_reverse  = "TRUE",
    convert_status = convert_status,
    convert_error  = convert_error,
    stringsAsFactors = FALSE
  )

  if (!is.null(ignore_reverse_check) && length(ignore_reverse_check) > 0) {
    relevancy_file$check_reverse[relevancy_file$name %in% ignore_reverse_check] <- "FALSE"
  }

  relevancy_file <- relevancy_file %>%
    left_join(questions_sheet_classified, by = "name")

  # Summary message so the caller sees, at a glance, how many rules were
  # skipped and why.
  .report_convert_summary(relevancy_file$convert_status)

  relevancy_file
}

.empty_relevancy_file <- function() {
  data.frame(
    type = character(), name = character(), relevance_rule = character(),
    relevant_question = character(), relevant_value = character(),
    Rcondition = character(), Remarks = character(),
    check_reverse = character(), convert_status = character(),
    convert_error = character(), sheet = character(),
    stringsAsFactors = FALSE
  )
}

.report_convert_summary <- function(status) {
  n_total <- length(status)
  if (n_total == 0) return(invisible())
  tab <- table(status)
  n_ok <- as.integer(tab["ok"]); if (is.na(n_ok)) n_ok <- 0L
  n_skip <- n_total - n_ok
  if (n_skip == 0) return(invisible())
  parts <- vapply(setdiff(names(tab), "ok"),
                  function(k) sprintf("%s=%d", k, tab[[k]]),
                  character(1))
  message(sprintf("create_relevancy_file: %d/%d rules converted (skipped: %s).",
                  n_ok, n_total, paste(parts, collapse = ", ")))
}
