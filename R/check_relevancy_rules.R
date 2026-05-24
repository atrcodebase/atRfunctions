#' Check Relevancy Rule
#'
#' Walks the relevancy file produced by [create_relevancy_file()] and flags
#' rows in `data` whose values violate the rules.
#'
#' Rules whose `convert_status` is anything other than `"ok"` are skipped
#' (their conversion was deferred at file-build time because the XLSForm
#' relevance expression uses constructs this package does not translate -
#' see [create_relevancy_file()]). Rules whose evaluation throws at runtime
#' are skipped too: a log row with `Remarks = "eval_error: <msg>"` is
#' written and the loop continues to the next rule. The attribute
#' `"skipped"` on the returned data frame carries a summary of skips.
#'
#' @param data The data set for which the relevancy will be checked
#' @param tool_relevancy The output of [create_relevancy_file()]
#' @param sheet_name The sheet name of data set in case it holds multiple
#'   sheets. Default value to "data"
#' @param KEY The Unique ID of observations. Default value to "KEY"
#'
#' @return Data frame, containing the relevancy rules broken, with an
#'   attribute `"skipped"` (data frame) listing rules that were not
#'   evaluated and why.
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @export
check_relevancy_rules <- function(data, tool_relevancy, sheet_name = "data", KEY = "KEY") {
  relevancy_log <- data.frame()
  missing_cols <- c()
  missing_relev_cols <- c()
  eval_errors <- data.frame(name = character(), reason = character(),
                            stringsAsFactors = FALSE)

  tool_relevancy <- tool_relevancy %>% filter(sheet == sheet_name)

  # Tolerate older relevancy files that don't carry convert_status.
  if (!"convert_status" %in% names(tool_relevancy)) {
    tool_relevancy$convert_status <- ifelse(is.na(tool_relevancy$Rcondition) |
                                              tool_relevancy$Rcondition == "",
                                            "empty", "ok")
  }

  questions <- tool_relevancy$name
  for (question_i in seq_along(questions)) {
    question <- questions[question_i]
    status <- tool_relevancy$convert_status[question_i]
    relevancy_sub <- tool_relevancy[question_i, ]

    if (!identical(status, "ok")) {
      eval_errors <- rbind(eval_errors,
                           data.frame(name = question,
                                      reason = paste0("skipped: ", status),
                                      stringsAsFactors = FALSE))
      next
    }

    relevant_question <- str_split(relevancy_sub$relevant_question, " - ")[[1]] %>% unique()
    check_reverse <- relevancy_sub$check_reverse

    if (question %notin% names(data)) {
      missing_cols <- c(missing_cols, question)
      next
    }
    if (any(!relevant_question %in% names(data))) {
      missing_relev_cols <- c(missing_relev_cols,
                              relevant_question[!relevant_question %in% names(data)])
      # We can't evaluate without all referenced columns - skip cleanly
      # instead of falling through to a guaranteed eval error.
      eval_errors <- rbind(eval_errors,
                           data.frame(name = question,
                                      reason = "skipped: missing_relevant_column",
                                      stringsAsFactors = FALSE))
      next
    }

    conditional_string <- relevancy_sub$Rcondition
    conditional_str_negated <- paste0("!(", conditional_string, ")")

    forward <- tryCatch(
      which(data[[question]] %notin% c(NA, "", NaN) &
              eval(parse(text = conditional_str_negated))),
      error = function(e) e
    )
    if (inherits(forward, "error")) {
      eval_errors <- rbind(eval_errors,
                           data.frame(name = question,
                                      reason = paste0("eval_error: ", conditionMessage(forward)),
                                      stringsAsFactors = FALSE))
      next
    }

    flagged_rows <- forward
    if (isTRUE(as.logical(check_reverse))) {
      reverse <- tryCatch(
        which(data[[question]] %in% c(NA, "", NaN) &
                eval(parse(text = conditional_string))),
        error = function(e) e
      )
      if (inherits(reverse, "error")) {
        eval_errors <- rbind(eval_errors,
                             data.frame(name = question,
                                        reason = paste0("eval_error_reverse: ", conditionMessage(reverse)),
                                        stringsAsFactors = FALSE))
        # Keep the forward results - reverse failure shouldn't blank them.
      } else {
        flagged_rows <- c(flagged_rows, reverse)
      }
    }
    # Dedup: a row caught by both forward and reverse should appear once.
    flagged_rows <- unique(flagged_rows)

    len_flagged <- length(flagged_rows)
    if (len_flagged > 0) {
      relevant_values <- data[flagged_rows, c(KEY, relevant_question)] %>%
        pivot_longer(-all_of(KEY), names_to = "cols", values_to = "value",
                     values_transform = as.character) %>%
        group_by(across(KEY)) %>%
        mutate(total = paste0(value, collapse = " - "), value = NULL, cols = NULL) %>%
        ungroup() %>% unique() %>% pull(total)

      log <- data.frame(
        KEY = data[[KEY]][flagged_rows],
        question = rep(question, len_flagged),
        value = data[[question]][flagged_rows],
        relevancy_rule = rep(relevancy_sub$relevance_rule[1], len_flagged),
        relevant_question = rep(paste0(relevant_question, collapse = " - "), len_flagged),
        relev_value = relevant_values,
        sheet = sheet_name,
        stringsAsFactors = FALSE
      )
      relevancy_log <- rbind(relevancy_log, log)
    }
  }

  if (length(missing_cols) != 0) {
    message("Column missing from dataset: ",
            paste(unique(missing_cols), collapse = ", "))
  }
  if (length(missing_relev_cols) != 0) {
    message("Relevant column missing from dataset: ",
            paste(unique(missing_relev_cols), collapse = ", "))
  }
  if (nrow(eval_errors) > 0) {
    skipped_tab <- table(sub(":.*$", "", eval_errors$reason))
    parts <- paste(sprintf("%s=%d", names(skipped_tab), as.integer(skipped_tab)),
                   collapse = ", ")
    message(sprintf("Skipped %d rule(s) (%s). See attr(<result>, 'skipped').",
                    nrow(eval_errors), parts))
  }

  if (nrow(relevancy_log) == 0) {
    message("No relevancy issues found in: ", sheet_name)
  }
  attr(relevancy_log, "skipped") <- eval_errors
  relevancy_log
}
