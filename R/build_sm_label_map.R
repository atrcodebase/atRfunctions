#' Build a select_multiple label map from an XLSForm
#'
#' Reads an XLSForm and returns a data frame that maps each raw dataset
#' column for a `select_multiple` question (e.g. `q1_3`) to a labeled
#' equivalent (e.g. `q1/Banana`). The choice label is sanitized to be safe
#' inside an R variable name - characters like `/`, `'`, `(`, `%`, and
#' whitespace are replaced with `_`. Works with both Kobo and SurveyCTO.
#'
#' The output is the input expected by [apply_sm_label_map()].
#'
#' @param tool Path to the XLSForm, a [read_xlsform()] result list, or a
#'   pre-read `read_xlsform()` result.
#' @param excluded_cols Vector of question names to skip.
#' @param choice_label Column name in the `choices` sheet that holds the
#'   choice label. If `NULL` (default), the function tries the typical
#'   XLSForm forms (`label::English`, `label:English`, `label`) and picks
#'   the first one present.
#' @param sm_label_separator Separator placed between the question name and
#'   the sanitized choice label in `labeled_col`. Default `"."` (produces
#'   valid R names like `fruits.Apple`).
#' @param tool_flavor One of `"auto"`, `"kobo"`, `"surveycto"`. Only used
#'   when `tool` is a file path. Default `"auto"`.
#'
#' @return Data frame with columns `Question`, `type`, `select_type`,
#'   `response_code`, `response_label`, `response_label_new`, `dataset_col`,
#'   `labeled_col`. `labeled_col` is
#'   `"<Question><sm_label_separator><sanitized_label>"`.
#' @import dplyr
#' @import stringr
#' @export
build_sm_label_map <- function(tool, excluded_cols = "", choice_label = NULL,
                               sm_label_separator = ".",
                               tool_flavor = "auto") {
  xlsform <- .resolve_tool(tool, tool_flavor = tool_flavor, needs_choices = TRUE)
  tool_survey  <- xlsform$survey
  tool_choices <- xlsform$choices

  label_col <- choice_label %||% .pick_label_col(tool_choices)
  if (!label_col %in% names(tool_choices)) {
    stop(sprintf("Label column '%s' not found in choices sheet.", label_col),
         call. = FALSE)
  }

  tool_survey <- tool_survey %>%
    select(type, Question = name) %>%
    filter(grepl("\\bselect_multiple", type) & Question %notin% excluded_cols) %>%
    mutate(
      select_type = str_replace_all(type, " .*", ""),
      type = str_replace_all(type, "select_one ", "") %>%
        str_replace_all("select_multiple ", "")
    )

  tool_choices <- tool_choices %>%
    filter(!is.na(list_name)) %>%
    select(list_name, response_code = name,
           response_label = all_of(label_col)) %>%
    mutate(
      response_label = str_squish(response_label),
      # Sanitize the label so the resulting column name is a valid R
      # identifier fragment (no slashes, apostrophes, whitespace, etc.).
      response_label_new = .sanitize_r_name(response_label),
      # Non-numeric choice codes (e.g. "yes"/"no") become NA and are
      # dropped by the filter below; suppress the expected NA warning.
      response_code = suppressWarnings(as.numeric(response_code))
    )

  tool_survey %>%
    left_join(tool_choices, by = c("type" = "list_name"),
              relationship = "many-to-many") %>%
    filter(!is.na(response_code) & !is.na(response_label)) %>%
    mutate(
      dataset_col = paste0(Question, "_", response_code),
      labeled_col = paste0(Question, sm_label_separator, response_label_new)
    )
}

# Pick a sensible default label column from those present in the choices sheet.
.pick_label_col <- function(choices) {
  candidates <- c("label::English", "label:English", "label::english",
                  "label:english", "label")
  hit <- candidates[candidates %in% names(choices)]
  if (length(hit) >= 1) return(hit[1])
  label_like <- grep("^label", names(choices), value = TRUE)
  if (length(label_like) >= 1) return(label_like[1])
  stop("No label column found in choices sheet. Pass `choice_label` explicitly.",
       call. = FALSE)
}

# Local null-coalescing operator.
`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- deprecated alias ----

#' Reshape an XLSForm (deprecated)
#'
#' Deprecated alias for [build_sm_label_map()]. The old name was misleading:
#' the function does not reshape the XLSForm itself but builds a column-name
#' lookup table. Use `build_sm_label_map()` instead.
#'
#' @inheritParams build_sm_label_map
#' @return Same as [build_sm_label_map()].
#' @export
reshape_tool <- function(tool, excluded_cols = "", choice_label = NULL,
                         sm_label_separator = ".",
                         tool_flavor = "auto") {
  warning("`reshape_tool()` is deprecated; use `build_sm_label_map()` instead.",
          call. = FALSE)
  build_sm_label_map(tool, excluded_cols = excluded_cols,
                     choice_label = choice_label,
                     sm_label_separator = sm_label_separator,
                     tool_flavor = tool_flavor)
}
