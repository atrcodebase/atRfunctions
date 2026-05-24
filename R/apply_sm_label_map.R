#' Rename select_multiple series columns to use response labels
#'
#' Renames the raw `Question_code` columns in a dataset to their labeled
#' equivalent (e.g. `fruits_3` -> `fruits/Banana`). The second argument is
#' polymorphic:
#'
#' \itemize{
#'   \item Pass a mapping data frame produced by [build_sm_label_map()]
#'     when you want explicit control over how labels are constructed.
#'   \item Pass an XLSForm (path / [read_xlsform()] result / pre-read
#'     `survey` data frame) when you just want the labels applied with
#'     defaults - the function builds the map internally.
#' }
#'
#' @param data A data frame.
#' @param x Either a mapping data frame (must have `dataset_col` and
#'   `labeled_col` columns) or an XLSForm (path, [read_xlsform()] result,
#'   or `survey` data frame). The two cases are distinguished by the
#'   presence of `dataset_col`/`labeled_col` columns.
#' @param question_separator Separator between question name and choice
#'   code in the dataset columns. Default `"_"`.
#' @param excluded_cols Columns to leave untouched (only meaningful when
#'   `x` is a mapping; when `x` is an XLSForm, the same vector is passed
#'   through to [build_sm_label_map()]).
#' @param choice_label Forwarded to [build_sm_label_map()] when `x` is an
#'   XLSForm. Ignored otherwise.
#' @param tool_flavor Forwarded to [build_sm_label_map()] when `x` is an
#'   XLSForm path. Ignored otherwise.
#'
#' @return The data frame with renamed columns.
#' @export
apply_sm_label_map <- function(data, x,
                               question_separator = "_",
                               excluded_cols = "",
                               choice_label = NULL,
                               tool_flavor = "auto") {
  mapping <- if (is.data.frame(x) &&
                 all(c("dataset_col", "labeled_col") %in% names(x))) {
    x
  } else {
    build_sm_label_map(x, excluded_cols = excluded_cols,
                       choice_label = choice_label, tool_flavor = tool_flavor)
  }

  sm_cols <- unique(mapping$Question)
  for (question in sm_cols) {
    series_cols <- names(data)[grepl(
      paste0("^", question, question_separator, "[0-9]{1,5}$"),
      names(data)
    )]
    series_cols <- series_cols[series_cols %notin% excluded_cols]
    for (col in series_cols) {
      replacement <- mapping$labeled_col[mapping$dataset_col %in% col]
      if (length(replacement) == 1) {
        names(data)[names(data) %in% col] <- replacement
      }
    }
  }
  data
}

# ---- deprecated alias ----

#' Rename select_multiple series columns (deprecated)
#'
#' Deprecated alias for [apply_sm_label_map()]. The old name's casing
#' (`SM_Label`) was the only CamelCase mix in the package. Use
#' `apply_sm_label_map()` instead.
#'
#' @inheritParams apply_sm_label_map
#' @param tool Deprecated alias for `x` (a mapping data frame).
#' @return Same as [apply_sm_label_map()].
#' @export
apply_SM_Label <- function(data, tool, question_separator = "_",
                           excluded_cols = "") {
  warning("`apply_SM_Label()` is deprecated; use `apply_sm_label_map()` instead.",
          call. = FALSE)
  apply_sm_label_map(data, tool,
                     question_separator = question_separator,
                     excluded_cols = excluded_cols)
}
