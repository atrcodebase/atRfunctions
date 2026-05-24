#' Rename select_multiple series columns to use response labels
#'
#' Given the output of [reshape_tool()] (a mapping from raw `Question_code`
#' columns to labeled `Question/label` columns), rename the corresponding
#' columns in a dataset.
#'
#' @param data A data frame.
#' @param tool A data frame produced by [reshape_tool()].
#' @param question_separator Separator between question name and choice code in
#'   the dataset columns. Default `"_"`.
#' @param excluded_cols Columns to leave untouched.
#'
#' @return The renamed data frame.
#' @export
apply_SM_Label <- function(data, tool, question_separator = "_",
                           excluded_cols = "") {
  sm_cols <- unique(tool$Question)
  for (question in sm_cols) {
    series_cols <- names(data)[grepl(
      paste0("^", question, question_separator, "[0-9]{1,5}$"),
      names(data)
    )]
    series_cols <- series_cols[series_cols %notin% excluded_cols]
    for (col in series_cols) {
      replacement <- tool$labeled_col[tool$dataset_col %in% col]
      if (length(replacement) == 1) {
        names(data)[names(data) %in% col] <- replacement
      }
    }
  }
  data
}
