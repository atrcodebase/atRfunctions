#' Identify and set the sheet names for each question in Data collection tool
#'
#' Accepts a pre-read XLSForm `survey` data frame (Kobo or SurveyCTO). Column
#' name differences (`relevant`/`relevance`) are normalized internally.
#'
#' @param tool path to the XLSForm, a [read_xlsform()] result list, or a
#'   pre-read `survey`-sheet data frame.
#' @param ... For backwards compatibility: the previous argument `kobo_tool`
#'   is still accepted as a deprecated alias for `tool`.
#'
#' @return The list of questions and their sheet names
#' @export
add_repeat_sheet_names_to_questions <- function(tool = NULL, ...) {
  tool <- .deprecated_arg(tool, list(...), new_name = "tool", old_name = "kobo_tool")
  if (is.null(tool)) stop("`tool` is required.", call. = FALSE)

  tool <- .resolve_tool(tool)$survey
  tool <- tool %>% filter(!is.na(type) & type != "")

  begin_repeat_condition = "^begin[_ ]?repeat"
  end_repeat_condition = "^end[_ ]?repeat"

  sheet_name = "data"
  tool$sheet <- sheet_name

  if (any(grepl(begin_repeat_condition, tool$type, ignore.case = T))) {
    for (row in seq_len(nrow(tool))) {
      if (grepl(begin_repeat_condition, tool$type[row])) {
        sheet_name <- c(sheet_name, tool$name[row])
      }

      tool$sheet[row] <- sheet_name[length(sheet_name)]

      if (grepl(end_repeat_condition, tool$type[row])) {
        sheet_name <- sheet_name[-length(sheet_name)]
      }
    }
  }

  result <- tool %>% select(name, sheet) %>% distinct(name, .keep_all = T)
  return(result)
}
