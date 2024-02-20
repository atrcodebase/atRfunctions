#' Identify and set the sheets names for each question in Data collection tool
#'
#' @param kobo_tool the kobo tool for which the question's sheet needed
#'
#' @return
#' @export
add_repeat_sheet_names_to_questions <- function(kobo_tool){
  kobo_tool <- kobo_tool %>% filter(!is.na(type) & type != "")

  begin_repeat_condition = "^begin[_ ]?repeat"
  end_repeat_condition = "^end[_ ]?repeat"

  sheet_name = "data"
  kobo_tool$sheet <- sheet_name

  if (any(grepl(begin_repeat_condition, kobo_tool$type, ignore.case = T))) {
    for (row in seq_len(nrow(kobo_tool))) {
      if (grepl(begin_repeat_condition, kobo_tool$type[row])) {
        sheet_name <- c(sheet_name, kobo_tool$name[row])
      }

      kobo_tool$sheet[row] <- sheet_name[length(sheet_name)]

      if (grepl(end_repeat_condition, kobo_tool$type[row])) {
        sheet_name <- sheet_name[-length(sheet_name)]
      }
    }
  }

  result <- kobo_tool %>% select(name, sheet) %>% distinct(name, .keep_all = T)
  return(result)
}
