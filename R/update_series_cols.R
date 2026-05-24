#' Update Select_multiple series columns
#'
#' This function updates the 'select_multiple' series column values (0s,1s) based on responses in the main question.
#'
#' @param data dataframe
#' @param tool path to the XLSForm, a [read_xlsform()] result list, or a
#'   pre-read `survey`-sheet data frame.
#' @param question_separator default value is '_'
#'
#'
#' @import dplyr
#' @import stringr
#' @export
update_series_cols <- function(data, tool, question_separator = "_") {
  survey <- .resolve_tool(tool)$survey
  sm_cols <- survey$name[grepl("select_multiple", survey$type) & survey$name %in% names(data)]

  for(question in sm_cols){
    # print(paste0("Updating: ", question)) # Print
    ## Get all series columns
    series_cols <- names(data)[grepl(paste0("^",question, question_separator, "[0-9]{1,4}$"), names(data))] # Regex: detect the question ended by 1 to 4 numbers followed by nothing else
    # Make all series cols numeric
    data <- data %>% mutate(across(all_of(series_cols), as.numeric))

    # Get rows with non-NA values
    rows <- which(!is.na(data[[question]]))
    na_rows <- which(is.na(data[[question]]))

    # Loop each series column
    for(column in series_cols){
      # Add word boundary for str_detect (differentiate 1 from 13)
      response <- paste0("\\b", gsub(paste0(question, question_separator), "", column),"\\b")
      # Assign 1 if value exists in main question, else 0
      data[rows, column] <- ifelse(str_detect(data[[question]][rows], response), 1, 0)
      # Make the rest of the rows na
      data[na_rows, column] <- NA_integer_
    }
  }
  return(data)
}
