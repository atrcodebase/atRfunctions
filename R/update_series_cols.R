#' Update Select_multiple series columns
#'
#' This function updates the 'select_multiple' series column values (0s,1s) based on responses in the main question.
#' We need this because sometimes the values of select_multiple questions are updated in correction log but the series columns are not.
#'
#' @param data dataframe
#' @param multi_vars a vector of multi-select questions
#' @param question_separator default value is '/'
#'
#' @examples
#' library(atRfunctions)
#'
#' dt <- data.frame(
#'      q1 = c("A", "A; B", "B; A", "B; C", "A; C"),
#'      q1_A = c(0, 0     , 0      , 0    , 0),
#'      q1_B = c(0, 0     , 0      , 0    , 0),
#'      q1_C = c(0, 0     , 0      , 0    , 0),
#'      q2 = c("option_A", "option_A; option_B", "option_B; option_A", "option_B; option_C", "option_A; option_C"),
#'      "q2_option_A" = c(1, 1     , 1      , 1    , 1),
#'      "q2_option_B" = c(1, 1     , 1      , 1    , 1),
#'      "q2_option_C" = c(1, 1     , 1      , 1    , 1)
#'      )
#'
#' update_series_cols(dt,
#'                    multi_vars = c("q1", "q2"),
#'                    question_separator = "_")
#'
#' @usage
#' library(atRfunctions)
#' update_series_cols(data,
#'                    multi_vars,
#'                    question_separator = "/")
#'
#' @import dplyr
#' @export
update_series_cols <- function(data, multi_vars, question_separator = "/"){
  for(question in multi_vars){
    print(paste0("Updating: ", question))
    # Get all series columns
    series_cols <- names(select(data, starts_with(paste0(question, question_separator))))
    # Make all series cols numeric
    data <- data %>% mutate(across(all_of(series_cols), as.numeric))
    # Get rows with non-NA values
    rows <- which(!is.na(data[[question]]))

    # Loop each series column
    for(column in series_cols){
      # Add word boundary for str_detect (differentiate 1 from 13)
      # response <- paste0("\\b", gsub(paste0(question, "/"), "", column),"\\b")
      # Note: changed '/' with 'question_separator'
      response <- paste0("\\b", gsub(paste0(question, question_separator), "", column),"\\b")
      # Assign 1 if value exists in main question, else 0
      data[rows, column] <- ifelse(str_detect(data[[question]][rows], response), 1, 0)
    }
  }
  return(data)
}
