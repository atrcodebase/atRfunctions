#' Check Series Cols
#'
#' This function checks if the 'select_multiple' series column values (0s,1s) are based on responses in the main question.
#'
#' @param data dataframe
#' @param tool The survey sheet of data collection tool
#' @param question_separator default value is '_'
#' @param KEY default value is 'KEY'
#'
#' @usage
#' library(atRfunctions)
#' check_select_multiple(data,
#'                    tool_survey,
#'                    question_separator = "_",
#'                    KEY = "KEY")
#'
#' @import dplyr
#' @export
check_select_multiple <- function(data, tool, question_separator="_", KEY="KEY"){
  # Read & Filter tool
  # tool <- read_excel(tool_path, "survey", guess_max = 100000)
  sm_cols <- tool$name[grepl("select_multiple", tool$type) & tool$name %in% names(data)]

  series_log <- data.frame(KEY=NA,question=NA,value=NA,series_columns=NA,
                           series_values=NA,Remarks=NA)

  for(question in sm_cols){
    # print(paste0("Checking: ", question)) # Print
    # Get all series columns
    series_cols <- names(data)[grepl(paste0("^",question, question_separator, "[0-9]{1,4}$"), names(data))] # Regex: detect the question ended by 1 to 4 numbers followed by nothing else
    # Filter NA responses
    data_sub <- data %>%
      select(all_of(question), all_of(series_cols), all_of(KEY)) %>%
      filter(!is.na(get(question)))

    if(nrow(data_sub)!=0){
      for(i in 1:nrow(data_sub)){
        #question value
        val <- str_split(data_sub[[question]][i], " |-")[[1]]
        # make related series column name
        series_columns <- paste0(question,question_separator, val)
        other_columns <- names(data_sub)[names(data_sub) %notin% c(series_columns, question, "KEY")]

        if(!all(series_columns %in% names(data_sub))){
          log <- c(data_sub$KEY[i],
                   question,
                   data_sub[[question]][i],
                   paste0(series_columns, collapse = " - "),
                   "",
                   Remarks="Series column not in data")
          series_log <- rbind(series_log, log)
        } else if(any(data_sub[i,series_columns] %in% c(NA, 0))){
          log <- c(data_sub$KEY[i],
                   question,
                   data_sub[[question]][i],
                   paste0(series_columns, collapse = " - "),
                   paste0(data_sub[i,series_columns], collapse = " - "),
                   Remarks = "Inonsistent series columns")
          series_log <- rbind(series_log, log)
        } else if(any(data_sub[i, other_columns] %in% 1)){

          other_cols <- other_columns[which(data_sub[i, other_columns] %in% 1)]
          log <- c(data_sub$KEY[i],
                   question,
                   data_sub[[question]][i],
                   paste0(other_cols, collapse = " - "),
                   paste0(data_sub[i,other_cols], collapse = " - "),
                   Remarks = "At least one response is not in the tool choices")
          series_log <- rbind(series_log, log)
        }
      }
    }
  }
  if(nrow(series_log) == 1){
    print(paste0("No mismatches found: ", deparse(substitute(data))))
    return(series_log[-1,])
  } else {
    return(series_log[-1,])
  }
}
