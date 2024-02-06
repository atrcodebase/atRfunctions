#' Generate translation log
#'
#' @usage
#' library(atRfunctions)
#' missing_translation(data, KEY, excluded_cols)
#'
#' @param data dataframe
#' @param KEY unique identifier; Default value is "KEY"
#' @param excluded_cols list of columns to be ignored
#' @import tidyr
#' @import dplyr
#' @export
missing_translation <- function(data, KEY = "KEY", excluded_cols){

  question <- c(); old_value <- c(); uuid <- c()
  data_cols <- colnames(data)[colnames(data) %notin% excluded_cols]
  special_characters <- "–|’|é|ý|\\‘|\\’|\\−"

  #checking each column
  for(col_name in data_cols){

    # remove UTF characters to avoid false flagging
    cell_values <- as.character(str_remove_all(data[[col_name]], special_characters))
    # Filter UTF strings
    logical_filter <- Encoding(cell_values) %in% "UTF-8"
    cell_val <- data[[col_name]][logical_filter]
    keys <- data[[KEY]][logical_filter]

    # log
    question <- c(question, rep(col_name, length(cell_val)))
    old_value <- c(old_value, cell_val)
    uuid <- c(uuid, keys)
  }
  if(length(question)+length(old_value)+length(uuid) == 0){
    print(paste0("No untranslated data found in: ", deparse(substitute(data))))
    log <- data.frame()
  } else{
    log <- data.frame(question, old_value, new_value=NA, uuid, Remarks=NA) %>% unique()
  }
}
