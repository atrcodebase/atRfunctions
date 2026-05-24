#' Generate translation log
#'
#' @param data dataframe
#' @param KEY unique identifier; Default value is "KEY"
#' @param excluded_cols list of columns to be ignored
#'
#' @return A data frame of untranslated values with columns `question`,
#'   `old_value`, `new_value`, `uuid`, `Remarks`. Empty data frame if nothing
#'   was flagged.
#' @note Prior to package version 0.0.2, this function built `log` internally
#'   but did not return it (callers received `NULL`). This was a bug; it now
#'   returns the log.
#' @import tidyr
#' @import dplyr
#' @import stringr
#' @export
missing_translation <- function(data, KEY = "KEY", excluded_cols) {

  question <- c(); old_value <- c(); uuid <- c()
  data_cols <- colnames(data)[colnames(data) %notin% excluded_cols]
  # Special characters preserved verbatim from prior versions of this
  # function. Written with backslash-u escapes so the source file stays
  # pure ASCII (R CMD check requires it).
  special_characters <- "\u2013|\u2019|\u00e9|\u00fd|\\\u2018|\\\u2019|\\\u2212"

  for (col_name in data_cols) {
    cell_values <- as.character(str_remove_all(data[[col_name]], special_characters))
    logical_filter <- Encoding(cell_values) %in% "UTF-8"
    cell_val <- data[[col_name]][logical_filter]
    keys <- data[[KEY]][logical_filter]

    question <- c(question, rep(col_name, length(cell_val)))
    old_value <- c(old_value, cell_val)
    uuid <- c(uuid, keys)
  }
  if (length(question) + length(old_value) + length(uuid) == 0) {
    print(paste0("No untranslated data found in: ", deparse(substitute(data))))
    log <- data.frame()
  } else {
    log <- data.frame(question, old_value, new_value = NA, uuid, Remarks = NA) %>% unique()
  }
  return(log)
}

