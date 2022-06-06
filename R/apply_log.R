#' Apply data cleaning/change log
#'
#' Apply cleaning log
#' @usage
#' library(atRfunctions)
#' apply_log(data, log,
#'           data_KEY = "KEY",
#'           log_columns = c(question = "question",
#'                           old_value = "old_value",
#'                           new_value = "new_value",
#'                           KEY = "KEY"
#'                            )
#'           )
#'
#' @param data Data set
#' @param log The log file
#' @param date_KEY The Unique identifier in data set. Must be same as the KEY in log file
#' @param log_column Column names in log file
#' @note It is suggested to rename the column names in the log file as below
#'
#'    **question**: column names in the data which the changes should apply to
#'
#'    **old_value**: the current value for the respective column
#'
#'    **new_value**: the value to replace the old_value with
#'
#'    **KEY**: unique identifier in log
#' @export
apply_log <- function(data, log,
                      data_KEY = "KEY",
                      log_columns = c(question = "question",
                                      old_value = "old_value",
                                      new_value = "new_value",
                                      KEY = "KEY"
                                      )
                      ) {

  for (rowi in 1:nrow(log)){

    var_i  <- log[[log_columns[["question"]]]][rowi]
    old_i  <- log[[log_columns[["old_value"]]]][rowi]
    new_i  <- log[[log_columns[["new_value"]]]][rowi]
    uuid_i <- log[[log_columns[["KEY"]]]][rowi]

    if (var_i %in% colnames(data)) {

      var_type <- class(data[[var_i]])

      if (var_type %in% "character") {
        data[data[[data_KEY]] %in% uuid_i, var_i] <- as.character(new_i)
      } else { # c("numeric", "double", "integer")
        data[data[[data_KEY]] %in% uuid_i, var_i] <- as.numeric(new_i)
      }

      print(paste("uuid:", uuid_i, "Old value: ", old_i, "changed to", new_i, "for", var_i))
    }

  }

  return(data)
}
