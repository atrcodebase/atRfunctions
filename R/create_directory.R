#' Checks if the path exists, if not, creates it
#'
#' This function checks if the given path/directory exists. If not, it will create it
#'
#' @param path The directory path to create
#'
#' @usage
#' library(atRfunctions)
#' create_directory(path)
#'
#' @import dplyr
#' @export
create_directory <- function(path){
  if (!file.exists(path)) {
    dir.create(path, showWarnings = TRUE, recursive = TRUE)
    cat("Created '", path, "' folder")
  } else {
    cat("The '",path,"' folder already exists")
  }
}

