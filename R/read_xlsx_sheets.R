#' Reading XLSX data as a list
#'
#' Read XLSX as list
#' @usage
#' library(atRfunctions)
#' read_xlsx_sheets(
#'          path,
#'          guess_max = 5000000,
#'          convert_to_na = c("N/A", "-", " ")
#'          )
#'
#' @param path Path to the XLSX file
#' @param guess_max Guess Max ; defaults to 5000000
#' @param convert_to_na a list of patterns to be converted to NA ; defaults to c("N/A", "-", " ")
#' @return A list of data set where each item represent XLSX's sheet
#' @import map2
#' @import readxl
#' @import openxlsx
#' @export
read_xlsx_sheets = function(path, guess_max = 5000000, convert_to_na = c("N/A", "-", " ")){
  df <- map2(path, getSheetNames(path), ~ read_excel(.x, sheet = .y, guess_max=guess_max, na=convert_to_na, .name_repair = "minimal"), .progress = TRUE) |> setNames(getSheetNames(path))
  if (length(df) == 1) df <- data.frame(unname(df))
  return(df)
}
