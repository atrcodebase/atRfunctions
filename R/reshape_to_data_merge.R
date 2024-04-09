#' Reshape the analysis result file into Data Merge format for Adobe InDesign
#'
#' @param analysis_result Data Analysis Result Dataset
#'
#' @return dataframe
#' @export
#'
#' @import devtools

reshape_to_datamerge = function(analysis_result){
  required_cols <- c("Disaggregation", "Disaggregation_level", "Question", "Response", "Aggregation_method", "Result")
  missing_cols <- required_cols[!required_cols %in% names(analysis_result)]

  if (nrow(analysis_result) == 0) stop("The analysis result is empty!")
  if (length(missing_cols) > 0) stop(paste0(paste0(missing_cols, collapse = ', '), ' is/are missing from analysis result'))
  if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages(package_name)

  duplicate_vars_choices <- analysis_result |>
    mutate(id_cols = paste(Disaggregation, Disaggregation_level, Question, Response)) |>
    filter(duplicated(id_cols) | duplicated(id_cols, fromLast = T))

  if (nrow(duplicate_vars_choices) > 0) stop("The combination of Disaggreagtion, Disaggregation level, Question, and Response is repeated multiple times!")

  reshaped <- analysis_result |>
    select(all_of(required_cols)) |>
    mutate(
      Response = ifelse(is.na(Response), "", Response),
      Result = ifelse(Aggregation_method == 'perc', paste0(Result, "%"), Result),
      Question = ifelse(Aggregation_method != 'perc', paste0(Aggregation_method, ' of ', Question), Question)
      ) |>
    pivot_wider(
      id_cols = c(Disaggregation, Disaggregation_level),
      names_from = c(Question, Response),
      names_sep = "~",values_from = Result
    )

  names(reshaped) <- gsub("~$", "", names(reshaped))

  return(reshaped)
}
