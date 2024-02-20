#' Extract question list from relevancy rule defined
#'
#' @param relevancy_string The relevancy formula for which the list of question is needed
#'
#' @return
#' @export
questions_from_relevancy = function(relevancy_string){
  # Extract question names from each element of the input vector
  question_names <- regmatches(relevancy_string, gregexpr("\\$\\{([^,}]+)", relevancy_string))
  question_names <- unlist(lapply(question_names, function(x) paste0(gsub("\\$\\{|\\}", "", x), collapse = " - ")))
  return(question_names)
}
