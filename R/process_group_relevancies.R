#' Set the relevancy defined for groups on the questions inside
#'
#' @param kobo_tool The data collection tool
#'
#' @return
#' @export
process_group_relevancies <- function(kobo_tool){
  begin_group_relevancies <- c()

  process_question <- function(question_type, relevancy) {

    if (grepl("^[Bb]egin[_]?", question_type)) {
      begin_group_relevancies <<- c(begin_group_relevancies, relevancy)
    } else if (grepl("^[Ee]nd[_]?", question_type) & length(begin_group_relevancies) > 0) {
      begin_group_relevancies <<- head(begin_group_relevancies, n = -1)
    }

    if (!grepl("^([Bb]egin|[Ee]nd)_?", question_type)) {
      begin_group_relevancies_merged <- ifelse(length(begin_group_relevancies) > 0,
                                               paste(unique(begin_group_relevancies[!is.na(begin_group_relevancies)]), collapse = " and "),
                                               "")
      if (!is.na(relevancy)) {
        begin_group_relevancies_merged <- ifelse(begin_group_relevancies_merged != "",
                                                 paste(begin_group_relevancies_merged, relevancy, sep = " and "),
                                                 relevancy)

        begin_group_relevancies_merged <- gsub(" +", " ", begin_group_relevancies_merged)

        solve_duplication = function(log_operator, string){
          splitted_string <- strsplit(string, log_operator)
          new_string = unlist(lapply(splitted_string, function(x) paste(x[!duplicated(x)], collapse = log_operator)))
          return(new_string)
        }

        begin_group_relevancies_merged <- solve_duplication(" and ", begin_group_relevancies_merged)
        begin_group_relevancies_merged <- solve_duplication(" or ", begin_group_relevancies_merged)

      }
      return(begin_group_relevancies_merged)
    } else {
      return(relevancy)
    }
  }
  kobo_tool$relevant <- mapply(process_question, kobo_tool$type, kobo_tool$relevant, SIMPLIFY = TRUE)
  return(kobo_tool)
}
