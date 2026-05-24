#' Set the relevancy defined for groups on the questions inside
#'
#' Accepts a pre-read XLSForm `survey` data frame (Kobo or SurveyCTO). Column
#' name differences (`relevant`/`relevance`) are normalized internally.
#'
#' @param tool the XLSForm `survey` sheet (data frame).
#' @param ... For backwards compatibility: the previous argument `kobo_tool`
#'   is still accepted as a deprecated alias for `tool`.
#'
#' @return The list of questions and their group name involved
#' @export
process_group_relevancies <- function(tool = NULL, ...) {
  tool <- .deprecated_arg(tool, list(...), new_name = "tool", old_name = "kobo_tool")
  if (is.null(tool)) stop("`tool` is required.", call. = FALSE)

  tool <- .normalize_survey(tool)

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

        solve_duplication = function(log_operator, string) {
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
  tool$relevant <- mapply(process_question, tool$type, tool$relevant, SIMPLIFY = TRUE)
  return(tool)
}
