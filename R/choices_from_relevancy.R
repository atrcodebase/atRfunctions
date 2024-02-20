#' Extract choices list from relevancy rule defined
#'
#' @param relevancy_string The relevancy formula for which the list of choices is needed
#'
#' @return The list of choices involved in the relevancy formula
#' @export
choices_from_relevancy <- function(relevancy_string) {
  choices <- gsub(" (and|or) ", " - ", relevancy_string)
  choices <- gsub("selected\\(\\$\\{(.+?)\\} *, *['\"](.+?)['\"]\\)", "\\2", choices)
  choices <- gsub("[cC]oalesce\\(\\$\\{(.+?)\\} *, *(.+?)\\)", "\\2", choices)
  choices <- gsub("sum\\(\\$\\{(.+?)\\}\\)", "", choices)
  choices <- gsub("\\$\\{(.+?)\\} ?|['\"]|not\\(|\\)|\\(|[<>!=]", "", choices)
  return(choices)
}
