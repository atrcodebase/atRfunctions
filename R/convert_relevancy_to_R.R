#' Converts the relevancy rules to R statements
#'
#' Accepts a pre-read XLSForm `survey` data frame (Kobo or SurveyCTO). Column
#' name differences (`relevant`/`relevance`) are normalized internally.
#'
#' @param tool the XLSForm `survey` sheet (data frame).
#' @param ... For backwards compatibility: the previous argument `kobo_tool`
#'   is still accepted as a deprecated alias for `tool`.
#'
#' @return The list of R statements converted from Relevancy Formulas
#' @import stringr
#'
#' @export
convert_relevancy_to_R <- function(tool = NULL, ...) {
  tool <- .deprecated_arg(tool, list(...), new_name = "tool", old_name = "kobo_tool")
  if (is.null(tool)) stop("`tool` is required.", call. = FALSE)

  tool <- .normalize_survey(tool)

  if (!all(c("type", "name", "relevant") %in% names(tool))) {
    stop("Required variable(s) not found in XLSForm")
  }

  relevancy_string = tool$relevant
  select_multiple_questions = tool$name[grepl("select_multiple ", tool$type)]

  selected_pattern <- "selected\\(\\$\\{(.+?)\\} *, *['\"](.+?)['\"]\\)"
  strings_containing_not = grepl("not\\(", relevancy_string)
  relevancy_string[strings_containing_not] <- str_replace_all(relevancy_string[strings_containing_not], paste0("not\\(", selected_pattern, "\\)"), "data$\\1 != '\\2'")
  relevancy_string <- gsub(selected_pattern, "data$\\1 = '\\2'", relevancy_string)
  relevancy_string <- gsub("\\$\\{(.*?)\\}", "data$\\1", relevancy_string)

  replacements <- c("(?<![!<>])=(?!=)" = " == ", "> *=" = " >= ", "< *=" = " <= ", "<>" = " != ", "not\\((.*?)\\)" = "\\1", "\n" = " ")
  relevancy_string <- str_replace_all(relevancy_string, replacements)

  question_names <- str_extract_all(relevancy_string, "\\$([[:alnum:]_]+)")
  questions_to_convert <- unique(unlist(lapply(question_names, function(x) return(intersect(unique(gsub("\\$", "", x)), select_multiple_questions)))))

  if (length(questions_to_convert) > 0) {
    for (question in questions_to_convert) {
      filter_condition <- grepl(question, relevancy_string)
      filter_strings <- relevancy_string[filter_condition]
      converted_strings <- str_replace_all(filter_strings, sprintf("data\\$%s *== *['\"](.+?)['\"]", question), paste0("grepl('\\\\b\\1\\\\b', ", paste0("data$", question), ")"))
      converted_strings <- str_replace_all(converted_strings, sprintf("data\\$%s *!= *['\"](.+?)['\"]", question), paste0("!grepl('\\\\b\\1\\\\b', ", paste0("data$", question), ")"))
      relevancy_string[filter_condition] <- converted_strings
    }
  }

  # handling grouped conditions
  relevancy_string <- ifelse(grepl(" or ", relevancy_string), gsub("( and )", ") and (", relevancy_string), relevancy_string)
  relevancy_string <- gsub("(.*)( or )(.*)", "(\\1\\2\\3)", relevancy_string)

  replacements = c(" and " = " & ", " or " = " | ", "\"" = "'", " +" = " ", '"or ' = '" | ', "'or " = "' | ")
  relevancy_string <- str_replace_all(relevancy_string, replacements)

  replacements <- c(" == " = " %in% ", " != " = " %notin% ")
  relevancy_string <- str_replace_all(relevancy_string, replacements)

  return(relevancy_string)
}
