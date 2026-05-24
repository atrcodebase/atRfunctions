#' Creates the relevancy file
#'
#' Works with both Kobo and SurveyCTO XLSForms - column name differences
#' (`relevant`/`relevance`) are normalized internally.
#'
#' @param tool the XLSForm `survey` sheet (data frame).
#' @param ignore_reverse_check The list of question names to be ignored for
#'   reverse checking
#' @param ... For backwards compatibility: the previous argument `kobo_tool`
#'   is still accepted as a deprecated alias for `tool`.
#'
#' @return The input for [check_relevancy_rules()]
#' @import stringr
#'
#' @export
create_relevancy_file <- function(tool = NULL, ignore_reverse_check = NULL, ...) {
  tool <- .deprecated_arg(tool, list(...), new_name = "tool", old_name = "kobo_tool")
  if (is.null(tool)) stop("`tool` is required.", call. = FALSE)

  tool <- .normalize_survey(tool)

  if (!all(c("type", "name", "relevant") %in% names(tool))) {
    stop("Required variable(s) not found in XLSForm")
  }

  # Takes care of group relevancy
  tool <- process_group_relevancies(tool)

  # Add Repeat sheet name to Tool's row/question
  questions_sheet_classified <- add_repeat_sheet_names_to_questions(tool = tool)

  # Specify the column types for which the relevancy check won't be applied
  empty_cells = c(NA, "", "NA")
  to_be_excluded = c(unique(tool$type[grepl("[Bb]egin[_ ]", tool$type)]),
                     unique(tool$type[grepl("[Ee]nd[_ ]", tool$type)]),
                     "note", "start", "end", "deviceid", "xml-external", "audit", "background-audio")

  # Exclude question does not have relevancy
  tool <- tool[!tool$relevant %in% empty_cells, ]
  tool <- tool[!tool$type %in% to_be_excluded, ]

  # Start generating the relevancy file
  relevancy_file = data.frame(type = unlist(lapply(strsplit(tool$type, " "), function(x) x[1])))
  relevancy_file$name = tool[, "name"] |> unlist() |> unname()
  relevancy_file$relevance_rule <- tool$relevant
  relevancy_file$relevant_question <- questions_from_relevancy(relevancy_string = tool$relevant)
  relevancy_file$relevant_value <- choices_from_relevancy(relevancy_string = tool$relevant)
  relevancy_file$Rcondition <- convert_relevancy_to_R(tool)
  relevancy_file$Rcondition <- gsub("\\\\", "\\\\\\\\", relevancy_file$Rcondition)
  relevancy_file$Remarks <- ""
  relevancy_file$check_reverse <- "TRUE"
  if (!is.null(ignore_reverse_check) & length(ignore_reverse_check) > 0) {
    relevancy_file$check_reverse[relevancy_file$name %in% ignore_reverse_check] <- "FALSE"
  }

  relevancy_file <- relevancy_file %>%
    left_join(questions_sheet_classified, by = "name")

  return(relevancy_file)
}
