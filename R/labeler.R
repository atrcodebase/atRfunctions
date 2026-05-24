#' Label Data set
#'
#' @description
#' Attach value labels to single-select and multiple select questions. Works
#' with both Kobo and SurveyCTO XLSForms - the flavor is auto-detected by
#' default, and column-name differences (`relevant`/`relevance`,
#' `list_name`/`list name`, `name`/`value`) are normalized internally.
#'
#' @param data data set
#' @param tool the path to the data collection tool (XLSForm)
#' @param survey_label column name for the question labels in the `survey`
#'   sheet of the XLSForm. Default `"label::English"`.
#' @param choice_label column name for value labels in the `choices` sheet of
#'   the XLSForm. Default `"label::English"`.
#' @param multi_response_sep separator for the multiple select questions.
#'   Default `";"`.
#' @param tool_flavor one of `"auto"`, `"kobo"`, `"surveycto"`. See
#'   [read_xlsform()]. Default `"auto"`.
#' @param ... For backwards compatibility: the deprecated argument
#'   `choice_lable` (misspelling) is still accepted and mapped to
#'   `choice_label`.
#' @import readxl
#' @import dplyr
#' @import stringr
#' @export
labeler <- function(data, tool,
                    survey_label = "label::English",
                    choice_label = "label::English",
                    multi_response_sep = ";",
                    tool_flavor = "auto",
                    ...) {
  dots <- list(...)
  if ("choice_lable" %in% names(dots)) {
    warning("Argument `choice_lable` is deprecated; use `choice_label` instead.",
            call. = FALSE)
    if (missing(choice_label)) choice_label <- dots$choice_lable
  }

  xlsform <- read_xlsform(tool, flavor = tool_flavor)
  survey_questions <- xlsform$survey
  survey_choices   <- xlsform$choices

  survey_choices$name <- gsub("\\.0", "", survey_choices$name)
  survey_questions <- survey_questions[grepl("\\bselect_", survey_questions$type), ]
  survey_questions$select_type <- survey_questions$type %>% str_replace_all(" .*", "")
  survey_questions$type <- survey_questions$type %>%
    str_replace_all("select_one ", "") %>%
    str_replace_all("select_multiple ", "")
  survey_questions <- survey_questions %>%
    select(type, name, select_type, all_of(survey_label))

  survey_choices$name <- survey_choices$name %>% as.character
  survey_choices <- survey_choices[!is.na(survey_choices$list_name), ]

  for (var in names(data)) {
    if (var %in% survey_questions$name) {
      survey_choices_i <- survey_choices[survey_choices$list_name %in%
                                           survey_questions$type[survey_questions$name %in% var], ]
      add_underscore <- function() {
        index <- gregexpr("[0-9]", survey_choices_i[[choice_label]])
        regmatches(survey_choices_i[[choice_label]], index) <<-
          lapply(regmatches(survey_choices_i[[choice_label]], index),
                 function(x) paste0("_", x, "_"))
      }
      add_underscore()
      if (survey_questions$select_type[survey_questions$name %in% var] == "select_one") {
        for (choice_i in 1:nrow(survey_choices_i)) {
          data[[var]] <- data[[var]] %>%
            str_replace_all(paste0("\\b", survey_choices_i$name[choice_i], "\\b"),
                            survey_choices_i[[choice_label]][choice_i] %>% str_squish())
        }
        data[[var]] <- data[[var]] %>% str_replace_all("_", "")
      }
      else if (survey_questions$select_type[survey_questions$name %in% var] == "select_multiple") {
        data[[var]] <- data[[var]] %>%
          str_replace_all("  ", " ") %>%
          str_replace_all(" ", paste0(multi_response_sep))
        for (choice_i in 1:nrow(survey_choices_i)) {
          data[[var]] <- data[[var]] %>%
            str_replace_all(paste0("\\b", survey_choices_i$name[choice_i], "\\b"),
                            survey_choices_i[[choice_label]][choice_i] %>% str_squish())
        }
        data[[var]] <- data[[var]] %>% str_replace_all("_", "")
      }
    }
  }
  return(data)
}
