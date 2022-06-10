#' Label SurveyCTO Dataset
#'
#' @description
#' Attach value labels to single-select and multi-select questions
#' @usage
#' library(atRfunctions)
#' labeler(data, tool,
#'         survey_label = "label:English",
#'         choice_lable = "label:English",
#'         multi_response_sep = ";"
#'         )
#'
#' @param data data set
#' @param tool the path to the SurveyCTO data collection tool
#' @param survey_label column name for the question labels in 'survey' sheet of the SurveyCTO data collection tool. The default value is 'label:English'
#' @param choice_lable column name for value labels in 'choices' sheet of the SurveyCTO data collection tool. The default value is 'label:English'
#' @param multi_response_sep separator for the multi-select questions. The default value is ';'
#' @import readxl
#' @import dplyr
#' @import stringr
#' @export
labeler <- function(data, tool, survey_label = "label:English", choice_lable = "label:English", multi_response_sep = ";"){

  survey_questions <- read_excel(tool, "survey")
  survey_choices <- read_excel(tool,"choices")

  if("value" %in% names(survey_choices)){
    names(survey_choices)[names(survey_choices) == "value"] <- "name"
  }

  if("list name" %in% names(survey_choices)){
    names(survey_choices)[names(survey_choices) == "list name"] <- "list_name"
  }

  survey_choices$name <- gsub("\\.0", "", survey_choices$name)

  # Prep Survey Questions
  survey_questions <- survey_questions[grepl("\\bselect_", survey_questions$type),]
  survey_questions$select_type <- survey_questions$type %>% str_replace_all(" .*","")
  survey_questions$type <- survey_questions$type %>% str_replace_all("select_one ", "") %>% str_replace_all("select_multiple ", "")
  survey_questions <- survey_questions %>%
    select(type, name, select_type, all_of(survey_label))

  # Prep Choices
  survey_choices$name <- survey_choices$name %>% as.character
  survey_choices <- survey_choices[!is.na(survey_choices$list_name),]

  for(var in names(data)){
    if(var %in% survey_questions$name){
      # Grab choices for that variable
      survey_choices_i <- survey_choices[survey_choices$list_name %in% survey_questions$type[survey_questions$name %in% var],]

      # add underscore before and after numeric values in {choice_lable}
      add_underscore <- function() {
        index <- gregexpr("[0-9]", survey_choices_i[[choice_lable]])
        regmatches(survey_choices_i[[choice_lable]], index) <<- lapply(regmatches(survey_choices_i[[choice_lable]], index), function(x) paste0("_",x,"_"))
      }
      add_underscore()

      # select one
      if(survey_questions$select_type[survey_questions$name %in% var] == "select_one"){

        for(choice_i in 1:nrow(survey_choices_i)){
          data[[var]] <- data[[var]] %>%
            str_replace_all(paste0("\\b", survey_choices_i$name[choice_i], "\\b"), survey_choices_i[[choice_lable]][choice_i])
        }
        data[[var]] <- data[[var]] %>% str_replace_all("_", "")

        # select multiple
      } else if(survey_questions$select_type[survey_questions$name %in% var] == "select_multiple"){

        data[[var]] <- data[[var]] %>%
          str_replace_all("  ", " ") %>%
          str_replace_all(" ", paste0(multi_response_sep, " "))

        for(choice_i in 1:nrow(survey_choices_i)){
          data[[var]] <- data[[var]] %>%
            str_replace_all(paste0("\\b", survey_choices_i$name[choice_i], "\\b"), survey_choices_i[[choice_lable]][choice_i])
        }

        data[[var]] <- data[[var]] %>% str_replace_all("_", "")

      }
      # data[[var]] <- data[[var]] %>% str_replace_all("_", "")

    }
  }

  return(data)

}
