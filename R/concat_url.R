#' Concatenate the image/audio url
#'
#' The `concat_url()` function concatenates the url for image and audio questions
#'
#' @param data the dataset/dataframe
#' @param tool link to SCTO tool
#' @param server_name the server name
#' @param KEY the unique identifier column name - UUID
#' @param question_types a string vector of question types which the url should be concatenate. Defualt value c("audio audit", "text audit", "audio", "image")

#' @import readxl
#' @import dplyr
#' @export
concat_url <- function(data, tool, server_name = "https://atrconsultingaf.surveycto.com", KEY = KEY, question_types = c("audio audit", "text audit", "audio", "image")) {
  survey_questions <- read_excel(tool, "survey")
  survey_questions <- survey_questions %>%
    filter(type %in% question_types) %>%
    pull(name)

  data %>%
    mutate(across(any_of(survey_questions),
                  function(x)
                    x = case_when(
                      !is.na(x) ~ paste0(server_name, "/view/submission-attachment/", gsub("File skipped from exports: ", "", x), "?uuid=uuid%3A", gsub("uuid:", "", substr(KEY, 1, 41))),
                      TRUE ~ NA_character_
                    )
    ))
}
