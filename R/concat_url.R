#' Concatenate the image/audio url
#'
#' The `concat_url()` function concatenates the url for image and audio
#' questions. Works with both Kobo and SurveyCTO XLSForms.
#'
#' @param data the dataset/dataframe
#' @param tool path to the XLSForm, a [read_xlsform()] result list, or a
#'   pre-read `survey`-sheet data frame.
#' @param server_name the server name
#' @param KEY the unique identifier column name - UUID
#' @param question_types a string vector of question types which the url should
#'   be concatenated. Default values are: `c("audio audit", "text audit",
#'   "audio", "image")`.
#' @param tool_flavor one of `"auto"`, `"kobo"`, `"surveycto"`. See
#'   [read_xlsform()]. Default `"auto"`.
#'
#' @import readxl
#' @import dplyr
#' @export
concat_url <- function(data, tool,
                       server_name = "https://atrconsultingaf.surveycto.com",
                       KEY = KEY,
                       question_types = c("audio audit", "text audit", "audio", "image"),
                       tool_flavor = "auto") {
  xlsform <- .resolve_tool(tool, tool_flavor = tool_flavor)
  survey_questions <- xlsform$survey %>%
    filter(type %in% question_types) %>%
    pull(name)

  data %>%
    mutate(across(any_of(survey_questions),
                  function(x)
                    x = case_when(
                      !is.na(x) ~ paste0(server_name, "/view/submission-attachment/",
                                          gsub("File skipped from exports: ", "", x),
                                          "?uuid=uuid%3A",
                                          gsub("uuid:", "", substr(KEY, 1, 41))),
                      TRUE ~ NA_character_
                    )
    ))
}
