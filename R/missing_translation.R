#' Generate translation log
#'
#' @usage
#' library(atRfunctions)
#' missing_translation(data, KEY)
#'
#' @param data dataframe
#' @param KEY unique identifier. Default value is "KEY"
#' @import tidyr
#' @import dplyr
#' @export
missing_translation <- function(data, KEY = "KEY") {
  data %>%
    mutate(across(everything(), function(x)
      x = as.character(x)
    )) %>%
    pivot_longer(-all_of(KEY), names_to = "question_name", values_to = "old_value") %>%
    filter(Encoding(old_value) %in% "UTF-8") %>%
    mutate(new_value = NA, changed = "No", DM_comment = "Missing translation") %>%
    relocate(KEY, .before = DM_comment)
}
