#' Reshape an XLSForm into a select_multiple column-name mapping
#'
#' Reads an XLSForm and returns a data frame that maps each raw dataset column
#' (e.g. `q1_3`) to its labeled equivalent (e.g. `q1/Banana`). Useful for
#' renaming series columns of `select_multiple` questions to human-readable
#' labels. Works with both Kobo and SurveyCTO XLSForms.
#'
#' @param tool Path to the XLSForm, or a pre-built result from [read_xlsform()].
#' @param excluded_cols Vector of question names to skip.
#' @param choice_label Column name in the `choices` sheet that holds the choice
#'   label. If `NULL` (the default), the function tries the typical XLSForm
#'   names (`label::English`, `label:English`, `label`) and picks the first
#'   one that exists.
#' @param tool_flavor One of `"auto"`, `"kobo"`, `"surveycto"`. Only used when
#'   `tool` is a file path. See [read_xlsform()]. Default `"auto"`.
#'
#' @return Data frame with columns `Question`, `type`, `select_type`,
#'   `response_code`, `response_label`, `response_label_new`, `dataset_col`,
#'   `labeled_col`.
#' @import dplyr
#' @import stringr
#' @export
reshape_tool <- function(tool, excluded_cols = "", choice_label = NULL,
                         tool_flavor = "auto") {
  xlsform <- if (is.list(tool) && all(c("survey", "choices") %in% names(tool))) {
    tool
  } else {
    read_xlsform(tool, flavor = tool_flavor)
  }
  tool_survey  <- xlsform$survey
  tool_choices <- xlsform$choices

  label_col <- choice_label %||% .pick_label_col(tool_choices)
  if (!label_col %in% names(tool_choices)) {
    stop(sprintf("Label column '%s' not found in choices sheet.", label_col),
         call. = FALSE)
  }

  tool_survey <- tool_survey %>%
    select(type, Question = name) %>%
    filter(grepl("\\bselect_multiple", type) & Question %notin% excluded_cols) %>%
    mutate(
      select_type = str_replace_all(type, " .*", ""),
      type = str_replace_all(type, "select_one ", "") %>%
        str_replace_all("select_multiple ", "")
    )

  tool_choices <- tool_choices %>%
    filter(!is.na(list_name)) %>%
    select(list_name, response_code = name,
           response_label = all_of(label_col)) %>%
    mutate(
      response_label = str_squish(response_label),
      response_label_new = str_replace_all(response_label, " ", "_"),
      # Non-numeric choice codes (e.g. "yes"/"no") become NA and are dropped
      # by the filter below; suppress the expected NA-coercion warning.
      response_code = suppressWarnings(as.numeric(response_code))
    )

  tool_survey %>%
    left_join(tool_choices, by = c("type" = "list_name"),
              relationship = "many-to-many") %>%
    filter(!is.na(response_code) & !is.na(response_label)) %>%
    mutate(
      dataset_col = paste0(Question, "_", response_code),
      labeled_col = paste0(Question, "/", response_label_new)
    )
}

# Pick a sensible default label column from those present in the choices sheet.
.pick_label_col <- function(choices) {
  candidates <- c("label::English", "label:English", "label::english",
                  "label:english", "label")
  hit <- candidates[candidates %in% names(choices)]
  if (length(hit) >= 1) return(hit[1])
  # Fall back to first column starting with "label"
  label_like <- grep("^label", names(choices), value = TRUE)
  if (length(label_like) >= 1) return(label_like[1])
  stop("No label column found in choices sheet. Pass `choice_label` explicitly.",
       call. = FALSE)
}

# Local null-coalescing operator used by reshape_tool.
`%||%` <- function(a, b) if (is.null(a)) b else a
