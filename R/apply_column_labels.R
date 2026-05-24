#' Rename data columns to use survey labels (all question types)
#'
#' Walks the XLSForm and renames every column in `data` whose name matches a
#' question in the `survey` sheet to a sanitized version of the question's
#' label. For `select_multiple` questions the parent column is renamed via
#' the survey label and the series columns are renamed via the choice labels
#' (e.g. `fruits_1` -> `Pick_fruits.Apple`). Columns whose name is not in
#' the XLSForm can be renamed via the optional `custom_labels` argument -
#' useful for calculated / derived variables added during analysis.
#'
#' Priority when more than one source could rename a given column:
#' `custom_labels` > select_multiple choice labels > survey label.
#'
#' @param data A data frame.
#' @param tool Path to the XLSForm, a [read_xlsform()] result list, or a
#'   pre-read `survey`-sheet data frame. Note: applying choice labels to
#'   `select_multiple` series columns requires a full XLSForm (path or
#'   list); a bare survey data frame is enough for everything else.
#' @param survey_label Column name in the `survey` sheet that holds the
#'   question label. If `NULL` (default), the function picks
#'   `label::English`, `label:English`, or `label` - whichever exists.
#' @param choice_label Column name in the `choices` sheet that holds the
#'   choice label. Same auto-pick rules as `survey_label`. Only used when
#'   the XLSForm has a choices sheet.
#' @param sm_label_separator Separator placed between the (labeled) question
#'   prefix and the sanitized choice label for `select_multiple` series
#'   columns. Default `"."`.
#' @param custom_labels Optional. Names + labels for columns that don't
#'   appear in the XLSForm (typically calculated or derived variables).
#'   Accepts:
#'   \itemize{
#'     \item A named character vector: `c(my_calc = "My calculation")`.
#'     \item A named list with character entries.
#'     \item A data frame with columns `name` and `label`.
#'   }
#'   Labels are sanitized the same way as survey labels.
#' @param excluded_cols Columns to leave untouched.
#' @param tool_flavor One of `"auto"`, `"kobo"`, `"surveycto"`. Only used
#'   when `tool` is a file path. Default `"auto"`.
#'
#' @return The data frame with renamed columns.
#' @import dplyr
#' @import stringr
#' @export
apply_column_labels <- function(data, tool,
                                survey_label = NULL,
                                choice_label = NULL,
                                sm_label_separator = ".",
                                custom_labels = NULL,
                                excluded_cols = "",
                                tool_flavor = "auto") {
  xlsform <- .resolve_tool(tool, tool_flavor = tool_flavor)
  survey  <- xlsform$survey
  choices <- xlsform$choices

  # 1. Resolve which column in `survey` carries the human-readable label.
  if (is.null(survey_label)) survey_label <- .pick_label_col(survey)
  if (!survey_label %in% names(survey)) {
    stop(sprintf("Survey label column '%s' not found in survey sheet.",
                 survey_label), call. = FALSE)
  }

  # 2. Build the survey-side rename map: name -> sanitized label.
  survey_map <- data.frame(
    name  = as.character(survey$name),
    label = as.character(survey[[survey_label]]),
    stringsAsFactors = FALSE
  )
  survey_map <- survey_map[!is.na(survey_map$name)  & nzchar(survey_map$name)  &
                            !is.na(survey_map$label) & nzchar(survey_map$label), ,
                          drop = FALSE]
  survey_map$new_name <- .sanitize_r_name(survey_map$label)

  # 3. Build the select_multiple map (parent + series cols). Only possible
  #    when we have a choices sheet. We rewrite labeled_col to use the
  #    labeled question name as prefix, so series cols stay paired with
  #    their renamed parent.
  sm_map <- NULL
  if (!is.null(choices) && nrow(choices) > 0) {
    sm_map <- tryCatch(
      build_sm_label_map(xlsform, excluded_cols = excluded_cols,
                         choice_label = choice_label,
                         sm_label_separator = sm_label_separator,
                         tool_flavor = tool_flavor),
      error = function(e) NULL
    )
    if (!is.null(sm_map) && nrow(sm_map) > 0) {
      sm_map$question_label_new <- survey_map$new_name[
        match(sm_map$Question, survey_map$name)
      ]
      sm_map$labeled_col <- ifelse(
        is.na(sm_map$question_label_new),
        sm_map$labeled_col,
        paste0(sm_map$question_label_new, sm_label_separator,
               sm_map$response_label_new)
      )
    }
  }

  # 4. Normalize custom_labels into a flat data frame and sanitize.
  custom_map <- .normalize_custom_labels(custom_labels)
  if (nrow(custom_map) > 0) custom_map$new_name <- .sanitize_r_name(custom_map$label)

  # 5. Apply renames. Priority: custom > sm > survey. We compute the new
  #    name for each column once based on its *current* (pre-rename) name.
  orig <- names(data)
  new  <- orig
  for (i in seq_along(orig)) {
    col <- orig[i]
    if (col %in% excluded_cols) next

    if (nrow(custom_map) > 0 && col %in% custom_map$name) {
      new[i] <- custom_map$new_name[match(col, custom_map$name)]
    } else if (!is.null(sm_map) && nrow(sm_map) > 0 && col %in% sm_map$dataset_col) {
      new[i] <- sm_map$labeled_col[match(col, sm_map$dataset_col)]
    } else if (col %in% survey_map$name) {
      new[i] <- survey_map$new_name[match(col, survey_map$name)]
    }
  }
  names(data) <- new
  data
}

# Coerce one of (NULL | named char vec | named list | data frame) into a
# canonical 2-column data frame (name, label). Errors clearly on bad shapes.
.normalize_custom_labels <- function(x) {
  if (is.null(x)) {
    return(data.frame(name = character(), label = character(),
                      stringsAsFactors = FALSE))
  }
  if (is.character(x)) {
    if (is.null(names(x))) {
      stop("`custom_labels` character vector must be named ",
           "(e.g. c(my_var = 'My variable')).", call. = FALSE)
    }
    return(data.frame(name = names(x), label = unname(as.character(x)),
                      stringsAsFactors = FALSE))
  }
  if (is.data.frame(x)) {
    if (!all(c("name", "label") %in% names(x))) {
      stop("`custom_labels` data frame must have columns `name` and `label`.",
           call. = FALSE)
    }
    return(data.frame(name = as.character(x$name),
                      label = as.character(x$label),
                      stringsAsFactors = FALSE))
  }
  if (is.list(x)) {
    if (length(x) == 0) {
      return(data.frame(name = character(), label = character(),
                        stringsAsFactors = FALSE))
    }
    if (is.null(names(x))) {
      stop("`custom_labels` list must be named.", call. = FALSE)
    }
    return(data.frame(name = names(x),
                      label = unlist(lapply(x, as.character)),
                      stringsAsFactors = FALSE))
  }
  stop("`custom_labels` must be NULL, a named character vector, a named ",
       "list, or a data frame with `name`/`label` columns.", call. = FALSE)
}
