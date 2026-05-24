#' Internal package utilities (not exported)
#'
#' @keywords internal
#' @name atRfunctions-internal
#' @importFrom stats setNames
#' @importFrom utils head
NULL

# Used by check_relevancy_rules, check_select_multiple, missing_translation.
`%notin%` <- Negate(`%in%`)

# Soft-deprecation helper. Looks up `old_name` in `dots`; if present, warns and
# returns its value when `new_value` was not supplied. Designed to be called as:
#
#   foo <- function(x = NULL, ...) {
#     x <- .deprecated_arg(x, list(...), new_name = "x", old_name = "old_x")
#   }
.deprecated_arg <- function(new_value, dots, new_name, old_name) {
  if (old_name %in% names(dots)) {
    warning(sprintf("Argument `%s` is deprecated; use `%s` instead.",
                    old_name, new_name), call. = FALSE)
    if (is.null(new_value)) return(dots[[old_name]])
  }
  new_value
}

# Detect XLSForm flavor from a survey/choices data frame's column names.
# Returns "kobo", "surveycto", or "unknown".
.detect_tool_flavor <- function(survey = NULL, choices = NULL) {
  if (!is.null(survey) && "relevance" %in% names(survey)) return("surveycto")
  if (!is.null(survey) && "relevant" %in% names(survey))  return("kobo")
  if (!is.null(choices) && "list name" %in% names(choices)) return("surveycto")
  if (!is.null(choices) && "list_name" %in% names(choices)) return("kobo")
  if (!is.null(choices) && "value" %in% names(choices))     return("surveycto")
  "unknown"
}

# Normalize a survey-sheet data frame so downstream code can rely on:
#   - column `relevant` (renamed from `relevance` if needed)
.normalize_survey <- function(survey) {
  if (!"relevant" %in% names(survey) && "relevance" %in% names(survey)) {
    names(survey)[names(survey) == "relevance"] <- "relevant"
  }
  survey
}

# Normalize a choices-sheet data frame so downstream code can rely on:
#   - column `list_name` (renamed from `list name`)
#   - column `name`      (renamed from `value`)
.normalize_choices <- function(choices) {
  if ("list name" %in% names(choices)) {
    names(choices)[names(choices) == "list name"] <- "list_name"
  }
  if (!"name" %in% names(choices) && "value" %in% names(choices)) {
    names(choices)[names(choices) == "value"] <- "name"
  }
  choices
}

# Resolve a label column name. Accepts user input like "English",
# "label::English", or "label:English" and returns whichever form actually
# exists in `available_cols`. Falls back to the original if no match.
.resolve_label_col <- function(label, available_cols) {
  # Already an existing column name? use it.
  if (label %in% available_cols) return(label)
  # Bare language, e.g. "English" -> try both prefixed forms.
  candidates <- c(label,
                  paste0("label::", label),
                  paste0("label:",  label),
                  sub("^label::", "label:", label),
                  sub("^label:",  "label::", label))
  hit <- candidates[candidates %in% available_cols]
  if (length(hit) >= 1) return(hit[1])
  label
}
