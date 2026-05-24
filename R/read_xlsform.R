#' Read an XLSForm (Kobo or SurveyCTO) into a normalized list
#'
#' Reads the `survey` and `choices` sheets of an XLSForm and normalizes the
#' column names so downstream code does not have to care whether the tool was
#' designed in Kobo or SurveyCTO. Specifically: `relevance` is renamed to
#' `relevant`, `list name` to `list_name`, and `value` to `name`.
#'
#' @param tool Path to the XLSForm `.xlsx` file.
#' @param flavor One of `"auto"`, `"kobo"`, or `"surveycto"`. When `"auto"` (the
#'   default) the flavor is detected from the column names actually present in
#'   the file.
#' @param guess_max Passed to [readxl::read_excel()]. Defaults to `100000`.
#'
#' @return A list with elements `survey` (data frame, normalized), `choices`
#'   (data frame, normalized), and `flavor` (the detected or supplied flavor).
#'
#' @import readxl
#' @export
read_xlsform <- function(tool, flavor = c("auto", "kobo", "surveycto"), guess_max = 100000) {
  flavor <- match.arg(flavor)

  survey  <- read_excel(tool, "survey",  guess_max = guess_max)
  choices <- read_excel(tool, "choices", guess_max = guess_max)

  if (flavor == "auto") flavor <- .detect_tool_flavor(survey, choices)

  survey  <- .normalize_survey(survey)
  choices <- .normalize_choices(choices)

  list(survey = survey, choices = choices, flavor = flavor)
}
