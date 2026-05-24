# Custom logic checks for this project.
#
# `data` is the cleaned + labeled dataset (after labeler() + update_series_cols()).
# Return a data frame with at least the columns:
#   KEY        - unique row identifier
#   question   - column name(s) the issue concerns
#   issue      - short human-readable description
#   value      - (optional) flagged value, as character
#
# The pipeline writes this frame as one sheet in the consolidated issues
# workbook under output/issues/.
custom_checks <- function(data) {
  issues <- data.frame(
    KEY      = character(),
    question = character(),
    issue    = character(),
    value    = character(),
    stringsAsFactors = FALSE
  )

  # ---- Example: flag adult-only questions answered by minors. -------------
  # if (all(c("age", "income") %in% names(data))) {
  #   bad <- !is.na(data$age) & data$age < 18 & !is.na(data$income)
  #   if (any(bad)) {
  #     issues <- rbind(issues, data.frame(
  #       KEY      = data$KEY[bad],
  #       question = "income",
  #       issue    = "Income reported by respondent under 18",
  #       value    = as.character(data$income[bad]),
  #       stringsAsFactors = FALSE
  #     ))
  #   }
  # }

  issues
}
