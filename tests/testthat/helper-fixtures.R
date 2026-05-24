# Build small XLSForm fixtures on demand for tests.
#
# We build the same logical form twice - once with Kobo column names and once
# with SurveyCTO column names - so that the same downstream call (e.g.
# `labeler(data, tool)`) should yield identical output for both flavors.

make_survey_df <- function(flavor = c("kobo", "surveycto"), label_col = NULL) {
  flavor <- match.arg(flavor)
  rel_col <- if (flavor == "kobo") "relevant" else "relevance"
  label_col <- label_col %||% if (flavor == "kobo") "label:English" else "label::English"

  df <- data.frame(
    type = c("text", "select_one yesno", "select_multiple fruits", "image", "integer"),
    name = c("name", "consent", "fruits", "photo", "age"),
    stringsAsFactors = FALSE
  )
  df[[label_col]] <- c("Your name", "Do you consent?", "Pick fruits", "Take a photo", "Your age")
  df[[rel_col]] <- c(NA, NA, "${consent} = 'yes'", "${consent} = 'yes'", NA)
  df
}

make_choices_df <- function(flavor = c("kobo", "surveycto"), label_col = NULL) {
  flavor <- match.arg(flavor)
  label_col <- label_col %||% if (flavor == "kobo") "label:English" else "label::English"

  if (flavor == "kobo") {
    df <- data.frame(
      list_name = c("yesno", "yesno", "fruits", "fruits", "fruits"),
      name      = c("yes", "no", "1", "2", "3"),
      stringsAsFactors = FALSE
    )
  } else {
    df <- data.frame(
      `list name` = c("yesno", "yesno", "fruits", "fruits", "fruits"),
      value       = c("yes", "no", "1", "2", "3"),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }
  df[[label_col]] <- c("Yes", "No", "Apple", "Banana", "Cherry")
  df
}

`%||%` <- function(a, b) if (is.null(a)) b else a

write_fixture_xlsform <- function(path, flavor = c("kobo", "surveycto")) {
  flavor <- match.arg(flavor)
  if (!requireNamespace("writexl", quietly = TRUE)) {
    skip("writexl not installed; skipping XLSForm fixture tests")
  }
  writexl::write_xlsx(
    list(survey = make_survey_df(flavor), choices = make_choices_df(flavor)),
    path = path
  )
  path
}

# Tiny dataset that matches the fixture form. Choice values are intentionally
# the raw "name" column values (yes/no/1/2/3) as they would appear after
# SurveyCTO/Kobo export.
make_fixture_data <- function() {
  data.frame(
    KEY     = c("uuid:aaa", "uuid:bbb", "uuid:ccc"),
    name    = c("Ali", "Sara", NA),
    consent = c("yes", "no", "yes"),
    fruits  = c("1 3", NA, "2"),
    photo   = c("photo1.jpg", NA, "photo3.jpg"),
    age     = c(30, 25, 40),
    stringsAsFactors = FALSE
  )
}
