# Relevancy pipeline: create_relevancy_file -> check_relevancy_rules.
# The fixture form has one rule: photo and fruits are only relevant when
# consent == "yes". Our test data has consent == "no" in row 2 with NA for
# fruits/photo - which is consistent (no violation). We mutate it to make a
# violation and check the log captures it.

test_that("create_relevancy_file works on a normalized survey from either flavor", {
  skip_if_not_installed("writexl")
  kobo_path <- tempfile(fileext = ".xlsx"); write_fixture_xlsform(kobo_path, "kobo")
  scto_path <- tempfile(fileext = ".xlsx"); write_fixture_xlsform(scto_path, "surveycto")

  kobo_survey <- read_xlsform(kobo_path)$survey
  scto_survey <- read_xlsform(scto_path)$survey

  rf_kobo <- create_relevancy_file(kobo_survey)
  rf_scto <- create_relevancy_file(scto_survey)

  # Same logical rules from both flavors
  expect_equal(rf_kobo$name, rf_scto$name)
  expect_equal(rf_kobo$Rcondition, rf_scto$Rcondition)
  expect_true(all(c("name", "relevance_rule", "Rcondition", "check_reverse", "sheet") %in% names(rf_kobo)))
})

test_that("check_relevancy_rules flags a clear violation", {
  skip_if_not_installed("writexl")
  kobo_path <- tempfile(fileext = ".xlsx"); write_fixture_xlsform(kobo_path, "kobo")

  survey <- read_xlsform(kobo_path)$survey
  rf <- create_relevancy_file(survey)

  data <- make_fixture_data()
  # Inject a violation: consent="no" but fruits has a value -> should be flagged.
  data$consent[2] <- "no"
  data$fruits[2]  <- "1"

  log <- check_relevancy_rules(data, rf, sheet_name = "data", KEY = "KEY")

  expect_true(nrow(log) >= 1)
  expect_true("fruits" %in% log$question)
})

test_that("create_relevancy_file accepts the deprecated kobo_tool argument with a warning", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx"); write_fixture_xlsform(path, "kobo")
  survey <- read_xlsform(path)$survey

  expect_warning(
    rf <- create_relevancy_file(kobo_tool = survey),
    "deprecated"
  )
  expect_true(nrow(rf) >= 1)
})
