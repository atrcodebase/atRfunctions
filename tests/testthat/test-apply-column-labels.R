test_that("apply_column_labels renames non-SM columns via survey labels", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "kobo")
  data <- make_fixture_data()
  data$fruits_1 <- c(1, 0, 0); data$fruits_2 <- c(0, 0, 1); data$fruits_3 <- c(1, 0, 0)

  out <- apply_column_labels(data, path,
                             survey_label = "label:English",
                             choice_label = "label:English")
  # Fixture survey labels: name->"Your name", consent->"Do you consent?",
  # fruits->"Pick fruits", photo->"Take a photo", age->"Your age".
  expect_true("Your_name"        %in% names(out))
  expect_true("Do_you_consent"   %in% names(out))
  expect_true("Pick_fruits"      %in% names(out))
  expect_true("Take_a_photo"     %in% names(out))
  expect_true("Your_age"         %in% names(out))
  # KEY isn't in the survey - leave it alone
  expect_true("KEY" %in% names(out))
})

test_that("apply_column_labels renames SM series cols using labeled question prefix", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "kobo")
  data <- make_fixture_data()
  data$fruits_1 <- c(1, 0, 0); data$fruits_2 <- c(0, 0, 1); data$fruits_3 <- c(1, 0, 0)

  out <- apply_column_labels(data, path,
                             survey_label = "label:English",
                             choice_label = "label:English")
  # The series cols inherit the labeled parent name as prefix.
  expect_true("Pick_fruits.Apple"  %in% names(out))
  expect_true("Pick_fruits.Banana" %in% names(out))
  expect_true("Pick_fruits.Cherry" %in% names(out))
  # The bare numeric series cols are gone
  expect_false("fruits_1" %in% names(out))
  expect_false("fruits_2" %in% names(out))
  expect_false("fruits_3" %in% names(out))
})

test_that("apply_column_labels honors a custom sm_label_separator", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "kobo")
  data <- make_fixture_data()
  data$fruits_1 <- c(1, 0, 0); data$fruits_2 <- c(0, 0, 1); data$fruits_3 <- c(1, 0, 0)

  out <- apply_column_labels(data, path,
                             survey_label = "label:English",
                             choice_label = "label:English",
                             sm_label_separator = "__")
  expect_true("Pick_fruits__Apple"  %in% names(out))
})

test_that("apply_column_labels honors excluded_cols", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "kobo")
  data <- make_fixture_data()
  out <- apply_column_labels(data, path,
                             survey_label = "label:English",
                             choice_label = "label:English",
                             excluded_cols = "consent")
  # Excluded -> original name preserved
  expect_true("consent" %in% names(out))
  # Other columns still relabeled
  expect_true("Your_name" %in% names(out))
})

# --- custom_labels in all three accepted shapes ---

test_that("apply_column_labels accepts custom_labels as a named character vector", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "kobo")
  data <- make_fixture_data()
  data$age_bracket <- c("18-24", "25-34", "35+")
  data$is_eligible <- c(TRUE, FALSE, TRUE)

  out <- apply_column_labels(
    data, path,
    survey_label = "label:English",
    choice_label = "label:English",
    custom_labels = c(age_bracket = "Age bracket",
                      is_eligible = "Is eligible?")
  )
  expect_true("Age_bracket"  %in% names(out))
  expect_true("Is_eligible"  %in% names(out))
  expect_false("age_bracket" %in% names(out))
  expect_false("is_eligible" %in% names(out))
})

test_that("apply_column_labels accepts custom_labels as a data frame", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "kobo")
  data <- make_fixture_data()
  data$age_bracket <- c("18-24", "25-34", "35+")

  cl <- data.frame(name = "age_bracket", label = "Age bracket",
                   stringsAsFactors = FALSE)
  out <- apply_column_labels(data, path,
                             survey_label = "label:English",
                             choice_label = "label:English",
                             custom_labels = cl)
  expect_true("Age_bracket" %in% names(out))
})

test_that("apply_column_labels accepts custom_labels as a named list", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "kobo")
  data <- make_fixture_data()
  data$age_bracket <- c("18-24", "25-34", "35+")

  out <- apply_column_labels(data, path,
                             survey_label = "label:English",
                             choice_label = "label:English",
                             custom_labels = list(age_bracket = "Age bracket"))
  expect_true("Age_bracket" %in% names(out))
})

test_that("custom_labels overrides survey label for the same column", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "kobo")
  data <- make_fixture_data()
  # `age` IS in the survey (labeled "Your age"). Override via custom_labels.
  out <- apply_column_labels(data, path,
                             survey_label = "label:English",
                             choice_label = "label:English",
                             custom_labels = c(age = "Age in years (override)"))
  expect_true("Age_in_years_override" %in% names(out))
  expect_false("Your_age" %in% names(out))
})

test_that("apply_column_labels errors on malformed custom_labels", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "kobo")
  data <- make_fixture_data()
  expect_error(
    apply_column_labels(data, path, survey_label = "label:English",
                        custom_labels = c("unlabeled", "vector")),
    "must be named"
  )
  expect_error(
    apply_column_labels(data, path, survey_label = "label:English",
                        custom_labels = data.frame(x = 1, y = 2)),
    "must have columns"
  )
})

# --- build_sm_label_map separator parameter ---

test_that("build_sm_label_map honors sm_label_separator", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "kobo")
  out_default <- build_sm_label_map(path, choice_label = "label:English")
  out_under   <- build_sm_label_map(path, choice_label = "label:English",
                                    sm_label_separator = "_")
  out_slash   <- build_sm_label_map(path, choice_label = "label:English",
                                    sm_label_separator = "/")
  expect_true(all(grepl("^fruits\\.", out_default$labeled_col)))
  expect_true(all(grepl("^fruits_",   out_under$labeled_col)))
  expect_true(all(grepl("^fruits/",   out_slash$labeled_col)))
})

test_that("apply_sm_label_map honors sm_label_separator when building internally", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "kobo")
  data <- make_fixture_data()
  data$fruits_1 <- c(1, 0, 0); data$fruits_2 <- c(0, 0, 1); data$fruits_3 <- c(1, 0, 0)
  out <- apply_sm_label_map(data, path,
                            choice_label = "label:English",
                            sm_label_separator = "__")
  expect_true("fruits__Apple" %in% names(out))
})
