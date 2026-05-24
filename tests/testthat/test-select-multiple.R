test_that("check_select_multiple accepts a file path and runs end-to-end", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "kobo")
  data <- make_fixture_data()
  # Add matching series columns so check_select_multiple has something to verify
  data$fruits_1 <- c(1, 0, 0)
  data$fruits_2 <- c(0, 0, 1)
  data$fruits_3 <- c(1, 0, 0)

  log <- check_select_multiple(data, path, question_separator = "_",
                               KEY = "KEY")
  # The consistent rows produce no log entries; we expect zero
  expect_s3_class(log, "data.frame")
  expect_equal(nrow(log), 0)
})

test_that("check_select_multiple still accepts a pre-read data frame (back-compat)", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "kobo")
  survey <- read_xlsform(path)$survey
  data <- make_fixture_data()
  data$fruits_1 <- c(1, 0, 0); data$fruits_2 <- c(0, 0, 1); data$fruits_3 <- c(1, 0, 0)

  log <- check_select_multiple(data, survey, question_separator = "_")
  expect_s3_class(log, "data.frame")
})

test_that("check_select_multiple flags an inconsistent row", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "kobo")
  data <- make_fixture_data()
  # Row 1: fruits = "1 3" but only fruits_1 = 1; fruits_3 = 0 -> inconsistency
  data$fruits_1 <- c(1, 0, 0)
  data$fruits_2 <- c(0, 0, 1)
  data$fruits_3 <- c(0, 0, 0)

  log <- check_select_multiple(data, path, question_separator = "_")
  expect_true(nrow(log) >= 1)
  expect_true("fruits" %in% log$question)
})

test_that("reshape_tool produces a dataset_col -> labeled_col mapping", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "kobo")

  out <- reshape_tool(path, choice_label = "label:English")
  expect_true(all(c("Question", "response_code", "response_label",
                    "dataset_col", "labeled_col") %in% names(out)))
  # Only the select_multiple question (fruits) should appear
  expect_setequal(unique(out$Question), "fruits")
  expect_setequal(out$dataset_col, c("fruits_1", "fruits_2", "fruits_3"))
  expect_true(all(grepl("^fruits/", out$labeled_col)))
})

test_that("reshape_tool works for surveycto flavor with auto label-column detection", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "surveycto")

  out <- reshape_tool(path) # No choice_label -> auto-pick
  expect_setequal(out$dataset_col, c("fruits_1", "fruits_2", "fruits_3"))
  expect_true(all(grepl("^fruits/", out$labeled_col)))
})

test_that("apply_SM_Label renames the dataset columns according to reshape_tool", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "kobo")
  data <- make_fixture_data()
  data$fruits_1 <- c(1, 0, 0)
  data$fruits_2 <- c(0, 0, 1)
  data$fruits_3 <- c(1, 0, 0)

  mapping <- reshape_tool(path, choice_label = "label:English")
  out <- apply_SM_Label(data, mapping)

  expect_false("fruits_1" %in% names(out))
  expect_true("fruits/Apple"  %in% names(out))
  expect_true("fruits/Banana" %in% names(out))
  expect_true("fruits/Cherry" %in% names(out))
  # Values are preserved
  expect_equal(out$`fruits/Apple`,  c(1, 0, 0))
  expect_equal(out$`fruits/Banana`, c(0, 0, 1))
  expect_equal(out$`fruits/Cherry`, c(1, 0, 0))
})
