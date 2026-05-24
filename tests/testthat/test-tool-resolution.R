# Every function that takes an XLSForm-shaped `tool` argument should accept
# the three canonical forms interchangeably:
#   (a) a file path
#   (b) a read_xlsform() result list
#   (c) a pre-read `survey` data frame (when the function only needs the
#       survey sheet)
#
# These tests exercise form (b) for each function and confirm it produces the
# same output as form (a). Form (c) is already covered by other test files.

skip_if_no_writexl <- function() skip_if_not_installed("writexl")

setup_xlsform <- function(flavor = "kobo") {
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, flavor)
  path
}

test_that("labeler accepts a read_xlsform() result list", {
  skip_if_no_writexl()
  path <- setup_xlsform("kobo")
  xform <- read_xlsform(path)
  data <- make_fixture_data()
  via_path <- labeler(data, path,
                      survey_label = "label:English",
                      choice_label = "label:English")
  via_list <- labeler(data, xform,
                      survey_label = "label:English",
                      choice_label = "label:English")
  expect_equal(via_path, via_list)
})

test_that("concat_url accepts a read_xlsform() result list", {
  skip_if_no_writexl()
  path <- setup_xlsform("kobo")
  xform <- read_xlsform(path)
  data <- make_fixture_data()
  via_path <- concat_url(data, path, KEY = data$KEY)
  via_list <- concat_url(data, xform, KEY = data$KEY)
  expect_equal(via_path, via_list)
})

test_that("update_media_links accepts a read_xlsform() result list", {
  skip_if_no_writexl()
  path <- setup_xlsform("kobo")
  xform <- read_xlsform(path)
  data <- make_fixture_data()
  via_path <- update_media_links(data, path)
  via_list <- update_media_links(data, xform)
  expect_equal(via_path, via_list)
})

test_that("add_repeat_sheet_names_to_questions accepts list, df, and path", {
  skip_if_no_writexl()
  path <- setup_xlsform("kobo")
  xform <- read_xlsform(path)
  via_path <- add_repeat_sheet_names_to_questions(path)
  via_list <- add_repeat_sheet_names_to_questions(xform)
  via_df   <- add_repeat_sheet_names_to_questions(xform$survey)
  expect_equal(via_path, via_list)
  expect_equal(via_path, via_df)
})

test_that("process_group_relevancies accepts list, df, and path", {
  skip_if_no_writexl()
  path <- setup_xlsform("kobo")
  xform <- read_xlsform(path)
  via_path <- process_group_relevancies(path)
  via_list <- process_group_relevancies(xform)
  via_df   <- process_group_relevancies(xform$survey)
  expect_equal(via_path$relevant, via_list$relevant)
  expect_equal(via_path$relevant, via_df$relevant)
})

test_that("convert_relevancy_to_R accepts list, df, and path", {
  skip_if_no_writexl()
  path <- setup_xlsform("kobo")
  xform <- read_xlsform(path)
  via_path <- convert_relevancy_to_R(path)
  via_list <- convert_relevancy_to_R(xform)
  via_df   <- convert_relevancy_to_R(xform$survey)
  expect_equal(via_path, via_list)
  expect_equal(via_path, via_df)
})

test_that("create_relevancy_file accepts list, df, and path", {
  skip_if_no_writexl()
  path <- setup_xlsform("kobo")
  xform <- read_xlsform(path)
  suppressMessages({
    via_path <- create_relevancy_file(path)
    via_list <- create_relevancy_file(xform)
    via_df   <- create_relevancy_file(xform$survey)
  })
  expect_equal(via_path$Rcondition,   via_list$Rcondition)
  expect_equal(via_path$Rcondition,   via_df$Rcondition)
  expect_equal(via_path$convert_status, via_list$convert_status)
})

test_that("update_series_cols accepts list, df, and path", {
  skip_if_no_writexl()
  path <- setup_xlsform("kobo")
  xform <- read_xlsform(path)
  data <- make_fixture_data()
  data$fruits_1 <- c(0, 0, 0); data$fruits_2 <- c(0, 0, 0); data$fruits_3 <- c(0, 0, 0)
  via_path <- update_series_cols(data, path, question_separator = "_")
  via_list <- update_series_cols(data, xform, question_separator = "_")
  via_df   <- update_series_cols(data, xform$survey, question_separator = "_")
  expect_equal(via_path, via_list)
  expect_equal(via_path, via_df)
})

test_that("check_select_multiple accepts list, df, and path", {
  skip_if_no_writexl()
  path <- setup_xlsform("kobo")
  xform <- read_xlsform(path)
  data <- make_fixture_data()
  data$fruits_1 <- c(1, 0, 0); data$fruits_2 <- c(0, 0, 1); data$fruits_3 <- c(1, 0, 0)
  via_path <- check_select_multiple(data, path, "_", "KEY")
  via_list <- check_select_multiple(data, xform, "_", "KEY")
  via_df   <- check_select_multiple(data, xform$survey, "_", "KEY")
  expect_equal(nrow(via_path), nrow(via_list))
  expect_equal(nrow(via_path), nrow(via_df))
})

test_that("reshape_tool accepts list (already supported) and path", {
  skip_if_no_writexl()
  path <- setup_xlsform("kobo")
  xform <- read_xlsform(path)
  via_path <- reshape_tool(path, choice_label = "label:English")
  via_list <- reshape_tool(xform, choice_label = "label:English")
  expect_equal(via_path, via_list)
})

test_that(".resolve_tool errors clearly when a function needs choices but got a survey df", {
  survey <- data.frame(type = "text", name = "q",
                       relevant = NA_character_, stringsAsFactors = FALSE)
  expect_error(
    atRfunctions:::.resolve_tool(survey, needs_choices = TRUE),
    "needs both the survey and choices sheets"
  )
})

test_that(".resolve_tool errors on an unrecognized tool shape", {
  expect_error(atRfunctions:::.resolve_tool(42),
               "Unrecognized .tool.")
  expect_error(atRfunctions:::.resolve_tool(list(foo = 1)),
               "Unrecognized .tool.")
})
