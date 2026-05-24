test_that("labeler accepts the deprecated `choice_lable` arg with a warning", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "kobo")
  data <- make_fixture_data()

  expect_warning(
    labeler(data, path,
            survey_label = "label:English",
            choice_lable = "label:English"),
    "deprecated"
  )
})

test_that("update_media_links accepts the deprecated `tool_path` arg with a warning", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "kobo")
  data <- make_fixture_data()

  expect_warning(
    update_media_links(data, tool_path = path),
    "deprecated"
  )
})

test_that("relevancy-pipeline functions accept the deprecated `kobo_tool` arg with a warning", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "kobo")
  survey <- read_xlsform(path)$survey

  expect_warning(add_repeat_sheet_names_to_questions(kobo_tool = survey), "deprecated")
  expect_warning(process_group_relevancies(kobo_tool = survey), "deprecated")
  expect_warning(convert_relevancy_to_R(kobo_tool = survey), "deprecated")
})
