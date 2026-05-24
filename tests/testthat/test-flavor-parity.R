# The core promise: a Kobo fixture and a SurveyCTO fixture that describe the
# same logical form should produce identical outputs from downstream functions.

test_that("labeler() returns identical output for kobo and surveycto flavors", {
  skip_if_not_installed("writexl")
  kobo_path  <- tempfile(fileext = ".xlsx"); write_fixture_xlsform(kobo_path,  "kobo")
  scto_path  <- tempfile(fileext = ".xlsx"); write_fixture_xlsform(scto_path,  "surveycto")
  data       <- make_fixture_data()

  # Pass the label column name the fixture used for each flavor.
  out_kobo <- labeler(data, kobo_path,
                      survey_label = "label:English",
                      choice_label = "label:English")
  out_scto <- labeler(data, scto_path,
                      survey_label = "label::English",
                      choice_label = "label::English")

  expect_equal(out_kobo, out_scto)
})

test_that("concat_url() returns identical output for kobo and surveycto flavors", {
  skip_if_not_installed("writexl")
  kobo_path  <- tempfile(fileext = ".xlsx"); write_fixture_xlsform(kobo_path,  "kobo")
  scto_path  <- tempfile(fileext = ".xlsx"); write_fixture_xlsform(scto_path,  "surveycto")
  data       <- make_fixture_data()

  out_kobo <- concat_url(data, kobo_path, KEY = data$KEY)
  out_scto <- concat_url(data, scto_path, KEY = data$KEY)

  expect_equal(out_kobo, out_scto)
  # Sanity: the photo column got URLed for non-NA rows
  expect_true(grepl("^https://", out_kobo$photo[1]))
  expect_true(is.na(out_kobo$photo[2]))
})

test_that("update_media_links() returns identical output for both flavors", {
  skip_if_not_installed("writexl")
  kobo_path  <- tempfile(fileext = ".xlsx"); write_fixture_xlsform(kobo_path,  "kobo")
  scto_path  <- tempfile(fileext = ".xlsx"); write_fixture_xlsform(scto_path,  "surveycto")
  data       <- make_fixture_data()

  out_kobo <- update_media_links(data, kobo_path)
  out_scto <- update_media_links(data, scto_path)

  expect_equal(out_kobo, out_scto)
})
