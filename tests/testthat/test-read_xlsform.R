test_that("read_xlsform auto-detects kobo flavor and normalizes columns", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, flavor = "kobo")

  res <- read_xlsform(path)

  expect_equal(res$flavor, "kobo")
  expect_true("relevant"  %in% names(res$survey))
  expect_true("list_name" %in% names(res$choices))
  expect_true("name"      %in% names(res$choices))
})

test_that("read_xlsform auto-detects surveycto flavor and normalizes columns", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, flavor = "surveycto")

  res <- read_xlsform(path)

  expect_equal(res$flavor, "surveycto")
  # After normalization the canonical names are the same regardless of input flavor
  expect_true("relevant"  %in% names(res$survey))
  expect_true("list_name" %in% names(res$choices))
  expect_true("name"      %in% names(res$choices))
  expect_false("relevance" %in% names(res$survey))
  expect_false("list name" %in% names(res$choices))
  expect_false("value"     %in% names(res$choices))
})

test_that("read_xlsform honors an explicit flavor override", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, flavor = "kobo")
  res <- read_xlsform(path, flavor = "surveycto")
  # Even though the user said surveycto, normalization should still succeed
  expect_equal(res$flavor, "surveycto")
  expect_true("relevant" %in% names(res$survey))
})
