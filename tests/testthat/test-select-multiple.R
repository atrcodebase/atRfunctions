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

test_that("build_sm_label_map produces a dataset_col -> labeled_col mapping", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "kobo")

  out <- build_sm_label_map(path, choice_label = "label:English")
  expect_true(all(c("Question", "response_code", "response_label",
                    "dataset_col", "labeled_col") %in% names(out)))
  expect_setequal(unique(out$Question), "fruits")
  expect_setequal(out$dataset_col, c("fruits_1", "fruits_2", "fruits_3"))
  expect_true(all(grepl("^fruits/", out$labeled_col)))
})

test_that("build_sm_label_map works for surveycto flavor with auto label-column detection", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "surveycto")

  out <- build_sm_label_map(path)
  expect_setequal(out$dataset_col, c("fruits_1", "fruits_2", "fruits_3"))
  expect_true(all(grepl("^fruits/", out$labeled_col)))
})

test_that("apply_sm_label_map renames columns using a pre-built mapping", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "kobo")
  data <- make_fixture_data()
  data$fruits_1 <- c(1, 0, 0); data$fruits_2 <- c(0, 0, 1); data$fruits_3 <- c(1, 0, 0)

  mapping <- build_sm_label_map(path, choice_label = "label:English")
  out <- apply_sm_label_map(data, mapping)

  expect_false("fruits_1" %in% names(out))
  expect_true("fruits/Apple"  %in% names(out))
  expect_true("fruits/Banana" %in% names(out))
  expect_true("fruits/Cherry" %in% names(out))
  expect_equal(out$`fruits/Apple`,  c(1, 0, 0))
  expect_equal(out$`fruits/Banana`, c(0, 0, 1))
  expect_equal(out$`fruits/Cherry`, c(1, 0, 0))
})

test_that("apply_sm_label_map also accepts an XLSForm directly (no map needed)", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "kobo")
  data <- make_fixture_data()
  data$fruits_1 <- c(1, 0, 0); data$fruits_2 <- c(0, 0, 1); data$fruits_3 <- c(1, 0, 0)

  out <- apply_sm_label_map(data, path, choice_label = "label:English")
  expect_true("fruits/Apple"  %in% names(out))
  expect_true("fruits/Banana" %in% names(out))
  expect_true("fruits/Cherry" %in% names(out))

  # Same result via read_xlsform() list
  xform <- read_xlsform(path)
  out2 <- apply_sm_label_map(data, xform, choice_label = "label:English")
  expect_equal(names(out), names(out2))
})

# --- sanitization: the new behavior the user asked for ---

test_that("build_sm_label_map sanitizes choice labels for valid R names", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  # A choices sheet whose labels contain characters that would break R names:
  # slash, apostrophe, parentheses, percent, leading digit, ampersand, whitespace.
  survey <- data.frame(
    type = "select_multiple weird", name = "q",
    relevant = NA_character_,
    `label::English` = "Pick all",
    stringsAsFactors = FALSE, check.names = FALSE
  )
  choices <- data.frame(
    list_name = rep("weird", 6),
    name      = c("1", "2", "3", "4", "5", "6"),
    `label::English` = c("Apple/Banana", "won't", "Mr. & Mrs.", "100%",
                          "Apple (red)", "  trailing  "),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  writexl::write_xlsx(list(survey = survey, choices = choices), path)

  mapping <- build_sm_label_map(path)

  # Every labeled_col must be reachable as a normal R name with no metachars
  # beyond the intentional `/` separator.
  fragments <- sub("^q/", "", mapping$labeled_col)
  expect_true(all(grepl("^[A-Za-z_][A-Za-z0-9_.]*$", fragments)),
              info = paste("got:", paste(fragments, collapse = ", ")))

  # Spot-checks of specific transformations
  expect_true("q/Apple_Banana"  %in% mapping$labeled_col)
  expect_true("q/wont"          %in% mapping$labeled_col)
  expect_true("q/Mr._Mrs"       %in% mapping$labeled_col)
  expect_true("q/x100"          %in% mapping$labeled_col)
  expect_true("q/Apple_red"     %in% mapping$labeled_col)
  expect_true("q/trailing"      %in% mapping$labeled_col)
})

test_that(".sanitize_r_name covers the corner cases", {
  s <- atRfunctions:::.sanitize_r_name
  expect_equal(s("Apple"),         "Apple")
  expect_equal(s("Apple Banana"),  "Apple_Banana")
  expect_equal(s("Apple/Banana"),  "Apple_Banana")
  expect_equal(s("won't"),         "wont")
  expect_equal(s("100"),           "x100")
  expect_equal(s("100%"),          "x100")
  expect_equal(s("  pad  "),       "pad")
  expect_equal(s(""),              "x")
  expect_equal(s(NA),              "x")
  expect_equal(s("Mr. Smith"),     "Mr._Smith")
  expect_equal(s("a___b"),         "a_b")
})

# --- deprecation aliases keep working ---

test_that("reshape_tool() works as a deprecated alias", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "kobo")
  expect_warning(out <- reshape_tool(path, choice_label = "label:English"),
                 "deprecated")
  expect_true("fruits/Apple" %in% out$labeled_col)
})

test_that("apply_SM_Label() works as a deprecated alias", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  write_fixture_xlsform(path, "kobo")
  data <- make_fixture_data()
  data$fruits_1 <- c(1, 0, 0); data$fruits_2 <- c(0, 0, 1); data$fruits_3 <- c(1, 0, 0)

  mapping <- build_sm_label_map(path, choice_label = "label:English")
  expect_warning(out <- apply_SM_Label(data, mapping), "deprecated")
  expect_true("fruits/Apple" %in% names(out))
})
