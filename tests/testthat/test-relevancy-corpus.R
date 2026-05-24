# Corpus of XLSForm relevance expressions covering the failure classes
# identified in the audit. Each test exercises a specific bug class that the
# old converter mishandled.

make_survey <- function(types_and_names) {
  # types_and_names: named char vector, name = question name, value = type
  df <- data.frame(
    type = unname(types_and_names),
    name = names(types_and_names),
    relevant = NA_character_,
    stringsAsFactors = FALSE
  )
  df
}

# --- classification ---

test_that(".classify_relevance recognizes supported and unsupported", {
  expect_equal(atRfunctions:::.classify_relevance(""), "empty")
  expect_equal(atRfunctions:::.classify_relevance(NA), "empty")
  expect_equal(atRfunctions:::.classify_relevance("${q1} = 'a'"), "ok")
  expect_equal(atRfunctions:::.classify_relevance("selected(${q1}, '1')"), "ok")
  expect_equal(atRfunctions:::.classify_relevance("not(selected(${q1}, '1'))"), "ok")
  expect_equal(atRfunctions:::.classify_relevance("count-selected(${q}) > 2"),
               "unsupported_function")
  expect_equal(atRfunctions:::.classify_relevance("string-length(${q}) > 3"),
               "unsupported_function")
  expect_equal(atRfunctions:::.classify_relevance("if(${a}='yes', 1, 0) = 1"),
               "unsupported_function")
  expect_equal(atRfunctions:::.classify_relevance("regex(${q}, '^a.+')"),
               "unsupported_function")
  expect_equal(atRfunctions:::.classify_relevance("coalesce(${a}, ${b}) = 'x'"),
               "unsupported_function")
  expect_equal(atRfunctions:::.classify_relevance("pulldata('csv', 'col', 'k', ${id})"),
               "unsupported_function")
  expect_equal(atRfunctions:::.classify_relevance("${a} div 2 > 1"),
               "unsupported_operator")
})

# --- the and/or precedence bug ---

test_that("and/or expressions retain precedence without paren mangling", {
  survey <- make_survey(c(a = "text", b = "text", c = "text", d = "text"))
  survey$relevant[1] <- "${a}='1' and ${b}='2' and ${c}='3' or ${d}='4'"
  out <- convert_relevancy_to_R(survey)[1]
  # Must be parseable
  expect_silent(parse(text = out))
  # Semantically equivalent: should evaluate the same as
  # (a=='1' & b=='2' & c=='3') | (d=='4')
  data <- data.frame(a = "1", b = "2", c = "3", d = "9", stringsAsFactors = FALSE)
  expect_true(eval(parse(text = out)))
  data <- data.frame(a = "9", b = "9", c = "9", d = "4", stringsAsFactors = FALSE)
  expect_true(eval(parse(text = out)))
  data <- data.frame(a = "1", b = "2", c = "9", d = "9", stringsAsFactors = FALSE)
  expect_false(eval(parse(text = out)))
})

# --- regex metacharacter escaping ---

test_that("selected() with regex-meta choice value matches literally only", {
  survey <- make_survey(c(size = "select_multiple sizes"))
  survey$relevant[1] <- "selected(${size}, '1.5')"
  out <- convert_relevancy_to_R(survey)[1]
  expect_silent(parse(text = out))
  # The generated grepl pattern must match "1.5" but NOT "1X5"
  data <- data.frame(size = "1.5", stringsAsFactors = FALSE)
  expect_true(eval(parse(text = out)))
  data <- data.frame(size = "1X5", stringsAsFactors = FALSE)
  expect_false(eval(parse(text = out)))
})

test_that("selected() with parenthesis in choice value parses and matches literally", {
  survey <- make_survey(c(opt = "select_multiple opts"))
  survey$relevant[1] <- "selected(${opt}, 'a+b')"
  out <- convert_relevancy_to_R(survey)[1]
  expect_silent(parse(text = out))
  # Literal "a+b" matches, but "aaab" (regex one-or-more) must not
  data <- data.frame(opt = "a+b", stringsAsFactors = FALSE)
  expect_true(eval(parse(text = out)))
  data <- data.frame(opt = "aaab", stringsAsFactors = FALSE)
  expect_false(eval(parse(text = out)))
})

# --- apostrophe in choice value ---

test_that("selected() with apostrophe in choice value produces parseable R", {
  survey <- make_survey(c(opinion = "select_one o"))
  survey$relevant[1] <- "selected(${opinion}, \"won't\")"
  out <- convert_relevancy_to_R(survey)[1]
  expect_silent(parse(text = out))
  data <- data.frame(opinion = "won't", stringsAsFactors = FALSE)
  expect_true(eval(parse(text = out)))
})

# --- unsupported functions cleanly skipped ---

test_that("unsupported functions yield NA, not a broken expression", {
  survey <- make_survey(c(m = "select_multiple m"))
  survey$relevant[1] <- "count-selected(${m}) > 2"
  out <- convert_relevancy_to_R(survey)[1]
  expect_true(is.na(out))
})

# --- type-aware comparisons ---

test_that("character columns are compared as character (no type-coercion bug)", {
  survey <- make_survey(c(count = "text"))
  survey$relevant[1] <- "${count} = 5"
  out <- convert_relevancy_to_R(survey)[1]
  expect_silent(parse(text = out))
  # data$count is character "5"; the comparison must say TRUE
  data <- data.frame(count = c("5", "10"), stringsAsFactors = FALSE)
  result <- eval(parse(text = out))
  expect_equal(result, c(TRUE, FALSE))
})

test_that("integer columns are compared as numeric (no string wrapping)", {
  survey <- make_survey(c(age = "integer"))
  survey$relevant[1] <- "${age} > 17"
  out <- convert_relevancy_to_R(survey)[1]
  expect_silent(parse(text = out))
  # Should NOT wrap age in as.character()
  expect_false(grepl("as.character(data\\$age)", out))
})

# --- end-to-end: create_relevancy_file + check_relevancy_rules ---

test_that("create_relevancy_file emits convert_status column and skips unsupported", {
  skip_if_not_installed("writexl")
  path <- tempfile(fileext = ".xlsx")
  survey <- data.frame(
    type     = c("text", "select_one yesno", "select_multiple opts", "integer", "text", "text"),
    name     = c("name", "consent", "opts",                  "age",     "alt",  "weird"),
    relevant = c(NA,     NA,                "${consent} = 'yes'",        "${consent} = 'yes'", NA,
                 "count-selected(${opts}) > 1"),
    stringsAsFactors = FALSE
  )
  survey$`label::English` <- c("name", "consent", "opts", "age", "alt", "weird")
  choices <- data.frame(
    list_name = c("yesno", "yesno", "opts", "opts"),
    name      = c("yes",   "no",    "1",    "2"),
    `label::English` = c("Yes", "No", "Opt A", "Opt B"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  writexl::write_xlsx(list(survey = survey, choices = choices), path)

  s <- read_xlsform(path)$survey
  suppressMessages(rf <- create_relevancy_file(s))
  expect_true("convert_status" %in% names(rf))
  # The count-selected rule must be flagged as unsupported with NA Rcondition
  unsupp <- rf[rf$name == "weird", ]
  expect_equal(unsupp$convert_status, "unsupported_function")
  expect_true(is.na(unsupp$Rcondition))
})

test_that("check_relevancy_rules tolerates eval errors without aborting", {
  # Build a relevancy file by hand with one good rule and one bad Rcondition.
  rf <- data.frame(
    type = c("text", "text"),
    name = c("a", "b"),
    relevance_rule    = c("${x} = 'yes'", "bad-rule"),
    relevant_question = c("x", "x"),
    relevant_value    = c("yes", ""),
    Rcondition        = c("as.character(data$x) == \"yes\"",
                          "this is not valid R code !!!"),
    Remarks           = c("", ""),
    check_reverse     = c("TRUE", "TRUE"),
    convert_status    = c("ok", "ok"),
    convert_error     = c(NA, NA),
    sheet             = c("data", "data"),
    stringsAsFactors = FALSE
  )
  data <- data.frame(KEY = c("u1", "u2"),
                     x = c("yes", "no"),
                     a = c("yes-answer", NA),
                     b = c(NA, NA),
                     stringsAsFactors = FALSE)
  suppressMessages(log <- check_relevancy_rules(data, rf, sheet_name = "data", KEY = "KEY"))
  skipped <- attr(log, "skipped")
  expect_true(nrow(skipped) >= 1)
  expect_true(any(grepl("eval_error", skipped$reason)))
  # The good rule still got evaluated even though the bad one crashed
  expect_true(any(skipped$name == "b"))
})

test_that("check_relevancy_rules attaches a 'skipped' attribute summarizing skips", {
  rf <- data.frame(
    type = c("text", "text"),
    name = c("a", "b"),
    relevance_rule    = c("${x} = 'yes'", "count-selected(${m}) > 2"),
    relevant_question = c("x", "m"),
    relevant_value    = c("yes", ""),
    Rcondition        = c("as.character(data$x) == \"yes\"", NA_character_),
    Remarks           = c("", ""),
    check_reverse     = c("TRUE", "TRUE"),
    convert_status    = c("ok", "unsupported_function"),
    convert_error     = c(NA, NA),
    sheet             = c("data", "data"),
    stringsAsFactors = FALSE
  )
  data <- data.frame(KEY = c("u1"), x = "yes", a = "value", b = NA,
                     stringsAsFactors = FALSE)
  suppressMessages(log <- check_relevancy_rules(data, rf, sheet_name = "data"))
  skipped <- attr(log, "skipped")
  expect_true(any(skipped$name == "b"))
  expect_true(any(grepl("unsupported", skipped$reason)))
})
