test_that("apply_log applies cleaning log to a character column", {
  data <- data.frame(KEY = c("a", "b", "c"), name = c("Ali", "Sara", "Omid"),
                     stringsAsFactors = FALSE)
  log <- data.frame(KEY = "b", question = "name",
                    old_value = "Sara", new_value = "Sarah",
                    stringsAsFactors = FALSE)

  out <- apply_log(data, log)
  expect_equal(out$name[2], "Sarah")
  expect_equal(out$name[c(1, 3)], c("Ali", "Omid"))
})

test_that("apply_log applies a cleaning log to a numeric column", {
  data <- data.frame(KEY = c("a", "b"), age = c(30, 25))
  log <- data.frame(KEY = "a", question = "age",
                    old_value = "30", new_value = "31",
                    stringsAsFactors = FALSE)

  out <- apply_log(data, log)
  expect_equal(out$age, c(31, 25))
})

test_that("compare_dt returns a long-format diff", {
  v1 <- data.frame(KEY = c("a", "b"), x = c(1, 2), y = c("p", "q"),
                   stringsAsFactors = FALSE)
  v2 <- v1
  v2$x[1] <- 99

  diff <- compare_dt(v1, v2, unique_id_df1 = "KEY", unique_id_df2 = "KEY",
                     compare_all = TRUE)
  expect_true(nrow(diff) == 1)
  expect_equal(diff$question, "x")
  expect_equal(diff$old_value, "1")
  expect_equal(diff$new_value, "99")
})

test_that("update_series_cols recomputes 0/1 dummies from a select_multiple", {
  data <- data.frame(KEY = c("a", "b"),
                     fruits   = c("1 3", "2"),
                     fruits_1 = c(0, 0),
                     fruits_2 = c(0, 0),
                     fruits_3 = c(0, 0),
                     stringsAsFactors = FALSE)
  survey <- data.frame(type = "select_multiple fruits", name = "fruits",
                       stringsAsFactors = FALSE)

  out <- update_series_cols(data, survey, question_separator = "_")
  expect_equal(out$fruits_1, c(1, 0))
  expect_equal(out$fruits_2, c(0, 1))
  expect_equal(out$fruits_3, c(1, 0))
})

test_that("reshape_to_datamerge pivots and formats correctly", {
  ar <- data.frame(
    Disaggregation       = c("province", "province"),
    Disaggregation_level = c("Kabul", "Kabul"),
    Question             = c("consent", "age"),
    Response             = c("yes", NA),
    Aggregation_method   = c("perc", "mean"),
    Result               = c("75", "32"),
    stringsAsFactors = FALSE
  )

  out <- reshape_to_datamerge(ar)
  expect_equal(nrow(out), 1)
  # perc -> "%" suffix; mean -> "mean of " prefix
  expect_true(any(grepl("75%", as.character(unlist(out)))))
  expect_true(any(grepl("mean of age", names(out))))
})

test_that("missing_translation returns a data frame (regression for missing-return bug)", {
  data <- data.frame(KEY = c("a", "b"),
                     q1  = c("normal text", "text ’ with curly quote"),
                     stringsAsFactors = FALSE)
  out <- missing_translation(data, KEY = "KEY", excluded_cols = "KEY")
  expect_s3_class(out, "data.frame")
})
