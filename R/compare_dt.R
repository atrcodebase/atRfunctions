#' Compare data frames
#'
#' The `compare_dt()` function compares two versions of the same data
#'
#' @param df1 Old version of the dataset
#' @param df2 Latest version of the dataset
#' @param unique_id_df1 unique identifier in df1
#' @param unique_id_df2 unique identifier in df2
#' @examples
#' library(atRfunctions)
#'
#' mtcars_v1 <- mtcars_v2 <- mtcars
#' mtcars_v1$id <- mtcars_v2$id <- rownames(mtcars)
#'
#' mtcars_v2$am[mtcars_v2$am == 1] <- 2
#' mtcars_v2$wt[mtcars_v2$wt > 100] <- 105
#' mtcars_v2$cyl[mtcars_v2$cyl == 8] <- 7
#' mtcars_v2 <- mtcars_v2[1:30, ]
#'
#' compare_dt(df1 = mtcars_v1, df2 = mtcars_v2,
#'           unique_id_df1 = "id",
#'           unique_id_df2 = "id"
#' )
#' @usage
#' library(atRfunctions)
#' compare_dt(df1, df2, unique_id_df1, unique_id_df2)

#' @import tidyr
#' @import dplyr
#' @import stringr
#' @export
compare_dt <- function(df1, df2, unique_id_df1, unique_id_df2) {

  if ("KEY" %in% colnames(df1) & unique_id_df1 != "KEY") {
    df1 <- df1 %>%
      rename(key = KEY)
  }

  df1 <- df1 %>%
    select(KEY = all_of(unique_id_df1), everything()) %>%
    mutate(across(-KEY, function(x)
      x = as.character(x)
    )) %>%
    pivot_longer(-KEY, values_to = "value_1") %>%
    mutate(value_1 = str_squish(value_1))

  if ("KEY" %in% colnames(df2) & unique_id_df2 != "KEY") {
    df2 <- df2 %>%
      rename(key = KEY)
  }

  df2 <- df2 %>%
    select(KEY = all_of(unique_id_df2), everything()) %>%
    mutate(across(-KEY, function(x)
      x = as.character(x)
    )) %>%
    pivot_longer(-KEY, values_to = "value_2") %>%
    mutate(value_2 = str_squish(value_2))

  df_both <- full_join(df1, df2, by = c("KEY", "name"))

  diff <- df_both %>%
    filter((value_1 != value_2) | (is.na(value_1) & !is.na(value_2)) | (!is.na(value_1) & is.na(value_2))) %>%
    rename(column_name = name, value_in_df1 = value_1, value_in_df2 = value_2) %>%
    mutate(column_name = ifelse(column_name == "key", "KEY", column_name))

  if(nrow(diff) == 0) {
    paste0("No difference in df1 and df2")
  } else {
    return(diff)
  }
}
