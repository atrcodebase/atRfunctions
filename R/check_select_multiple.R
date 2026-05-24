#' Check select_multiple series columns
#'
#' Verifies that the `select_multiple` series columns (0/1 dummies) are
#' consistent with the responses in the main multi-response column.
#'
#' @param data A data frame.
#' @param tool path to the XLSForm, a [read_xlsform()] result list, or a
#'   pre-read `survey`-sheet data frame.
#' @param question_separator Separator between question name and choice code.
#'   Default `"_"`.
#' @param KEY Column uniquely identifying rows. Default `"KEY"`.
#' @param excluded_col Vector of column names to skip. Default `""`.
#' @param tool_flavor One of `"auto"`, `"kobo"`, `"surveycto"`. Only used when
#'   `tool` is a file path. See [read_xlsform()]. Default `"auto"`.
#'
#' @return Data frame of inconsistency log rows.
#' @import dplyr
#' @import stringr
#' @export
check_select_multiple <- function(data, tool, question_separator = "_",
                                  KEY = "KEY", excluded_col = "",
                                  tool_flavor = "auto") {
  survey <- .resolve_tool(tool, tool_flavor = tool_flavor)$survey

  sm_cols <- survey$name[grepl("select_multiple", survey$type) &
                           survey$name %in% names(data)]
  sm_cols <- sm_cols[sm_cols %notin% excluded_col]

  series_log <- data.frame(KEY = NA, question = NA, value = NA,
                           series_columns = NA, series_values = NA,
                           Remarks = NA)

  for (question in sm_cols) {
    series_cols <- names(data)[grepl(
      paste0("^", question, question_separator, "[0-9]{1,5}$"),
      names(data)
    )]
    data_sub <- data %>%
      select(all_of(question), all_of(series_cols), all_of(KEY)) %>%
      filter(!is.na(get(question)))

    if (nrow(data_sub) != 0) {
      for (i in seq_len(nrow(data_sub))) {
        val <- str_split(data_sub[[question]][i], " |-")[[1]]
        series_columns <- paste0(question, question_separator, val)
        other_columns <- names(data_sub)[names(data_sub) %notin%
                                           c(series_columns, question, "KEY")]
        if (!all(series_columns %in% names(data_sub))) {
          log <- c(data_sub$KEY[i], question, data_sub[[question]][i],
                   paste0(series_columns, collapse = " - "),
                   "", "Series column not in data")
          series_log <- rbind(series_log, log)
        } else if (any(data_sub[i, series_columns] %in% c(NA, 0))) {
          log <- c(data_sub$KEY[i], question, data_sub[[question]][i],
                   paste0(series_columns, collapse = " - "),
                   paste0(data_sub[i, series_columns], collapse = " - "),
                   "Inconsistent series columns")
          series_log <- rbind(series_log, log)
        } else if (any(data_sub[i, other_columns] %in% 1)) {
          other_cols <- other_columns[which(data_sub[i, other_columns] %in% 1)]
          log <- c(data_sub$KEY[i], question, data_sub[[question]][i],
                   paste0(other_cols, collapse = " - "),
                   paste0(data_sub[i, other_cols], collapse = " - "),
                   "At least one response is not in the tool choices")
          series_log <- rbind(series_log, log)
        }
      }
    }
  }
  if (nrow(series_log) == 1) {
    message("No mismatches found: ", deparse(substitute(data)))
    return(series_log[-1, ])
  }
  series_log[-1, ]
}
