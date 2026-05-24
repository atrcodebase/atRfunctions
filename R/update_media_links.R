#' Update Media Links
#'
#' Adds the proper URL for datasets downloaded from SCTO Desktop. Works with
#' both Kobo and SurveyCTO XLSForms.
#'
#' @param data dataframe
#' @param tool path to the XLSForm
#' @param download_link default value is
#'   `'https://artftpm.surveycto.com/view/submission-attachment/'`
#' @param key_col default value is `'KEY'`
#' @param rename adds the link in a new column if `TRUE`
#' @param tool_flavor one of `"auto"`, `"kobo"`, `"surveycto"`. See
#'   [read_xlsform()]. Default `"auto"`.
#' @param ... For backwards compatibility: the previous argument `tool_path`
#'   is still accepted as a deprecated alias for `tool`.
#'
#' @import readxl
#' @import dplyr
#' @import stringr
#' @export
update_media_links <- function(data, tool = NULL,
                               download_link = "https://artftpm.surveycto.com/view/submission-attachment/",
                               key_col = "KEY",
                               rename = FALSE,
                               tool_flavor = "auto",
                               ...) {
  tool <- .deprecated_arg(tool, list(...), new_name = "tool", old_name = "tool_path")
  if (is.null(tool)) stop("`tool` is required.", call. = FALSE)

  # Data types with download link
  link_types <- c("image", "audio", "audio audit", "text audit")
  common_file_types <- ".csv$|.m4a$|.amr$|.wav$|.aac$|.mp3$|.jpg$|.ogg$"

  xlsform <- read_xlsform(tool, flavor = tool_flavor)
  link_cols <- xlsform$survey %>%
    filter(type %in% link_types & name %in% names(data)) %>%
    pull(name)

  # Loop through each column and recode
  for (col_i in link_cols) {
    # Filter NA values and anything that does not have a proper file extension (value might be changed in the log)
    filtered_index <- which(!is.na(data[[col_i]]) &
                              !grepl(download_link, data[[col_i]]) &
                              grepl(common_file_types, data[[col_i]]))
    new_keys <- str_replace(data[[key_col]][filtered_index], "uuid:", "?uuid=uuid%3A")

    ## Replace with cto link
    data[[col_i]] <- str_remove(data[[col_i]], "File skipped from exports: ")
    data[[col_i]][filtered_index] <- paste0(download_link, data[[col_i]][filtered_index])
    data[[col_i]][filtered_index] <- paste0(data[[col_i]][filtered_index], new_keys) %>% str_squish()

    ## Rename the new column if asked
    if (rename) {
      ncol_i <- paste0("n", col_i) # New name
      names(data)[names(data) == col_i] <- ncol_i
    }
  }
  return(data)
}
