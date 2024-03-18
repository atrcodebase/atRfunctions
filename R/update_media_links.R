#' Update Media Links
#'
#' This function adds the proper URL for datasets downloaded from SCTO Desktop.
#'
#' @param data dataframe
#' @param tool_path The path of the data collection tool
#' @param download_link default value is 'https://artftpm.surveycto.com/view/submission-attachment/'
#' @param key_col default value is 'KEY'
#' @param rename adds the link in a new column if True
#'
#' @usage
#' library(atRfunctions)
#' update_media_links(data,
#'                    tool_path,
#'                    download_link="https://artftpm.surveycto.com/view/submission-attachment/",
#'                    key_col="KEY",
#'                    rename=FALSE)
#'
#' @import dplyr
#' @export
update_media_links <- function(data, tool_path, download_link="https://artftpm.surveycto.com/view/submission-attachment/", key_col="KEY", rename=FALSE){
  # Data types with download link
  link_types <- c("image", "audio", "audio audit", "text audit")
  # common_file_types <- c(".csv", ".m4a", ".amr", ".wav", ".aac", ".mp3", ".jpg")
  common_file_types <- ".csv$|.m4a$|.amr$|.wav$|.aac$|.mp3$|.jpg$|.ogg$"
  # Read & Filter tool
  tool <- read_excel(tool_path, "survey", guess_max = 100000)
  link_cols <- tool %>% filter(type %in% link_types & name %in% names(data)) %>% pull(name)

  # Loop through each column and recode
  for(col_i in link_cols){
    # Filter NA values and anything that does not have a proper file extension (value might be changed in the log)
    filtered_index <- which(!is.na(data[[col_i]]) & !grepl(download_link, data[[col_i]]) & grepl(common_file_types, data[[col_i]]))
    new_keys <- str_replace(data[[key_col]][filtered_index], "uuid:", "?uuid=uuid%3A")

    ## Replace with cto link
    data[[col_i]] <- str_remove(data[[col_i]], "File skipped from exports: ") # Not using str_replace because some may only have the file name

    data[[col_i]][filtered_index] <- paste0(download_link, data[[col_i]][filtered_index])
    data[[col_i]][filtered_index] <- paste0(data[[col_i]][filtered_index], new_keys) %>% str_squish()

    ## Rename the new column if asked
    if(rename){
      ncol_i <- paste0("n", col_i) # New name
      names(data)[names(data) == col_i] <- ncol_i
    }
  }
  return(data)
}
