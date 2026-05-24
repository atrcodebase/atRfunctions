# Read a cleaning / QA / translation log by short name.
#
# The function looks up two env vars:
#   <env_prefix><NAME>_URL  e.g. ATRP_QA_LOG_URL
#   <env_prefix><NAME>_GID  e.g. ATRP_QA_LOG_GID    (optional)
# and dispatches based on the URL shape:
#
#   - "publish-to-web" CSV URLs (".../pub?output=csv" or any URL containing
#     "output=csv") -> readr::read_csv()  (no auth needed)
#   - Full Google Sheets URLs (".../spreadsheets/d/<ID>/...") ->
#     googlesheets4::read_sheet()  (one-time gs4_auth() required)
#   - Anything else -> treated as a local CSV path or generic CSV URL.
#
# If the URL env var is unset or empty, returns an empty data frame and
# emits a warning - so downstream stages keep running even when a log isn't
# configured yet.
read_log <- function(name, env_prefix = "ATRP_") {
  upper   <- toupper(name)
  url_var <- paste0(env_prefix, upper, "_URL")
  gid_var <- paste0(env_prefix, upper, "_GID")

  url <- Sys.getenv(url_var, NA_character_)
  gid <- Sys.getenv(gid_var, NA_character_)

  if (is.na(url) || url == "") {
    warning(sprintf("Env var %s not set; returning empty log for '%s'.",
                    url_var, name), call. = FALSE)
    return(data.frame())
  }

  is_pub  <- grepl("/pub", url) || grepl("output=csv", url, fixed = TRUE)
  is_full <- grepl("/spreadsheets/d/", url, fixed = TRUE)

  if (is_pub) {
    sep <- if (grepl("?", url, fixed = TRUE)) "&" else "?"
    url_csv <- if (!is.na(gid) && gid != "") paste0(url, sep, "gid=", gid) else url
    return(readr::read_csv(url_csv, show_col_types = FALSE))
  }

  if (is_full) {
    if (!requireNamespace("googlesheets4", quietly = TRUE)) {
      stop("Install `googlesheets4` to read private Google Sheets, or ",
           "switch the URL to a Publish-to-Web CSV.", call. = FALSE)
    }
    if (!is.na(gid) && gid != "") {
      props <- googlesheets4::sheet_properties(url)
      sheet <- props$name[match(as.integer(gid), props$id)]
      if (length(sheet) == 0 || is.na(sheet)) {
        stop(sprintf("Sheet with GID %s not found at %s.", gid, url), call. = FALSE)
      }
      return(googlesheets4::read_sheet(url, sheet = sheet))
    }
    return(googlesheets4::read_sheet(url))
  }

  # Fallback: treat as a local file path or generic CSV URL.
  readr::read_csv(url, show_col_types = FALSE)
}
