# Stage helpers. Edit freely.

# Null-coalescing operator (used in _targets.R too).
`%||%` <- function(a, b) if (is.null(a)) b else a

# Drop interviews whose KEY appears in the rejection log.
filter_rejected <- function(data, rejection_log, key = "KEY") {
  if (is.null(rejection_log) || !nrow(rejection_log)) return(data)
  if (!key %in% names(rejection_log)) {
    warning(sprintf("Rejection log has no `%s` column; returning data unfiltered.",
                    key), call. = FALSE)
    return(data)
  }
  rejected <- rejection_log[[key]]
  data[!data[[key]] %in% rejected, , drop = FALSE]
}

# Drop a vector of columns; silent on names that aren't in `data`.
drop_cols <- function(data, cols) {
  if (is.null(cols) || length(cols) == 0) return(data)
  data[, !names(data) %in% cols, drop = FALSE]
}

# Build the client-facing version: analyst data minus PII, with column names
# replaced by their survey / custom labels.
build_client_version <- function(analyst, xform, columns) {
  d <- drop_cols(analyst, columns$pii_columns)
  apply_column_labels(d, xform, custom_labels = columns$custom_labels)
}

# Write a data frame to CSV and return the path - the shape `targets` needs
# for file targets. dir.create() is recursive so we don't error on first run.
write_csv_target <- function(data, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(data, path)
  path
}

# Write a named list of issue data frames to a multi-sheet xlsx. Empty
# frames are skipped; if nothing has any rows, write a single sheet noting
# that no issues were found.
write_issues_xlsx <- function(issues_list, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  keep <- vapply(issues_list,
                 function(x) is.data.frame(x) && nrow(x) > 0,
                 logical(1))
  payload <- if (any(keep)) {
    issues_list[keep]
  } else {
    list(empty = data.frame(note = "No issues found"))
  }
  if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(payload, path)
  } else {
    openxlsx::write.xlsx(payload, path)
  }
  path
}
