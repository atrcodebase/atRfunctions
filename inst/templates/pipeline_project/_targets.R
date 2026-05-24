# Pipeline definition for {{project_name}}.
#
# Run interactively with:
#   targets::tar_make()
#
# Or visualize the dependency graph with:
#   targets::tar_visnetwork()
#
# Configuration lives in `config/project.yml` (tools, logs, toggles) and
# `config/columns.yml` (drop / pii / custom_labels). Sensitive log URLs go
# in `.Renviron` (copy `.Renviron.example` first).

library(targets)
library(tarchetypes)
library(atRfunctions)

# Source every R/ helper into the targets workspace.
tar_source("R")

project <- yaml::read_yaml("config/project.yml")
columns <- yaml::read_yaml("config/columns.yml")

# Log targets always re-fetch from Google Sheets when always_refetch_logs is
# TRUE - because `targets` cannot otherwise tell that an external sheet
# changed.
log_cue <- if (isTRUE(project$always_refetch_logs)) {
  tar_cue(mode = "always")
} else {
  tar_cue()
}

# ---- log targets ------------------------------------------------------------
log_targets <- lapply(project$logs, function(log_name) {
  tar_target_raw(
    name    = paste0(log_name, "_log"),
    command = bquote(read_log(.(log_name), env_prefix = .(project$log_env_prefix %||% "ATRP_"))),
    cue     = log_cue
  )
})

# ---- per-tool targets -------------------------------------------------------
tools_df <- data.frame(
  tool_id = vapply(project$tools, `[[`, character(1), "short_name"),
  xlsform = vapply(project$tools, `[[`, character(1), "xlsform"),
  data    = vapply(project$tools, `[[`, character(1), "data"),
  stringsAsFactors = FALSE
)

per_tool <- tar_map(
  values = tools_df,
  names  = tool_id,

  # Read XLSForm + raw data (file-tracked so changes invalidate downstream)
  tar_target(xlsform_path,  xlsform, format = "file"),
  tar_target(data_path,     data,    format = "file"),
  tar_target(xform,         read_xlsform(xlsform_path)),
  tar_target(raw_data,      readr::read_csv(data_path, show_col_types = FALSE)),

  # Filter rejected interviews, then apply correction + translation logs
  tar_target(filtered,      filter_rejected(raw_data, rejection_log,
                                            key = project$rejection$key %||% "KEY")),
  tar_target(corrected,     apply_log(filtered,  correction_log)),
  tar_target(translated,    apply_log(corrected, translation_log)),

  # Attach value labels + recompute SM series cols
  tar_target(labeled,       labeler(translated, xform)),
  tar_target(synced,        update_series_cols(labeled, xform)),

  # Checks
  tar_target(rules,         create_relevancy_file(xform)),
  tar_target(rel_issues,    check_relevancy_rules(synced, rules)),
  tar_target(sm_issues,     check_select_multiple(synced, xform)),
  tar_target(trans_issues,  missing_translation(synced)),
  tar_target(custom_iss,    custom_checks(synced)),

  # Three output variants of the clean data
  tar_target(analyst,       drop_cols(synced, columns$drop_columns)),
  tar_target(analyst_lbl,   apply_column_labels(analyst, xform,
                                                 custom_labels = columns$custom_labels)),
  tar_target(client_,       build_client_version(analyst, xform, columns)),

  # Write outputs (each is a file target so paths are tracked)
  tar_target(out_analyst,
             write_csv_target(analyst,
                              file.path("output/analyst",
                                        paste0(tool_id, "_clean.csv"))),
             format = "file"),
  tar_target(out_analyst_lbl,
             write_csv_target(analyst_lbl,
                              file.path("output/analyst",
                                        paste0(tool_id, "_clean_labeled.csv"))),
             format = "file"),
  tar_target(out_client,
             write_csv_target(client_,
                              file.path("output/client",
                                        paste0(tool_id, "_client.csv"))),
             format = "file"),
  tar_target(out_issues,
             write_issues_xlsx(
               list(relevancy           = rel_issues,
                    select_multiple     = sm_issues,
                    missing_translation = trans_issues,
                    custom              = custom_iss),
               file.path("output/issues", paste0(tool_id, "_issues.xlsx"))),
             format = "file")
)

list(log_targets, per_tool)
