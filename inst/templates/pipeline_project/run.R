# Pipeline for {{project_name}}.
#
# Run with:
#   source("run.R")              # interactive (results stay in your env)
#   Rscript run.R                # batch
#
# Edit any stage in R/stages.R or R/custom_checks.R - everything is a plain
# function call, so you can drop browser() anywhere and re-source.

suppressPackageStartupMessages({
  library(atRfunctions)
})

source("R/log_io.R")
source("R/stages.R")
source("R/custom_checks.R")

project <- yaml::read_yaml("config/project.yml")
columns <- yaml::read_yaml("config/columns.yml")

# ---- 1. Read shared logs ----------------------------------------------------
message("Reading logs ...")
logs <- setNames(
  lapply(project$logs, function(name) {
    message(sprintf("  - %s", name))
    read_log(name, env_prefix = project$log_env_prefix %||% "ATRP_")
  }),
  project$logs
)

# ---- 2. Per-tool pipeline ---------------------------------------------------
# `results` holds every per-tool intermediate, so after source("run.R")
# you can inspect e.g. results$main$data interactively.
results <- list()

for (tool in project$tools) {
  short <- tool$short_name
  message(sprintf("\n=== %s ===", short))

  # 2a. Read inputs
  message("  Reading XLSForm + raw data ...")
  xform <- read_xlsform(tool$xlsform)
  data  <- readr::read_csv(tool$data, show_col_types = FALSE)

  # 2b. Filter rejected interviews using the rejection log
  if (!is.null(project$rejection)) {
    message("  Filtering rejected interviews ...")
    data <- filter_rejected(
      data,
      logs[[project$rejection$log]],
      key = project$rejection$key %||% "KEY"
    )
  }

  # 2c. Apply correction + translation logs
  if (!is.null(logs$correction)) {
    message("  Applying correction log ...")
    data <- apply_log(data, logs$correction)
  }
  if (!is.null(logs$translation)) {
    message("  Applying translation log ...")
    data <- apply_log(data, logs$translation)
  }

  # 2d. Value labeling + select_multiple sync
  message("  Attaching value labels ...")
  data <- labeler(data, xform)
  message("  Recomputing select_multiple series cols ...")
  data <- update_series_cols(data, xform)

  # 2e. Checks
  message("  Running checks ...")
  rules        <- create_relevancy_file(xform)
  rel_issues   <- check_relevancy_rules(data, rules)
  sm_issues    <- check_select_multiple(data, xform)
  trans_issues <- missing_translation(data)
  custom_iss   <- custom_checks(data)

  # 2f. Build the three output flavors
  message("  Building outputs ...")
  analyst     <- drop_cols(data, columns$drop_columns)
  analyst_lbl <- apply_column_labels(analyst, xform,
                                      custom_labels = columns$custom_labels)
  client      <- build_client_version(analyst, xform, columns)

  # 2g. Write to disk
  message("  Writing files ...")
  write_csv_safe(analyst,
                 file.path("output/analyst", paste0(short, "_clean.csv")))
  write_csv_safe(analyst_lbl,
                 file.path("output/analyst", paste0(short, "_clean_labeled.csv")))
  write_csv_safe(client,
                 file.path("output/client",  paste0(short, "_client.csv")))
  write_issues_xlsx(
    list(relevancy           = rel_issues,
         select_multiple     = sm_issues,
         missing_translation = trans_issues,
         custom              = custom_iss),
    file.path("output/issues", paste0(short, "_issues.xlsx"))
  )

  # Keep everything in memory for interactive inspection
  results[[short]] <- list(
    xform       = xform,
    data        = data,
    analyst     = analyst,
    analyst_lbl = analyst_lbl,
    client      = client,
    rules       = rules,
    issues      = list(
      relevancy           = rel_issues,
      select_multiple     = sm_issues,
      missing_translation = trans_issues,
      custom              = custom_iss
    )
  )
}

message(sprintf(
  "\nDone. %d tool(s) processed. Inspect via results$<tool_name>$...",
  length(project$tools)
))
