# {{project_name}}

Survey data-processing pipeline scaffolded by
[`atRfunctions::scaffold_pipeline_project()`](https://github.com/atrcodebase/atRfunctions).
The pipeline is a plain `run.R` script that calls `atRfunctions` helpers in
sequence — easy to read, easy to debug (drop `browser()` anywhere and
re-source), easy to hand off.

## Layout

```
config/
  project.yml         # tools, logs, rejection rule
  columns.yml         # drop / pii / custom_labels
input/
  tools/              # XLSForm files (one per tool)
  data/               # raw CSV inputs (one per tool)
R/
  log_io.R            # Google Sheets log reader (pub-CSV + googlesheets4)
  stages.R            # filter_rejected, drop_cols, build_client_version, ...
  custom_checks.R     # project-specific logic checks (fill in)
output/
  analyst/            # full clean dataset (raw + labeled column names)
  client/             # PII-stripped client deliverable
  issues/             # consolidated issues workbooks
run.R                 # pipeline entry point
.Renviron.example     # log URL template - copy to .Renviron and fill in
```

## Quick start

1. **Drop your XLSForm(s)** under `input/tools/` and **raw CSV(s)** under
   `input/data/`. Each pair gets one entry in `config/project.yml`:

   ```yaml
   tools:
     - short_name: main
       xlsform: "input/tools/main.xlsx"
       data:    "input/data/main.csv"
   ```

2. **Configure logs.** Copy `.Renviron.example` to `.Renviron` and fill in
   the URLs / GIDs. Two URL shapes are supported automatically:

   - Publish-to-Web CSV URLs (no auth) for public logs.
   - Full Google Sheets URLs (private) - run `googlesheets4::gs4_auth()`
     once to cache an OAuth token.

3. **Edit `config/columns.yml`** with the columns to drop project-wide
   (`drop_columns`), to additionally drop from the client version
   (`pii_columns`), and any labels for calculated variables added during
   cleaning (`custom_labels`).

4. **Customize the checks** in `R/custom_checks.R` if you have
   project-specific logic the generic checks (relevancy, select_multiple,
   missing_translation) don't cover.

5. **Run the pipeline:**

   ```r
   source("run.R")        # interactive - results stay in your global env
   # or
   Rscript run.R          # batch
   ```

   Per tool, four files land under `output/`:

   - `output/analyst/<tool>_clean.csv` — cleaned data, raw column names.
   - `output/analyst/<tool>_clean_labeled.csv` — cleaned data, column names
     replaced with survey labels (analyst-friendly).
   - `output/client/<tool>_client.csv` — same as labeled but PII columns
     removed.
   - `output/issues/<tool>_issues.xlsx` — one sheet per issue family
     (relevancy violations, select_multiple inconsistencies, untranslated
     text, custom checks).

## Debugging tips

The pipeline runs top-to-bottom in a single R session, so:

- **Step through any stage**: put `browser()` before the function call in
  `run.R` (or inside the function in `R/stages.R`), then `source("run.R")`.
- **Skip a stage**: comment it out. Each step writes back to `data`, so
  removing a line just leaves the upstream value in place.
- **Inspect any intermediate after a successful run**:
  `results$main$data`, `results$main$analyst`, `results$main$issues$...`.
- **Re-run only one tool**: remove the others from `config/project.yml`
  (or use `project$tools <- project$tools[1]` after sourcing the config).
