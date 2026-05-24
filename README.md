# atRfunctions

<!-- badges: start -->
<!-- badges: end -->

`atRfunctions` is the ATR Data Management Team's R package of survey-workflow
helpers. It packages the routines we use on every project — labeling data
from an XLSForm, validating skip-logic rules, applying cleaning logs,
reshaping analysis results — behind one cohesive API. Both **Kobo** and
**SurveyCTO** XLSForms are supported out of the box, with auto-detected
column normalization.

## Installation

```r
# install.packages("devtools")
devtools::install_github("atrcodebase/atRfunctions")
```

## Quick start

```r
library(atRfunctions)

# Read the XLSForm once, pass it everywhere.
xform <- read_xlsform("path/to/tool.xlsx")

# Apply column labels (renames every column using survey + choice labels).
data <- apply_column_labels(data, xform)

# Apply value labels (replaces choice codes with labels inside cells).
data <- labeler(data, xform, multi_response_sep = ";")

# Build a relevancy file and flag rows that violate skip logic.
rules <- create_relevancy_file(xform)
violations <- check_relevancy_rules(data, rules)
```

## Kobo & SurveyCTO support

Every XLSForm-aware function accepts the **tool** in three interchangeable
forms:

| Form | Example |
| --- | --- |
| File path | `labeler(data, "tool.xlsx")` |
| `read_xlsform()` result | `labeler(data, xform)` |
| Pre-read `survey` data frame | `convert_relevancy_to_R(survey_df)` |

Flavor is auto-detected from the XLSForm's column names (Kobo uses
`relevant` / `list_name`; SurveyCTO uses `relevance` / `list name`).
Override with `tool_flavor = "kobo"` or `"surveycto"` if needed.

```r
labeler(data, "tool.xlsx", tool_flavor = "kobo")
concat_url(data, "tool.xlsx", tool_flavor = "surveycto")
```

## Function reference

Every function exported by the package is documented below, grouped by
purpose. Use `?function_name` for the full reference.

### XLSForm I/O

#### `read_xlsform()`

Read an XLSForm and normalize the column names so downstream code does not
have to care about the Kobo / SurveyCTO differences.

```r
xform <- read_xlsform("tool.xlsx", flavor = "auto", guess_max = 100000)
# xform$survey, xform$choices, xform$flavor
```

#### `read_xlsx_sheets()`

Read every sheet of an `.xlsx` file into a named list of data frames.

```r
sheets <- read_xlsx_sheets("workbook.xlsx",
                           guess_max = 5000000,
                           convert_to_na = c("N/A", "-", " "))
```

### Column labeling (rename data columns)

#### `apply_column_labels()`

Rename data columns to use the question labels from the XLSForm — for every
question type. `select_multiple` series columns inherit the labeled parent
name as their prefix, so the parent/series relationship survives the
rename. Columns not in the XLSForm can be relabeled via `custom_labels`.

```r
data <- apply_column_labels(
  data, xform,
  survey_label       = NULL,          # auto-pick label::English / label:English / label
  choice_label       = NULL,
  sm_label_separator = ".",           # fruits_1 -> Pick_fruits.Apple
  custom_labels      = c(age_bracket = "Age bracket",
                         is_eligible = "Is eligible?"),
  excluded_cols      = c("KEY")
)
```

#### `build_sm_label_map()`

Build a mapping from raw `select_multiple` series columns (`q_1`, `q_2`)
to labeled equivalents (`q.Apple`, `q.Banana`). The choice label is
sanitized so the result is a valid R name. Use directly when you want
explicit control, or implicitly via `apply_sm_label_map()`.

```r
mapping <- build_sm_label_map(xform,
                              choice_label       = "label::English",
                              sm_label_separator = ".")
```

#### `apply_sm_label_map()`

Rename `select_multiple` series columns. The second argument is
polymorphic — either a pre-built `build_sm_label_map()` mapping or an
XLSForm (the map is built internally in that case).

```r
# Two-step (explicit map)
mapping <- build_sm_label_map(xform)
data    <- apply_sm_label_map(data, mapping)

# One-shot (build map internally)
data    <- apply_sm_label_map(data, xform, sm_label_separator = ".")
```

### Value labeling (replace choice codes with labels in cells)

#### `labeler()`

Replace choice codes with their labels in `select_one` and `select_multiple`
columns. `data$consent` values like `"yes"` become `"Yes"`; multi-select
values like `"1 3"` become `"Apple;Cherry"`.

```r
data <- labeler(data, xform,
                survey_label       = "label::English",
                choice_label       = "label::English",
                multi_response_sep = ";")
```

### Media URLs

#### `concat_url()`

Build full media-attachment URLs for `image` / `audio` / `audio audit` /
`text audit` columns using the survey UUID.

```r
data <- concat_url(data, xform,
                   server_name    = "https://atrconsultingaf.surveycto.com",
                   KEY            = data$KEY,
                   question_types = c("audio audit", "text audit", "audio", "image"))
```

#### `update_media_links()`

Add proper download URLs for media columns in data exported from SCTO
Desktop. Optionally writes results into a new column rather than overwriting.

```r
data <- update_media_links(data, xform,
                           download_link = "https://artftpm.surveycto.com/view/submission-attachment/",
                           key_col       = "KEY",
                           rename        = FALSE)
```

### `select_multiple` helpers

#### `update_series_cols()`

Recompute the 0/1 dummy columns (`q_1`, `q_2`, ...) for every
`select_multiple` question from its parent column's space-separated codes.

```r
data <- update_series_cols(data, xform, question_separator = "_")
```

#### `check_select_multiple()`

Verify the parent column and its series columns agree. Returns a data
frame of inconsistencies.

```r
issues <- check_select_multiple(data, xform,
                                question_separator = "_",
                                KEY                = "KEY",
                                excluded_col       = "")
```

### Relevancy / skip-logic pipeline

#### `create_relevancy_file()`

Produce the table of relevance rules (one row per question with a non-empty
`relevant` expression). Columns include `Rcondition` (the rule converted
to R), `convert_status` (`"ok"`, `"unsupported_function"`,
`"unsupported_operator"`, `"parse_error"`, `"empty"`), and `convert_error`.

```r
rules <- create_relevancy_file(xform,
                               ignore_reverse_check = c("Q1", "Q10"))
```

#### `check_relevancy_rules()`

Walk a relevancy file and flag rows in `data` that violate the rules.
Rules with `convert_status != "ok"` are skipped (logged via
`attr(result, "skipped")`). Individual rule failures are caught and
logged — one bad rule never aborts the whole check.

```r
violations <- check_relevancy_rules(data, rules,
                                    sheet_name = "data",
                                    KEY        = "KEY")
attr(violations, "skipped")   # which rules were not evaluated, and why
```

#### `convert_relevancy_to_R()`

Translate XLSForm relevance expressions to R. Used internally by
`create_relevancy_file()`; exposed for advanced workflows.

```r
expressions <- convert_relevancy_to_R(xform)
# selected(${gender}, 'female')  ->  grepl("\\bfemale\\b", data$gender)
# ${age} >= 18                   ->  as.numeric(data$age) >= 18
# count-selected(${q}) > 2       ->  NA   (unsupported -> flagged & skipped)
```

#### `process_group_relevancies()`

Propagate a `begin_group` / `begin_repeat` relevancy onto every question
inside that group, preserving operator precedence.

```r
survey <- process_group_relevancies(xform)
```

#### `add_repeat_sheet_names_to_questions()`

Tag every question with the data-sheet (repeat group) it lives on.

```r
sheet_map <- add_repeat_sheet_names_to_questions(xform)
```

#### `questions_from_relevancy()` / `choices_from_relevancy()`

Parse a relevance expression to extract referenced question names or
choice codes.

```r
questions_from_relevancy("selected(${q1}, '1') and ${q2} = 'yes'")
# -> "q1 - q2"
choices_from_relevancy("selected(${q1}, '1') and ${q2} = 'yes'")
# -> "1  yes"
```

### Data operations

#### `apply_log()`

Apply a data-cleaning log to a dataset. The log carries
`question` / `old_value` / `new_value` / `KEY` columns; cell values are
coerced to the column's existing type (character / numeric / Date).

```r
data <- apply_log(data, log,
                  data_KEY    = "KEY",
                  log_columns = c(question  = "question",
                                  old_value = "old_value",
                                  new_value = "new_value",
                                  KEY       = "KEY"))
```

#### `compare_dt()`

Diff two versions of the same dataset. Returns a long-format data frame of
`question` / `old_value` / `new_value` / `KEY` rows for every cell that
changed.

```r
diff <- compare_dt(df_old, df_new,
                   unique_id_df1 = "KEY",
                   unique_id_df2 = "KEY",
                   compare_all   = TRUE)
```

#### `missing_translation()`

Scan a dataset for cells containing non-ASCII / untranslated text and
return them as a log ready for the translation team.

```r
log <- missing_translation(data,
                           KEY           = "KEY",
                           excluded_cols = c("KEY", "submission_time"))
```

#### `reshape_to_datamerge()`

Pivot a long analysis result (with `Disaggregation`,
`Disaggregation_level`, `Question`, `Response`, `Aggregation_method`,
`Result`) into the wide format Adobe InDesign Data Merge expects.
`perc` values get a `%` suffix; non-perc methods are prefixed
(`mean of <var>`).

```r
wide <- reshape_to_datamerge(analysis_result)
```

### Utilities

#### `create_directory()`

Create a directory recursively if it does not already exist.

```r
create_directory("output/2026-05/cleaning_logs")
```

## Deprecations

These names continue to work but emit a one-shot warning. Migrate at your
convenience.

| Deprecated | Replacement |
| --- | --- |
| `reshape_tool()` | `build_sm_label_map()` |
| `apply_SM_Label()` | `apply_sm_label_map()` |
| `kobo_tool =` argument | `tool =` |
| `choice_lable =` argument (typo) | `choice_label =` |
| `tool_path =` argument | `tool =` |

## Contributing

Run the test suite and check the package before sending a PR:

```r
devtools::document()   # regenerate NAMESPACE + man/ from roxygen
devtools::test()       # run testthat suite
devtools::check()      # full R CMD check
```

***

##### Author: ATR — Data Management Team
