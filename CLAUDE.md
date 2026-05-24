# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

`atRfunctions` is an R package that wraps frequently-used custom functions for the ATR Data Management Team. The functions support survey data workflows built around **SurveyCTO / Kobo XLSForm** tools — labeling data with choice lists, validating skip-logic (relevancy) rules from the XLSForm, applying cleaning logs, reshaping analysis results for Adobe InDesign Data Merge, etc.

## Package development commands

This is a standard R package (`DESCRIPTION` + `R/` + `man/` + `NAMESPACE`) built with **roxygen2** and **devtools**. Run from the package root in an R session:

```r
# Regenerate NAMESPACE + man/*.Rd from roxygen comments (do this after editing any R/*.R headers)
devtools::document()

# Install the package locally for testing
devtools::install()

# R CMD check (full build + check)
devtools::check()

# Quick reload during iteration without reinstalling
devtools::load_all()
```

There is no `tests/` directory — the package currently has no automated test suite, so verification is done by loading and exercising functions interactively against real SurveyCTO/Kobo data and tools.

Users install the development version via:

```r
devtools::install_github("atrcodebase/atRfunctions")
```

## Architecture

All exported functions live as single files in `R/` (one function per file, matching the filename), are documented via roxygen2 headers, and are listed in `NAMESPACE` via `@export`. There is no shared internal state and no S4/R6 classes — each function is independent.

The functions fall into a few cooperating groups:

### Relevancy (skip-logic) pipeline

This is the most multi-file workflow in the package. The XLSForm `relevant` column uses ODK/SurveyCTO syntax (e.g. `${q1} = 'yes' and selected(${q2}, 'a')`), which has to be converted to R before it can be evaluated against a data frame. The pipeline is:

1. `process_group_relevancies()` — propagates `begin_group` / `end_group` relevancies down onto each contained question (mutates `kobo_tool$relevant`).
2. `add_repeat_sheet_names_to_questions()` — tags each question with the repeat-group / sheet it belongs to (so the relevancy check knows which dataset sheet a question lives on).
3. `questions_from_relevancy()` / `choices_from_relevancy()` — parse `${var}` references and `'value'` literals out of relevancy strings.
4. `convert_relevancy_to_R()` — translates ODK syntax to R: `${q}` → `data$q`, `selected(${q}, 'x')` → `grepl('\\bx\\b', data$q)`, `=`/`<>`/`and`/`or` → `==`/`!=`/`&`/`|`, finally `==`/`!=` → `%in%`/`%notin%`. The user-facing wrapper is `create_relevancy_file()`.
5. `create_relevancy_file(kobo_tool, ignore_reverse_check)` — orchestrates steps 1–4 and returns a data frame whose `Rcondition` column holds the converted expressions, plus `name`, `relevance_rule`, `relevant_question`, `sheet`, `check_reverse`, etc.
6. `check_relevancy_rules(data, tool_relevancy, sheet_name, KEY)` — consumes the output of `create_relevancy_file()` and runs each `Rcondition` via `eval(parse(text=...))` against `data`, flagging two cases: a value present when the condition is false, and (if `check_reverse=TRUE`) a missing value when the condition is true. Returns a long-format log of violations.

When changing any of these functions, keep this contract intact: `create_relevancy_file()`'s output is the exact input shape `check_relevancy_rules()` expects, and `Rcondition` must be evaluable in an environment where the data frame is bound to the symbol `data`.

### XLSForm-driven data transformations

- `labeler(data, tool, ...)` — reads the `survey` and `choices` sheets of the XLSForm and replaces choice values in single- and multi-select columns with their labels (default `label::English`). Multi-select columns are split on space and rejoined with `multi_response_sep` (default `;`). It uses an internal `add_underscore()` trick (wrapping digits with underscores during replacement) to avoid partial-number collisions like `1` matching inside `10`.
- `concat_url(data, tool, ...)` — for media/audit question types (`audio`, `image`, `audio audit`, `text audit`), turns the bare filename in the export into a full SurveyCTO submission-attachment URL using the row's UUID.
- `update_series_cols(data, multi_vars, question_separator = "/")` — recomputes the per-choice 0/1 dummy columns (`q/choice1`, `q/choice2`, …) from the space-separated parent column for select_multiple questions.

### Data ops

- `compare_dt(df1, df2, unique_id_df1, unique_id_df2, compare_all)` — diff two snapshots of a dataset by unique ID.
- `apply_log(data, log, data_KEY, log_columns)` — applies a cleaning/change log (rows of `question / old_value / new_value / KEY`) to `data`, dispatching by column class (POSIXct → `as.Date`, character → `as.character`, otherwise `as.numeric`). If any row fails, the error message names the offending UUID / column.
- `missing_translation(data, KEY)` — flags untranslated text fields.
- `reshape_to_datamerge(analysis_result)` — pivots a long analysis result (`Disaggregation`, `Disaggregation_level`, `Question`, `Response`, `Aggregation_method`, `Result`) into the wide format Adobe InDesign Data Merge expects; `Aggregation_method == 'perc'` formats results with a `%` suffix, otherwise prefixes the question with the aggregation name.

### Utilities

- `read_xlsx_sheets()` — reads all sheets of an `.xlsx` into a named list.
- `create_directory()`, `update_media_links()`, `check_select_multiple()` — small helpers (not all exported; check `NAMESPACE`).

## Conventions when editing functions

- Every exported function has roxygen2 documentation in its `R/<name>.R` header — keep `@param`, `@usage`, `@import`, and `@export` in sync, then run `devtools::document()` so `NAMESPACE` and `man/<name>.Rd` regenerate. Don't hand-edit `NAMESPACE` or files in `man/`.
- Package dependencies live in `DESCRIPTION` under `Imports:`. Don't add `library()` calls inside functions — declare imports with roxygen `@import` / `@importFrom` tags.
- Functions assume **tidyverse-style** inputs (data frames, `dplyr` verbs, the magrittr pipe `%>%` and the base `|>` are both used). `%notin%` is defined locally inside functions that need it (`` `%notin%` <- Negate(`%in%`) ``).
- XLSForm column conventions the code depends on: `survey` sheet has `type`, `name`, `relevant` (or `relevance`), and a label column like `label::English`; `choices` sheet has `list_name` (or `list name`), `name` (or `value`), and the same label column. `labeler()` and `create_relevancy_file()` both renormalize these names defensively — preserve that behavior if you refactor.
- `KEY` is the conventional name for the unique-ID column (UUID from SurveyCTO). It's the default for most function arguments.
