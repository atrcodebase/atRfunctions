# atRfunctions 0.0.2

## New features

- `read_xlsform()` — a single helper that reads an XLSForm's `survey` and `choices` sheets and normalizes column-name differences between Kobo and SurveyCTO (`relevant`/`relevance`, `list_name`/`list name`, `name`/`value`). Used internally by every function that ingests an XLSForm.
- New `tool_flavor` argument (`"auto"`, `"kobo"`, `"surveycto"`) on `labeler()`, `concat_url()`, `update_media_links()`. Default is `"auto"` — the flavor is detected from the XLSForm's columns. Users who want to be explicit can pass `tool_flavor = "kobo"` or `tool_flavor = "surveycto"`.

## Argument standardization

The following old argument names continue to work but emit a one-shot deprecation warning. Please migrate to the new names:

| Function | Deprecated | New |
|---|---|---|
| `labeler()` | `choice_lable` | `choice_label` |
| `update_media_links()` | `tool_path` | `tool` |
| `add_repeat_sheet_names_to_questions()` | `kobo_tool` | `tool` |
| `process_group_relevancies()` | `kobo_tool` | `tool` |
| `convert_relevancy_to_R()` | `kobo_tool` | `tool` |
| `create_relevancy_file()` | `kobo_tool` | `tool` |

## Bug fixes

- **`missing_translation()` now returns its log.** Prior to this release the function built the result internally but never returned it, so callers received `NULL`. This is the one intentional output-changing fix in this release.

## Newly exported

- `check_select_multiple()`, `create_directory()`, `update_media_links()`, `reshape_to_datamerge()` — these were defined in `R/` but missing from `NAMESPACE`/`man/`. They are now properly exported.

## Internal cleanup

- Removed `install.packages()` calls from inside function bodies. All required packages are declared in `DESCRIPTION` `Imports:`.
- Removed the spurious `@import devtools` from `reshape_to_datamerge()`.
- Consolidated the `%notin%` operator into `R/utils.R` so it no longer has to be re-defined in every consumer.
- Fixed `apply_log()` roxygen typo (`@param date_KEY` → `@param data_KEY`).
