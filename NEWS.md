# atRfunctions 0.0.6

## New: `apply_column_labels()`

A general column-labeling function that renames data columns using the labels from the XLSForm `survey` sheet. Handles every question type, not just `select_multiple`:

- Plain questions (text, integer, select_one, etc.) are renamed to the sanitized survey label (e.g. `consent` -> `Do_you_consent`).
- `select_multiple` series columns are renamed using the **labeled** parent name as prefix, so series cols stay prefix-paired with the renamed parent (e.g. `fruits_1` -> `Pick_fruits.Apple` when `fruits` itself is labeled `"Pick fruits"`).
- Columns not present in the XLSForm can be renamed via an optional `custom_labels` argument - useful for calculated / derived variables added after data collection. Accepts a named character vector, a named list, or a `data.frame(name, label)`. Priority: `custom_labels` > select_multiple choice labels > survey label.

```r
data <- apply_column_labels(
  data, xform,
  custom_labels = c(age_bracket  = "Age bracket",
                    is_eligible  = "Is eligible?")
)
```

## `sm_label_separator` argument

`build_sm_label_map()` and `apply_sm_label_map()` now take an `sm_label_separator` argument (default `"."`) controlling the separator between the question name and the choice label in the generated `labeled_col`. The previous behavior (separator `"/"`) is recoverable by passing `sm_label_separator = "/"`. The new default produces valid R names without backticks (`data$fruits.Apple` vs `` data$`fruits/Apple` ``).

# atRfunctions 0.0.5

## Renamed: `reshape_tool` -> `build_sm_label_map`, `apply_SM_Label` -> `apply_sm_label_map`

The old names were misleading or inconsistent. The new pair is descriptive and uses the package's lowercase-with-underscores convention. The old names continue to work as deprecated wrappers (one-shot warning per call) so existing scripts keep running.

## Sanitized labels in `build_sm_label_map`

The previous `reshape_tool` only replaced spaces with underscores when building `response_label_new`. Choice labels containing `/`, `'`, `(`, `&`, `%`, leading digits, etc. were left intact, producing column names that were either not valid R identifiers (`fruits/100%`) or hard to read (`fruits/Apple/Banana` - which `/` is the separator?). `build_sm_label_map()` now sanitizes the label fragment via a new internal `.sanitize_r_name()`:

- non-`[A-Za-z0-9_.]` characters become `_`
- runs of `_` collapse
- leading/trailing `_`/`.` are stripped
- leading digit gets `x` prefix

Example transformations: `"Apple/Banana"` -> `"Apple_Banana"`, `"won't"` -> `"wont"`, `"100%"` -> `"x100"`, `"Apple (red)"` -> `"Apple_red"`. The `/` separator between the question prefix and the label fragment is preserved as before.

## `apply_sm_label_map` accepts an XLSForm directly

`apply_sm_label_map(data, x, ...)` is polymorphic in `x`:

- If `x` is a mapping data frame (has `dataset_col` and `labeled_col` columns), apply it.
- Otherwise treat `x` as an XLSForm (path / `read_xlsform()` result / survey data frame) and call `build_sm_label_map()` internally with the supplied `choice_label` / `tool_flavor`.

So both workflows work:

```r
# explicit two-step
mapping <- build_sm_label_map(xform)
data    <- apply_sm_label_map(data, mapping)

# one-shot
data <- apply_sm_label_map(data, xform)
```

# atRfunctions 0.0.4

## Uniform `tool` argument

Every function that takes an XLSForm-shaped `tool` argument now accepts all three canonical forms interchangeably:

1. A file path (`"path/to/form.xlsx"`).
2. A [read_xlsform()] result list.
3. A pre-read `survey`-sheet data frame (for functions that need only the survey sheet).

This means you can read an XLSForm once and pass it everywhere:

```r
xform <- read_xlsform("form.xlsx")
data  <- labeler(data, xform)
data  <- concat_url(data, xform, KEY = data$KEY)
data  <- update_media_links(data, xform)
rf    <- create_relevancy_file(xform)
mapping <- reshape_tool(xform)
```

Affected functions: `labeler`, `concat_url`, `update_media_links`, `add_repeat_sheet_names_to_questions`, `process_group_relevancies`, `convert_relevancy_to_R`, `create_relevancy_file`, `update_series_cols`, `check_select_multiple`, `reshape_tool`. The polymorphism is implemented via a new internal `.resolve_tool()` helper.

# atRfunctions 0.0.3

## Relevancy pipeline overhaul

The XLSForm relevancy conversion and checking pipeline was rewritten to fix several classes of invalid R conditions the old converter produced. The audit that drove the change is summarized below.

### Behavior changes (output-affecting)

- `create_relevancy_file()` gains two new columns: **`convert_status`** (`"ok"`, `"unsupported_function"`, `"unsupported_operator"`, `"parse_error"`, `"empty"`) and **`convert_error`** (short reason string when not `"ok"`). Existing columns are unchanged.
- Rules that use XPath functions other than `selected()` and `not()` (e.g. `count-selected`, `if`, `regex`, `coalesce`, `pulldata`, `int`, `string-length`, date/time helpers) are no longer best-effort converted into nonsense R. Their `Rcondition` is `NA` and `check_relevancy_rules()` skips them.
- The generated `Rcondition` strings are now produced via `deparse1()`-style quoting, which means literal regex patterns are written with the correct number of backslashes for `parse(text=...)`. The legacy `gsub("\\\\", "\\\\\\\\", ...)` post-processing in `create_relevancy_file()` is removed - it double-escaped the new strings.
- `check_relevancy_rules()` now returns a data frame with an `attr(<result>, "skipped")` attribute listing rules that were not evaluated and why. A summary `message()` reports the total skip count.

### Bug fixes

- **`selected()` with regex-meta choice values.** Previously `selected(${size}, '1.5')` became `grepl('\b1.5\b', data$size)` where `.` matches any character (so `"1X5"` matched). The converter now escapes regex metacharacters: the same input becomes `grepl("\\b1\\.5\\b", data$size)`, which matches `"1.5"` only.
- **Apostrophes in choice values.** `selected(${q}, "won't")` previously produced unparseable R (`data$q = 'won't'`). It now produces parseable R via `deparse1()`.
- **AND/OR precedence mangling.** The old converter wrapped AND clauses in parens whenever the expression contained `or`, producing unbalanced parens and reshuffling logic. The new converter relies on the fact that XPath `and`/`or` precedence matches R `&`/`|` precedence and does no paren rewriting.
- **String-vs-numeric `==` for character columns.** The old `==` -> `%in%` rewrite produced silently-wrong results when comparing a character column to an unquoted numeric literal (e.g. `c("5","10") %in% 5` returns all FALSE). The `%in%` rewrite is removed; comparisons use `==` (which R type-coerces correctly) and character-typed XLSForm fields are wrapped in `as.character()` to be robust to data-type drift.
- **`check_relevancy_rules()` no longer crashes on a bad rule.** A single `parse()`/`eval()` failure used to abort the whole file. Each rule is now wrapped in `tryCatch`; failures are logged in the `skipped` attribute and the loop continues.
- **Missing relevant-column path was buggy.** When a rule referenced a column not present in `data`, the function appended to `missing_relev_cols` but fell through to `eval()` and crashed. It now `next`s cleanly.
- **`process_group_relevancies()` precedence.** When concatenating a group's relevancy with a question's relevancy, the function now wraps operands containing `or` in parentheses so that XPath precedence is preserved. Previously `${a}=1` combined with `${b}=1 or ${b}=2` produced `${a}=1 and ${b}=1 or ${b}=2`, which XPath/R both parse as `(${a}=1 and ${b}=1) or ${b}=2` (the wrong tree).
- **`process_group_relevancies()` deduplication.** The old per-operator split-and-unique dedup mangled mixed `and`/`or` expressions. The new dedup only collapses adjacent identical operands at the top `and` level - never across `or` boundaries.

# atRfunctions 0.0.2

## New features

- `read_xlsform()` — a single helper that reads an XLSForm's `survey` and `choices` sheets and normalizes column-name differences between Kobo and SurveyCTO (`relevant`/`relevance`, `list_name`/`list name`, `name`/`value`). Used internally by every function that ingests an XLSForm.
- New `tool_flavor` argument (`"auto"`, `"kobo"`, `"surveycto"`) on `labeler()`, `concat_url()`, `update_media_links()`. Default is `"auto"` — the flavor is detected from the XLSForm's columns. Users who want to be explicit can pass `tool_flavor = "kobo"` or `tool_flavor = "surveycto"`.
- `reshape_tool()` — reads an XLSForm and returns a mapping from raw `select_multiple` series columns (e.g. `q1_3`) to labeled equivalents (e.g. `q1/Banana`). Auto-detects the label column when not supplied; works with both XLSForm flavors. Ported from `atRpipeline`.
- `apply_SM_Label()` — renames `select_multiple` series columns in a dataset using the mapping from `reshape_tool()`. Ported from `atRpipeline`.

## Improved

- `check_select_multiple()` now accepts either a file path (XLSForm) or a pre-read `survey` data frame. When passed a path, it uses `read_xlsform()` so both flavors work. Gained an `excluded_col` argument and a `tool_flavor` argument; switched `print()` to `message()` for the "no mismatches" notice.

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
