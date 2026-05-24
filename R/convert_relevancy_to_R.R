#' Converts XLSForm relevancy rules to R expressions
#'
#' Accepts a pre-read XLSForm `survey` data frame (Kobo or SurveyCTO). Column
#' name differences (`relevant`/`relevance`) are normalized internally.
#'
#' Rules that use XPath functions other than `selected()` and `not()` (e.g.
#' `count-selected`, `if`, `regex`, `coalesce`, `pulldata`, date/time helpers,
#' `int`, `string-length`) are deliberately **not** converted - the function
#' returns `NA` for those rules so they can be skipped downstream rather than
#' miscompiled into nonsense R. Use [create_relevancy_file()] to get a
#' diagnostics column (`convert_status`) describing why each NA was returned.
#'
#' @param tool path to the XLSForm, a [read_xlsform()] result list, or a
#'   pre-read `survey`-sheet data frame.
#' @param field_types Optional named character vector mapping question name to
#'   primary XLSForm type (`"text"`, `"integer"`, `"select_one"`,
#'   `"select_multiple"`, ...). If `NULL`, built from `tool`.
#' @param ... For backwards compatibility: the previous argument `kobo_tool`
#'   is still accepted as a deprecated alias for `tool`.
#'
#' @return Character vector of R expressions, same length as the survey
#'   sheet. `NA_character_` for rules that are empty or unsupported.
#' @import stringr
#'
#' @export
convert_relevancy_to_R <- function(tool = NULL, field_types = NULL, ...) {
  tool <- .deprecated_arg(tool, list(...), new_name = "tool", old_name = "kobo_tool")
  if (is.null(tool)) stop("`tool` is required.", call. = FALSE)

  tool <- .resolve_tool(tool)$survey

  if (!all(c("type", "name", "relevant") %in% names(tool))) {
    stop("Required variable(s) not found in XLSForm")
  }

  if (is.null(field_types)) field_types <- .xlsform_field_types(tool)

  vapply(tool$relevant, function(rule) .convert_one_rule(rule, field_types),
         character(1), USE.NAMES = FALSE)
}

# Convert a single XLSForm relevance string to an R expression. Returns
# NA_character_ for empty or unsupported rules.
.convert_one_rule <- function(rule, field_types) {
  status <- .classify_relevance(rule)
  if (status != "ok") return(NA_character_)

  rule <- as.character(rule)

  # 1. Stash selected() and not(selected()) as opaque placeholders so the
  #    following gsub steps don't mangle their internals. The placeholders
  #    carry the question name + the original quoted choice literal verbatim.
  selected_re <- "selected\\(\\s*\\$\\{([^}]+)\\}\\s*,\\s*(\"[^\"]*\"|'[^']*')\\s*\\)"
  not_selected_re <- paste0("not\\(\\s*", selected_re, "\\s*\\)")
  rule <- gsub(not_selected_re, "<<NOTSEL::\\1::\\2>>", rule, perl = TRUE)
  rule <- gsub(selected_re,     "<<SEL::\\1::\\2>>",    rule, perl = TRUE)

  # 2. Replace remaining ${var} with data$var. If we have a field type and
  #    it's a character-like field, wrap in as.character() so that equality
  #    comparisons against string literals work even if the column has been
  #    coerced to factor/numeric.
  rule <- .replace_field_refs(rule, field_types)

  # 3. Normalize XLSForm operators to R syntax.
  rule <- gsub("<>", "!=", rule, fixed = TRUE)            # XPath not-equal
  rule <- gsub("(?<![!<>=])=(?!=)", "==", rule, perl = TRUE) # single = -> ==
  rule <- gsub(">\\s*=", ">=", rule, perl = TRUE)
  rule <- gsub("<\\s*=", "<=", rule, perl = TRUE)

  # 4. Replace logical keywords. Note: XPath precedence (and binds tighter
  #    than or) matches R precedence (& binds tighter than |), so a plain
  #    substitution preserves correctness as long as the original parens are
  #    left alone. We intentionally do NOT add or rearrange parens.
  rule <- gsub("\\bnot\\s*\\(", "!(", rule, perl = TRUE)
  rule <- gsub("\\band\\b",     "&",  rule, perl = TRUE)
  rule <- gsub("\\bor\\b",      "|",  rule, perl = TRUE)

  # 5. Canonicalize any *remaining* string literals (those that didn't get
  #    stashed in step 1 - e.g. RHS of plain `${q} = 'won''t'`). Crucial:
  #    must happen BEFORE we expand the placeholders, because the expansion
  #    injects already-canonical literals (regex patterns wrapped via
  #    deparse1) that we don't want re-processed.
  rule <- .normalize_string_literals(rule)

  # 6. Now expand the placeholders we stashed in step 1.
  rule <- .expand_selected_placeholders(rule, field_types)

  # 7. Tidy whitespace.
  rule <- gsub("\\s+", " ", rule, perl = TRUE)
  rule <- trimws(rule)

  if (rule == "") return(NA_character_)
  rule
}

# Replace ${var} with data$var, optionally wrapping in as.character() for
# character-like fields.
.replace_field_refs <- function(rule, field_types) {
  re <- "\\$\\{([^}]+)\\}"
  repeat {
    m <- regexec(re, rule, perl = TRUE)
    parts <- regmatches(rule, m)[[1]]
    if (length(parts) == 0) break
    qname <- parts[2]
    ftype <- if (qname %in% names(field_types)) field_types[[qname]] else NA_character_
    if (.is_character_type(ftype)) {
      repl <- sprintf("as.character(data$%s)", qname)
    } else {
      repl <- sprintf("data$%s", qname)
    }
    rule <- sub(re, "@@FIELDREF@@", rule, perl = TRUE)
    rule <- sub("@@FIELDREF@@", repl, rule, fixed = TRUE)
  }
  rule
}

# Numeric XLSForm types we *don't* wrap in as.character().
.is_character_type <- function(ftype) {
  if (is.na(ftype)) return(TRUE)  # Unknown -> default to character (XLSForm export is usually chars)
  !(ftype %in% c("integer", "decimal"))
}
