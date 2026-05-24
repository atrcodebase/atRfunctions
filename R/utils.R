#' Internal package utilities (not exported)
#'
#' @keywords internal
#' @name atRfunctions-internal
#' @importFrom stats setNames
#' @importFrom utils head
NULL

# Used by check_relevancy_rules, check_select_multiple, missing_translation.
`%notin%` <- Negate(`%in%`)

# Soft-deprecation helper. Looks up `old_name` in `dots`; if present, warns and
# returns its value when `new_value` was not supplied. Designed to be called as:
#
#   foo <- function(x = NULL, ...) {
#     x <- .deprecated_arg(x, list(...), new_name = "x", old_name = "old_x")
#   }
.deprecated_arg <- function(new_value, dots, new_name, old_name) {
  if (old_name %in% names(dots)) {
    warning(sprintf("Argument `%s` is deprecated; use `%s` instead.",
                    old_name, new_name), call. = FALSE)
    if (is.null(new_value)) return(dots[[old_name]])
  }
  new_value
}

# Detect XLSForm flavor from a survey/choices data frame's column names.
# Returns "kobo", "surveycto", or "unknown".
.detect_tool_flavor <- function(survey = NULL, choices = NULL) {
  if (!is.null(survey) && "relevance" %in% names(survey)) return("surveycto")
  if (!is.null(survey) && "relevant" %in% names(survey))  return("kobo")
  if (!is.null(choices) && "list name" %in% names(choices)) return("surveycto")
  if (!is.null(choices) && "list_name" %in% names(choices)) return("kobo")
  if (!is.null(choices) && "value" %in% names(choices))     return("surveycto")
  "unknown"
}

# Normalize a survey-sheet data frame so downstream code can rely on:
#   - column `relevant` (renamed from `relevance` if needed)
.normalize_survey <- function(survey) {
  if (!"relevant" %in% names(survey) && "relevance" %in% names(survey)) {
    names(survey)[names(survey) == "relevance"] <- "relevant"
  }
  survey
}

# Normalize a choices-sheet data frame so downstream code can rely on:
#   - column `list_name` (renamed from `list name`)
#   - column `name`      (renamed from `value`)
.normalize_choices <- function(choices) {
  if ("list name" %in% names(choices)) {
    names(choices)[names(choices) == "list name"] <- "list_name"
  }
  if (!"name" %in% names(choices) && "value" %in% names(choices)) {
    names(choices)[names(choices) == "value"] <- "name"
  }
  choices
}

# Resolve a label column name. Accepts user input like "English",
# "label::English", or "label:English" and returns whichever form actually
# exists in `available_cols`. Falls back to the original if no match.
.resolve_label_col <- function(label, available_cols) {
  # Already an existing column name? use it.
  if (label %in% available_cols) return(label)
  # Bare language, e.g. "English" -> try both prefixed forms.
  candidates <- c(label,
                  paste0("label::", label),
                  paste0("label:",  label),
                  sub("^label::", "label:", label),
                  sub("^label:",  "label::", label))
  hit <- candidates[candidates %in% available_cols]
  if (length(hit) >= 1) return(hit[1])
  label
}

# Polymorphic `tool` resolver. Accepts any of:
#   - a file path to an XLSForm xlsx     -> reads both sheets via read_xlsform()
#   - a read_xlsform() result list       -> used as-is
#   - a pre-read `survey`-sheet data frame -> wrapped as list(survey, choices=NULL)
#
# Always returns the list shape `list(survey, choices, flavor)`. `choices`
# may be NULL when the caller only passed a survey-sheet data frame.
.resolve_tool <- function(tool, tool_flavor = "auto", needs_choices = FALSE) {
  if (is.character(tool) && length(tool) == 1) {
    return(read_xlsform(tool, flavor = tool_flavor))
  }
  if (is.list(tool) && !is.data.frame(tool) &&
      all(c("survey", "choices") %in% names(tool))) {
    # Already a read_xlsform() result. Normalize defensively in case the caller
    # built it by hand from sheets that haven't been normalized yet.
    tool$survey  <- .normalize_survey(tool$survey)
    if (!is.null(tool$choices)) tool$choices <- .normalize_choices(tool$choices)
    if (is.null(tool$flavor)) tool$flavor <- "unknown"
    return(tool)
  }
  if (is.data.frame(tool)) {
    if (needs_choices) {
      stop("This function needs both the survey and choices sheets. Pass a ",
           "file path or a read_xlsform() result instead of a bare survey ",
           "data frame.", call. = FALSE)
    }
    return(list(survey = .normalize_survey(tool), choices = NULL,
                flavor = "unknown"))
  }
  stop("Unrecognized `tool`. Expected a file path, a read_xlsform() result, ",
       "or the survey-sheet data frame.", call. = FALSE)
}

# ---- relevancy-conversion helpers ----

# Build a question-name -> XLSForm-primary-type lookup ("select_one",
# "select_multiple", "text", "integer", "decimal", ...). Used by
# convert_relevancy_to_R() to decide:
#   - whether selected() should become grepl() (select_multiple) or == (select_one)
#   - whether to wrap data$<col> in as.character() (text/select_* fields)
.xlsform_field_types <- function(survey) {
  if (is.null(survey) || nrow(survey) == 0) {
    return(setNames(character(0), character(0)))
  }
  type_first <- sub("\\s.*$", "", as.character(survey$type))
  setNames(type_first, survey$name)
}

# Classify a single XLSForm relevance expression. Returns one of:
#   "empty"                 - blank, NA, or "NA"
#   "ok"                    - uses only constructs the converter supports
#   "unsupported_function"  - calls a function other than selected() or not()
#   "unsupported_operator"  - uses div/mod or other operators we don't translate
.classify_relevance <- function(rule) {
  if (is.na(rule) || trimws(rule) == "" || trimws(rule) == "NA") return("empty")

  # Detect div/mod operators (XPath integer division and modulo). These
  # collide with R variable parsing so we don't try to convert them.
  if (grepl("\\b(div|mod)\\b", rule)) return("unsupported_operator")

  # Find every function-call-like token: an identifier followed by '('.
  # Allow hyphens in the identifier (XPath functions like count-selected).
  calls <- regmatches(rule, gregexpr("[A-Za-z][A-Za-z0-9_-]*\\s*\\(", rule))[[1]]
  if (length(calls) == 0) return("ok")
  func_names <- tolower(sub("\\s*\\($", "", calls))
  supported <- c("selected", "not")
  if (any(!(func_names %in% supported))) return("unsupported_function")

  "ok"
}

# Escape POSIX/PCRE regex metacharacters in a literal string so it can be
# safely interpolated into a larger regex pattern.
.escape_regex_literal <- function(s) {
  gsub("([.\\\\|()\\[\\]{}^$*+?])", "\\\\\\1", s, perl = TRUE)
}

# Render an R string literal that is guaranteed to parse, no matter what
# quotes/backslashes the value contains. deparse1() handles all the edge
# cases (apostrophes, embedded backslashes, etc.).
.r_string_literal <- function(s) {
  deparse1(as.character(s))
}

# Walk a partially-converted expression and rewrite every quoted XLSForm
# string literal (single or double quoted) into a canonical R literal via
# .r_string_literal(). This is what fixes things like
#   selected(${q}, "won't")  -- the embedded apostrophe.
.normalize_string_literals <- function(s) {
  re <- "\"[^\"]*\"|'[^']*'"
  m <- gregexpr(re, s, perl = TRUE)[[1]]
  if (length(m) == 1 && m[1] == -1) return(s)
  lengths <- attr(m, "match.length")
  out <- ""
  pos <- 1L
  for (i in seq_along(m)) {
    start <- m[i]
    len   <- lengths[i]
    if (start > pos) out <- paste0(out, substr(s, pos, start - 1L))
    tok <- substr(s, start, start + len - 1L)
    raw <- substr(tok, 2L, nchar(tok) - 1L)
    out <- paste0(out, .r_string_literal(raw))
    pos <- start + len
  }
  if (pos <= nchar(s)) out <- paste0(out, substr(s, pos, nchar(s)))
  out
}

# Expand SELECTED placeholders left behind by convert_relevancy_to_R().
# Each placeholder carries the question name + the *original* quoted choice
# literal (with surrounding quotes preserved). For select_multiple fields we
# emit grepl(); otherwise we emit == comparison after as.character() coercion.
.expand_selected_placeholders <- function(rule, field_types) {
  re <- "<<(NOTSEL|SEL)::([^:]+)::(\"[^\"]*\"|'[^']*')>>"
  repeat {
    m <- regexec(re, rule, perl = TRUE)
    parts <- regmatches(rule, m)[[1]]
    if (length(parts) == 0) break
    negate <- parts[2] == "NOTSEL"
    qname  <- parts[3]
    quoted <- parts[4]
    raw    <- substr(quoted, 2L, nchar(quoted) - 1L)

    ftype <- if (qname %in% names(field_types)) field_types[[qname]] else NA_character_
    is_multi <- !is.na(ftype) && ftype == "select_multiple"

    if (is_multi) {
      pattern <- paste0("\\b", .escape_regex_literal(raw), "\\b")
      repl <- sprintf("grepl(%s, data$%s)",
                      .r_string_literal(pattern), qname)
    } else {
      repl <- sprintf("as.character(data$%s) == %s",
                      qname, .r_string_literal(raw))
    }
    if (negate) repl <- paste0("!(", repl, ")")

    # Replace just the FIRST occurrence (fixed-string, no regex re-interpretation).
    rule <- sub(re, "@@PLACEHOLDER@@", rule, perl = TRUE)
    rule <- sub("@@PLACEHOLDER@@", repl, rule, fixed = TRUE)
  }
  rule
}

