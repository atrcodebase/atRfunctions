#' Set the relevancy defined for groups on the questions inside
#'
#' Walks the XLSForm `survey` sheet tracking `begin_group` / `begin_repeat`
#' nesting. For every non-group question, the function combines the open
#' group relevancies (joined by ` and `) with the question's own relevancy
#' (also joined by ` and `). Operands containing `or` are wrapped in
#' parentheses to preserve XPath precedence - without this, a group
#' relevancy `${a}=1` combined with a question relevancy `${b}=1 or ${b}=2`
#' would be silently re-parsed as `(${a}=1 and ${b}=1) or ${b}=2`, which is
#' not what the form author wrote.
#'
#' Accepts a pre-read XLSForm `survey` data frame (Kobo or SurveyCTO). Column
#' name differences (`relevant`/`relevance`) are normalized internally.
#'
#' @param tool the XLSForm `survey` sheet (data frame).
#' @param ... For backwards compatibility: the previous argument `kobo_tool`
#'   is still accepted as a deprecated alias for `tool`.
#'
#' @return The input tool with `relevant` column updated to carry the merged
#'   group + question relevancy for each row.
#' @export
process_group_relevancies <- function(tool = NULL, ...) {
  tool <- .deprecated_arg(tool, list(...), new_name = "tool", old_name = "kobo_tool")
  if (is.null(tool)) stop("`tool` is required.", call. = FALSE)

  tool <- .normalize_survey(tool)

  begin_group_relevancies <- c()

  process_question <- function(question_type, relevancy) {

    if (grepl("^[Bb]egin[_]?", question_type)) {
      begin_group_relevancies <<- c(begin_group_relevancies, relevancy)
    } else if (grepl("^[Ee]nd[_]?", question_type) & length(begin_group_relevancies) > 0) {
      begin_group_relevancies <<- head(begin_group_relevancies, n = -1)
    }

    if (!grepl("^([Bb]egin|[Ee]nd)_?", question_type)) {
      # Build the combined relevancy by AND-joining: open group relevancies,
      # then the question's own. Each operand that itself contains a top-level
      # `or` is wrapped in parens to preserve precedence.
      group_parts <- unique(begin_group_relevancies[!is.na(begin_group_relevancies)])
      operands <- c(group_parts, if (!is.na(relevancy)) relevancy)

      if (length(operands) == 0) return(NA_character_)

      wrapped <- vapply(operands, .paren_if_or, character(1), USE.NAMES = FALSE)
      # Drop duplicate operands that occur side-by-side (the common pattern
      # produced by group relevancies being repeated). We dedup only at the
      # top AND level - never across `or` boundaries, which would silently
      # change the form's logic.
      wrapped <- .dedup_top_and(wrapped)
      merged <- paste(wrapped, collapse = " and ")
      gsub(" +", " ", merged)
    } else {
      return(relevancy)
    }
  }
  tool$relevant <- mapply(process_question, tool$type, tool$relevant, SIMPLIFY = TRUE)
  tool
}

# Wrap an expression in parentheses if it contains a top-level `or`. Cheap
# heuristic: if " or " appears anywhere, wrap; we accept the occasional
# unnecessary paren in exchange for never breaking precedence.
.paren_if_or <- function(expr) {
  if (is.na(expr) || expr == "") return(expr)
  if (grepl(" or ", expr, fixed = TRUE)) paste0("(", expr, ")") else expr
}

# Remove adjacent duplicate operands from a vector of AND-joined operands.
# We deliberately do NOT dedup across the whole vector - the same condition
# appearing in two different AND chains is rare and worth preserving rather
# than risk altering the logic.
.dedup_top_and <- function(operands) {
  if (length(operands) <= 1) return(operands)
  keep <- rep(TRUE, length(operands))
  for (i in seq_along(operands)[-1]) {
    if (identical(operands[i], operands[i - 1])) keep[i] <- FALSE
  }
  operands[keep]
}
