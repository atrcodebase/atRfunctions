#' Scaffold a new targets-based survey data-processing project
#'
#' Emits a runnable project directory that consumes the `atRfunctions`
#' helpers via a [`targets`](https://docs.ropensci.org/targets/) pipeline.
#' The generated tree includes:
#'
#' - `_targets.R` - pipeline definition.
#' - `config/project.yml` - tools, logs, toggles.
#' - `config/columns.yml` - drop / pii / custom_labels.
#' - `R/log_io.R`, `R/stages.R`, `R/custom_checks.R` - helper functions.
#' - `.Renviron.example` - template with `ATRP_<LOG>_URL` / `_GID` slots.
#' - `.gitignore`, `README.md`, `<project_name>.Rproj`.
#' - Empty `input/tools/`, `input/data/`, `output/{analyst,client,issues}/`.
#'
#' Workflow after scaffolding:
#'
#' 1. Drop the XLSForm(s) under `input/tools/` and raw CSV(s) under `input/data/`.
#' 2. Edit `config/project.yml` to point at them.
#' 3. Copy `.Renviron.example` to `.Renviron` and fill in log URLs.
#' 4. Run `targets::tar_make()`.
#'
#' @section Generated-project dependencies:
#' The scaffolded project (not `atRfunctions` itself) needs these packages
#' installed before `tar_make()` runs:
#' \itemize{
#'   \item `targets`, `tarchetypes` - pipeline orchestration
#'   \item `yaml` - reading `config/*.yml`
#'   \item `readr` - reading raw CSV inputs and pub-CSV log URLs
#'   \item `writexl` - writing the consolidated issues workbook
#'   \item `googlesheets4` - only required if any log URL is a full
#'     `/spreadsheets/d/<ID>/...` URL; not needed for pub-CSV URLs
#' }
#'
#' ```r
#' install.packages(c("targets", "tarchetypes", "yaml", "readr",
#'                    "writexl", "googlesheets4"))
#' ```
#'
#' @param path Destination directory. Created if it does not exist.
#' @param project_name Project name used inside `project.yml`, `README.md`,
#'   and the generated `.Rproj` filename. Defaults to `basename(path)`.
#' @param overwrite If `FALSE` (the default) and `path` is non-empty, error
#'   out. If `TRUE`, write into the existing directory, overwriting any
#'   colliding files.
#'
#' @return The (normalized) path to the scaffolded project, invisibly.
#' @export
scaffold_pipeline_project <- function(path,
                                      project_name = NULL,
                                      overwrite = FALSE) {
  if (missing(path) || !is.character(path) || length(path) != 1L) {
    stop("`path` must be a single string.", call. = FALSE)
  }
  if (is.null(project_name)) project_name <- basename(normalizePath(path, mustWork = FALSE))
  if (!nzchar(project_name)) project_name <- "pipeline_project"

  template_root <- system.file("templates", "pipeline_project",
                               package = "atRfunctions")
  if (template_root == "" || !dir.exists(template_root)) {
    stop("Could not locate the pipeline_project template directory inside ",
         "the installed atRfunctions package. Re-install the package.",
         call. = FALSE)
  }

  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  existing <- list.files(path, all.files = TRUE, no.. = TRUE)
  if (length(existing) > 0 && !overwrite) {
    stop(sprintf(
      "Destination `%s` is not empty. Pass `overwrite = TRUE` to scaffold into it anyway.",
      path), call. = FALSE)
  }

  # Walk the template tree and emit each file.
  files <- list.files(template_root, recursive = TRUE, all.files = TRUE,
                      no.. = TRUE)
  for (rel in files) {
    src  <- file.path(template_root, rel)
    dest <- file.path(path, .map_template_path(rel, project_name))
    dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
    .copy_template_file(src, dest, project_name)
  }

  # Create the directories that hold no template files (so the user has a
  # place to drop XLSForms / raw data and so `targets` finds the output
  # folders ready).
  for (sub in c("input/tools", "input/data",
                "output/analyst", "output/client", "output/issues")) {
    dir.create(file.path(path, sub), showWarnings = FALSE, recursive = TRUE)
  }

  .scaffold_next_steps_message(path, project_name)
  invisible(normalizePath(path))
}

# Translate a source-template relative path into the destination relative
# path. Two rules:
#   - "dot-X" -> ".X" (so .Renviron.example, .gitignore aren't hidden
#     files in the installed package, which R CMD check dislikes).
#   - "project.Rproj.template" -> "<project_name>.Rproj".
.map_template_path <- function(rel, project_name) {
  parts <- strsplit(rel, "/", fixed = TRUE)[[1]]
  base  <- parts[length(parts)]
  if (startsWith(base, "dot-")) {
    base <- paste0(".", substring(base, 5))
  } else if (identical(base, "project.Rproj.template")) {
    base <- paste0(project_name, ".Rproj")
  }
  parts[length(parts)] <- base
  paste(parts, collapse = "/")
}

# Copy a single template file, substituting {{project_name}} in text files.
# Binary files (none ship today, but future-proofed) get copied verbatim.
.copy_template_file <- function(src, dest, project_name) {
  if (.is_text_file(src)) {
    txt <- readLines(src, warn = FALSE, encoding = "UTF-8")
    txt <- gsub("{{project_name}}", project_name, txt, fixed = TRUE)
    writeLines(txt, dest, useBytes = TRUE)
  } else {
    file.copy(src, dest, overwrite = TRUE)
  }
}

# Conservative heuristic: anything with a known text extension is treated
# as text. Plain extension check is good enough for the files we ship.
.is_text_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  # Files with no extension and "Rproj.template" both count as text.
  if (ext == "" || grepl("\\.Rproj\\.template$", path)) return(TRUE)
  ext %in% c("r", "yml", "yaml", "md", "txt", "example", "rproj",
             "gitignore", "renviron")
}

.scaffold_next_steps_message <- function(path, project_name) {
  message(sprintf("Created '%s' project at %s", project_name, path))
  message("")
  message("Next steps:")
  message(sprintf("  1. cd %s", path))
  message("  2. Drop XLSForm(s) under input/tools/ and raw CSV(s) under input/data/")
  message("  3. Edit config/project.yml and config/columns.yml")
  message("  4. cp .Renviron.example .Renviron   (and fill in log URLs)")
  message("  5. targets::tar_make()")
}
