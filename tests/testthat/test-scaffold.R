test_that("scaffold_pipeline_project writes the expected file tree", {
  dest <- file.path(tempdir(), paste0("scaffold-", as.integer(Sys.time())))
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  expect_message(
    scaffold_pipeline_project(dest, project_name = "Test Project"),
    "Created 'Test Project' project"
  )

  expected <- c(
    "_targets.R",
    "config/project.yml",
    "config/columns.yml",
    "R/log_io.R",
    "R/stages.R",
    "R/custom_checks.R",
    ".Renviron.example",
    ".gitignore",
    "README.md",
    "Test Project.Rproj"
  )
  for (f in expected) {
    expect_true(file.exists(file.path(dest, f)),
                info = paste("missing file:", f))
  }

  # Empty user-supplied directories
  for (d in c("input/tools", "input/data",
              "output/analyst", "output/client", "output/issues")) {
    expect_true(dir.exists(file.path(dest, d)), info = paste("missing dir:", d))
  }
})

test_that("scaffold output files all parse as R / YAML", {
  skip_if_not_installed("yaml")
  dest <- file.path(tempdir(), paste0("scaffold-parse-", as.integer(Sys.time())))
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)
  suppressMessages(scaffold_pipeline_project(dest, project_name = "Parse Test"))

  # R files must parse
  for (f in c("_targets.R", "R/log_io.R", "R/stages.R", "R/custom_checks.R")) {
    expect_silent(parse(file = file.path(dest, f)))
  }

  # YAML files must parse and contain the documented top-level keys
  project <- yaml::read_yaml(file.path(dest, "config/project.yml"))
  expect_named(project,
               c("project_name", "tools", "logs", "log_env_prefix",
                 "always_refetch_logs", "rejection"),
               ignore.order = TRUE)
  expect_equal(project$project_name, "Parse Test")
  expect_true("qa" %in% project$logs)
  expect_true(isTRUE(project$always_refetch_logs))

  columns <- yaml::read_yaml(file.path(dest, "config/columns.yml"))
  expect_true("drop_columns" %in% names(columns))
  expect_true("pii_columns"  %in% names(columns))
  expect_true("custom_labels" %in% names(columns))
})

test_that("scaffold_pipeline_project errors on a non-empty destination unless overwrite=TRUE", {
  dest <- file.path(tempdir(), paste0("scaffold-overwrite-", as.integer(Sys.time())))
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)
  suppressMessages(scaffold_pipeline_project(dest))

  expect_error(scaffold_pipeline_project(dest), "not empty")
  expect_message(scaffold_pipeline_project(dest, overwrite = TRUE),
                 "Created")
})

test_that("scaffold_pipeline_project substitutes {{project_name}} consistently", {
  dest <- file.path(tempdir(), paste0("scaffold-sub-", as.integer(Sys.time())))
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)
  suppressMessages(scaffold_pipeline_project(dest, project_name = "My Survey"))

  yml <- readLines(file.path(dest, "config/project.yml"))
  expect_true(any(grepl('project_name: "My Survey"', yml, fixed = TRUE)))

  rm <- readLines(file.path(dest, "README.md"))
  expect_true(any(grepl("# My Survey", rm, fixed = TRUE)))
  # The placeholder must be fully replaced - no stray {{...}} anywhere.
  for (f in c("config/project.yml", "config/columns.yml", "README.md",
              "My Survey.Rproj")) {
    contents <- readLines(file.path(dest, f), warn = FALSE)
    expect_false(any(grepl("{{project_name}}", contents, fixed = TRUE)),
                 info = paste("placeholder remains in", f))
  }
})

test_that(".map_template_path renames dot- prefix and project.Rproj.template", {
  m <- atRfunctions:::.map_template_path
  expect_equal(m("dot-Renviron.example", "Foo"), ".Renviron.example")
  expect_equal(m("dot-gitignore", "Foo"), ".gitignore")
  expect_equal(m("project.Rproj.template", "Foo"), "Foo.Rproj")
  expect_equal(m("config/project.yml", "Foo"), "config/project.yml")
  expect_equal(m("R/stages.R", "Foo"), "R/stages.R")
})
