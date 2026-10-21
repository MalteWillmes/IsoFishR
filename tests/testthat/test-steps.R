test_that("find_runfiles searches subfolders by the preset extension and flags duplicates", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "day1"))
  dir.create(file.path(dir, "day2", "late"), recursive = TRUE)
  for (f in c("day1/a.run", "day1/b.RUN", "day2/late/c.run", "day2/a.run", "day2/notes.txt")) {
    writeLines("x", file.path(dir, f))
  }
  found <- find_runfiles(dir)
  expect_setequal(found$file, c("a.run", "b.RUN", "c.run", "a.run"))
  expect_equal(found$duplicate[found$sample_id == "a"], c(TRUE, TRUE))
  expect_false(any(found$duplicate[found$sample_id != "a"]))
  expect_true("day2/late" %in% found$folder)

  imp <- default_settings()$import
  imp$extension <- "txt"
  expect_equal(find_runfiles(dir, imp)$file, "notes.txt")
})

test_that("the last step is remembered in the project folder", {
  p <- create_project(file.path(withr::local_tempdir(), "P"), user = "tester")
  on.exit(close_project(p))
  expect_null(read_last_step(p))
  write_last_step(p, "calibration")
  expect_equal(read_last_step(p), "calibration")
  expect_false("app_state" %in% read_log(p)$action)  # navigation is not a project change
  ro <- open_project(p$path, user = "x", read_only = TRUE)
  expect_false(write_last_step(ro, "export"))
  expect_equal(read_last_step(p), "calibration")
})

test_that("export writes only the chosen files", {
  p <- add_runfiles(create_project(file.path(withr::local_tempdir(), "P"), user = "tester"),
                    example_runfiles()[1:2])
  on.exit(close_project(p))
  dir <- export_project(p, files = c("samples", "profiles"))
  expect_setequal(list.files(dir), c("samples.csv", "profiles.csv", "export_report.md"))
  expect_error(export_project(p, files = "nonsense"))
})

test_that("exports and replaced files within the same second get unique names", {
  p <- add_runfiles(create_project(file.path(withr::local_tempdir(), "P"), user = "tester"),
                    example_runfiles()[1])
  on.exit(close_project(p))
  d1 <- export_project(p, files = "samples")
  d2 <- export_project(p, files = "samples")
  expect_false(d1 == d2)
  expect_true(all(file.exists(file.path(c(d1, d2), "samples.csv"))))

  # Replace the same run twice in quick succession
  dir <- withr::local_tempdir()
  f <- file.path(dir, basename(example_runfiles()[1]))
  for (k in 1:2) {
    file.copy(example_runfiles()[1], f, overwrite = TRUE)
    cat(rep("\n", k), file = f, append = TRUE)
    p <- suppressMessages(add_runfiles(p, f, mode = "replace"))
  }
  expect_length(list.files(file.path(p$path, "raw", "removed")), 2)
  expect_equal(p$samples$md5, unname(tools::md5sum(f)))
})

test_that("custom 87Sr/86Sr lines are parsed and kept with the last step", {
  expect_equal(parse_ratio_lines("0.7105, 0.7092;0.7092 abc 5"), c(0.7092, 0.7105))
  expect_equal(parse_ratio_lines(""), numeric())
  p <- create_project(file.path(withr::local_tempdir(), "P"), user = "tester")
  p <- suppressMessages(add_runfiles(p, example_runfiles()[1]))
  write_last_step(p, "workspace")
  write_app_state(p, ratio_lines = c(0.7092, 0.7105))
  expect_equal(read_last_step(p), "workspace")
  expect_equal(unlist(read_app_state(p)$ratio_lines), c(0.7092, 0.7105))
  d <- project_data(p, p$samples$sample_id[1])
  b <- profile_plot(d, lines = c(0.7092, 0.7105))
  expect_true(all(c("0.70920", "0.71050") %in% vapply(b$x$data, \(t) t$name %||% "", "")))
  expect_true(b$x$config$scrollZoom)
})

test_that("reduction progress counts only the runs that are not cached", {
  p <- create_project(file.path(withr::local_tempdir(), "P"), user = "tester")
  p <- suppressMessages(add_runfiles(p, example_runfiles()[1:3]))
  calls <- list()
  progress_env$hook <- function(done, total, id) calls[[length(calls) + 1]] <<- c(done, total)
  withr::defer(progress_env$hook <- NULL)

  ids <- p$samples$sample_id
  project_data(p, ids[1])
  expect_equal(calls, list(c(1L, 1L)))
  calls <- list()
  project_data(p, ids)                      # the first is cached now
  expect_equal(calls, list(c(1L, 2L), c(2L, 2L)))
  calls <- list()
  project_data(p, ids)                      # all cached: no progress
  expect_length(calls, 0)
})
