# Regression tests for problems found in the October 2026 review of the
# calculations and file handling.

fresh_project <- function(env = parent.frame()) {
  p <- create_project(file.path(withr::local_tempdir(.local_envir = env), "P"), user = "tester")
  withr::defer(close_project(p), envir = env)
  p
}
fish <- function(n) example_runfiles()[grepl("PC_2016", example_runfiles())][seq_len(n)]

# ---- calibration --------------------------------------------------------------

test_that("with the step fit every primary run is corrected by its own factor", {
  runs <- data.frame(id = c("RM1", "fish", "RM2", "RM3"), minute = c(0, 10, 20, 40),
                     ratio = c(0.7100, 0.7105, 0.7110, 0.7120))
  p <- synthetic_project(runs)
  p <- set_reference_material(p, "MMC", 0.7092)
  p <- set_run_type(p, c("RM1", "RM2", "RM3"), "reference", "MMC")
  p <- update_project_settings(p, list(calibration = list(primary = "MMC", fit = "step")))
  r <- reference_material_results(p)
  expect_equal(r$mean, rep(0.7092, 3), tolerance = 1e-7)
})

test_that("the calibration spline is held constant outside the standards and keeps an uncertainty", {
  model <- list(accepted = 0.70918, uncertainty_2s = NA_real_, fit = "spline",
                points = tibble::tibble(session = "S", time = c(0, 1800, 3600, 5400),
                                        factor = c(1.000, 1.001, 1.002, 1.003),
                                        u_rel = rep(1.4e-5, 4)))
  late <- correction_at(model, "S", c(5400, 5400 + 4 * 3600))
  expect_equal(late$factor[2], late$factor[1])
  expect_gt(min(late$u_rel), 1e-6)
})

test_that("a primary run without usable points is left out of the calibration", {
  runs <- data.frame(id = c("RM1", "RM2", "fish"), minute = c(0, 20, 10),
                     ratio = c(0.7100, 0.7101, 0.7105))
  p <- synthetic_project(runs)
  p <- set_reference_material(p, "MMC", 0.7092)
  p <- set_run_type(p, c("RM1", "RM2"), "reference", "MMC")
  p <- update_project_settings(p, list(calibration = list(primary = "MMC", fit = "mean")))
  # Trim everything away from RM2: it has no points left
  p <- update_sample(p, "RM2", trim_start_s = 1e6)
  m <- calibration_model(p)
  expect_equal(m$points$sample_id, "RM1")
  fish <- project_data(p, "fish")
  expect_false(anyNA(fish$correction_factor))
})

# ---- trims ------------------------------------------------------------------------

test_that("trimmed data never affects outlier rejection or smoothing", {
  raw <- read_runfiles(fish(1))
  s <- default_settings()
  d0 <- reduce_runs(raw, s)$data
  cut <- d0$profile_start_s[1] - d0$run_start_s[1] + 40   # 40 s into the profile
  trims <- tibble::tibble(sample_id = raw$sample_id[1], trim_start_s = cut, trim_end_s = 0)
  # Change the data in the part that is trimmed away (but not the blank,
  # which is used for the whole run)
  changed <- raw
  early <- changed$time_s > s$reduction$blank_time_s &
    changed$time_s < d0$profile_start_s[1] + 35   # clear of the block at the trim
  changed$raw87[early] <- changed$raw87[early] * 1.004
  a <- reduce_runs(raw, s, trims = trims)$data
  b <- reduce_runs(changed, s, trims = trims)$data
  expect_equal(b$sr87_sr86_ma, a$sr87_sr86_ma)
  expect_equal(b$sr87_sr86_spline, a$sr87_sr86_spline)
  expect_equal(b$is_outlier, a$is_outlier)
  expect_gte(min(a$time_s), d0$profile_start_s[1] + 40 - 1e-9)
})

test_that("trims stay on the same part of the sample when the thresholds change", {
  raw <- read_runfiles(fish(1))
  s <- default_settings()
  d0 <- reduce_runs(raw, s)$data
  trims <- tibble::tibble(sample_id = raw$sample_id[1],
                          trim_start_s = d0$profile_start_s[1] - d0$run_start_s[1] + 20,
                          trim_end_s = 0)
  first <- function(min_v) {
    s$reduction$raw88_min_v <- min_v
    min(reduce_runs(raw, s, trims = trims)$data$time_s)
  }
  expect_equal(first(0.5), first(1))
})

test_that("trims of format 1 projects are converted when the project is opened", {
  p <- fresh_project()
  p <- suppressMessages(add_runfiles(p, fish(2)))
  ids <- p$samples$sample_id
  b <- profile_bounds(p, ids[1])
  # Write an old project: trims counted from the first passing point
  s <- p$samples
  s$trim_start_s[1] <- 10
  s$trim_end_s[1] <- 5
  write_table_file(s, project_file(p$path, "samples.csv"))
  yml <- yaml::read_yaml(project_file(p$path, "project.yml"))
  yml$project$format <- 1L
  yaml::write_yaml(yml, project_file(p$path, "project.yml"))
  close_project(p)

  p2 <- open_project(p$path, user = "tester")
  expect_equal(p2$samples$trim_start_s[1], b$profile_start - b$run_start + 10)
  expect_equal(p2$samples$trim_end_s[1], b$run_end - b$profile_end + 5)
  expect_equal(p2$samples$trim_start_s[2], 0)
  expect_equal(yaml::read_yaml(project_file(p$path, "project.yml"))$project$format, 2L)
  expect_true("convert_trims" %in% read_log(p2)$action)
  # Same profile as before: 10 s cut from the profile start
  d <- project_data(p2, ids[1], calibrate = FALSE)
  expect_equal(d$profile_start_s[1], b$profile_start)
  expect_equal(min(d$time_s), b$profile_start + 10, tolerance = 0.6)
  close_project(p2)
  # Opening again changes nothing
  p3 <- open_project(p$path, user = "tester")
  expect_equal(p3$samples$trim_start_s, p2$samples$trim_start_s)
  close_project(p3)
})

test_that("trims shown in micrometres convert both ways", {
  tm <- list(run_start = 0, run_end = 300, profile_start = 30, profile_end = 280)
  expect_equal(trims_to_um(0, 0, tm, 10), c(start = 0, end = 0))
  s <- um_to_trims(100, "start", tm, 10)
  e <- um_to_trims(50, "end", tm, 10)
  expect_equal(c(s, e), c(40, 25))
  expect_equal(trims_to_um(s, e, tm, 10), c(start = 100, end = 50))
  expect_equal(um_to_trims(0, "start", tm, 10), 0)
})

# ---- outliers ------------------------------------------------------------------

test_that("clusters of spikes are rejected and noise mostly is not", {
  set.seed(4)
  x <- 0.709 + stats::rnorm(600, sd = 1e-4)
  x[c(100, 101, 102)] <- 0.711
  x[300] <- 0.7105
  d <- tibble::tibble(sample_id = "s", sr87_sr86 = x)
  out <- reject_outliers(d, sd = 3, window = 20)
  expect_true(all(out$is_outlier[c(100:102, 300)]))
  expect_lt(sum(out$is_outlier[-c(100:102, 300)]), 6)
  # A short window still finds a single spike
  expect_true(reject_outliers(d, sd = 3, window = 5)$is_outlier[300])
})

# ---- files ------------------------------------------------------------------------

test_that("a log that can't be written doesn't undo a saved change", {
  p <- fresh_project()
  p <- suppressMessages(add_runfiles(p, fish(1)))
  id <- p$samples$sample_id
  log <- project_file(p$path, "log.csv")
  unlink(log)
  dir.create(log)                       # can't be written to
  expect_warning(p <- update_sample(p, id, comment = "first edit"), "session log",
                 class = "isofishr_warning")
  expect_equal(p$samples$comment, "first edit")
  unlink(log, recursive = TRUE)
  p <- update_sample(p, id, flag = TRUE)
  reopened <- open_project(p$path, read_only = TRUE)
  expect_equal(reopened$samples$comment, "first edit")
})

test_that("a missing raw file doesn't stop the project from opening", {
  p <- fresh_project()
  p <- suppressMessages(add_runfiles(p, fish(2)))
  s <- p$samples
  s$acquired <- ""                      # forces the times to be read again
  write_table_file(s, project_file(p$path, "samples.csv"))
  unlink(project_file(p$path, p$samples$runfile[1]))
  close_project(p)
  expect_no_error(p2 <- open_project(p$path, user = "tester"))
  expect_equal(nrow(p2$samples), 2)
  close_project(p2)
})

test_that("a failed open releases the lock", {
  p <- fresh_project()
  close_project(p)
  writeLines("not,a,valid\"csv", project_file(p$path, "regions.csv"))
  dir.create(project_file(p$path, "samples.csv.bad"))
  unlink(project_file(p$path, "samples.csv"))
  dir.create(project_file(p$path, "samples.csv"))   # unreadable
  expect_error(open_project(p$path, user = "tester"))
  expect_false(file.exists(project_file(p$path, ".lock")))
})

test_that("run names differing only in case count as the same run", {
  p <- fresh_project()
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "a"))
  dir.create(file.path(dir, "b"))
  file.copy(fish(1), file.path(dir, "a", "Fish01.run"))
  file.copy(fish(2)[2], file.path(dir, "b", "fish01.RUN"))
  found <- find_runfiles(dir)
  expect_true(all(found$duplicate))
  p <- suppressMessages(add_runfiles(p, found$path, found$sample_id))
  expect_equal(attr(p, "import_report")$result, c("added", "skipped"))
  expect_equal(nrow(p$samples), 1)
  # And against runs already in the project
  p <- suppressMessages(add_runfiles(p, file.path(dir, "b", "fish01.RUN"), "FISH01"))
  expect_equal(attr(p, "import_report")$result, "skipped")
})

test_that("adding runfiles leaves raw/ untouched when the tables can't be saved", {
  p <- fresh_project()
  dir.create(project_file(p$path, "samples.csv.tmp"))   # makes saving fail
  expect_error(suppressMessages(add_runfiles(p, fish(2))), "No runfiles were added")
  expect_length(list.files(project_file(p$path, "raw")), 0)
  unlink(project_file(p$path, "samples.csv.tmp"), recursive = TRUE)
  p <- suppressMessages(add_runfiles(p, fish(2)))       # works on retry
  expect_equal(nrow(p$samples), 2)
})

test_that("removing samples is all or nothing", {
  p <- fresh_project()
  p <- suppressMessages(add_runfiles(p, fish(2)))
  dir.create(project_file(p$path, "samples.csv.tmp"))
  expect_error(remove_samples(p, p$samples$sample_id), "No samples were removed")
  expect_true(all(file.exists(project_file(p$path, p$samples$runfile))))
  unlink(project_file(p$path, "samples.csv.tmp"), recursive = TRUE)
  p <- remove_samples(p, p$samples$sample_id)
  expect_equal(nrow(p$samples), 0)
})

test_that("Latin-1 characters in a runfile header don't stop it being read", {
  f <- withr::local_tempfile(fileext = ".run")
  lines <- readLines(fish(1))
  lines[2] <- paste0(lines[2], " \xb5V")
  writeLines(lines, f, useBytes = TRUE)
  d <- read_runfile(f, default_settings()$import, "x")
  expect_gt(nrow(d), 1000)
  expect_false(is.na(read_acquisition_times(f)))
})

test_that("settings are saved with full precision", {
  p <- fresh_project()
  p <- update_project_settings(p, list(reduction = list(rb85_rb87 = 2.597123456)))
  close_project(p)
  p2 <- open_project(p$path, read_only = TRUE)
  expect_equal(p2$settings$reduction$rb85_rb87, 2.597123456)
})

test_that("an empty or missing run name is refused, and folder names don't depend on a trailing slash", {
  p <- fresh_project()
  p <- suppressMessages(add_runfiles(p, fish(1), sample_ids = NA_character_))
  expect_equal(attr(p, "import_report")$result, "failed")
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "day1"))
  file.copy(fish(1), file.path(dir, "day1"))
  expect_equal(find_runfiles(paste0(dir, "/"))$folder, "day1")
  expect_equal(find_runfiles(dir)$folder, "day1")
})
