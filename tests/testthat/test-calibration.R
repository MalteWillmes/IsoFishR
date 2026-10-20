PRIMARY <- 0.70918
SECONDARY <- 0.71030

# Instrument drift: measured = true * (1 + k * minutes)
drifted <- function(true, minute, k = 1e-5) true * (1 + k * minute)

bracketed_project <- function(env = parent.frame()) {
  runs <- data.frame(
    id = c("RM1", "fish1", "SEC1", "RM2", "fish2", "RM3"),
    minute = c(0, 15, 25, 40, 55, 70),
    true = c(PRIMARY, 0.71000, SECONDARY, PRIMARY, 0.70500, PRIMARY)
  )
  runs$ratio <- drifted(runs$true, runs$minute)
  p <- synthetic_project(runs, env)
  p <- set_reference_material(p, "MMC", PRIMARY, 0.00002, "test primary")
  p <- set_reference_material(p, "SEC", SECONDARY, 0.00004, "test secondary")
  p <- set_run_type(p, c("RM1", "RM2", "RM3"), "reference", "MMC")
  p <- set_run_type(p, "SEC1", "reference", "SEC")
  update_project_settings(p, list(calibration = list(primary = "MMC")))
}

run_means <- function(d) c(tapply(d$sr87_sr86, d$sample_id, mean, na.rm = TRUE))

test_that("acquisition times are read from runfile headers and order the runs", {
  p <- bracketed_project()
  runs <- project_runs(p)
  expect_equal(runs$sample_id, c("RM1", "fish1", "SEC1", "RM2", "fish2", "RM3"))
  expect_equal(runs$acquired[1], t0)
  expect_equal(unique(runs$session), "S1 2024-05-02")
  expect_equal(runs$rm_role, c("primary", "", "secondary", "primary", "", "primary"))
})

test_that("the primary reference material removes linear drift (linear fit)", {
  p <- bracketed_project()
  d <- project_data(p)
  m <- run_means(d)
  expect_equal(unname(m["fish1"]), 0.71000, tolerance = 1e-7)
  expect_equal(unname(m["fish2"]), 0.70500, tolerance = 1e-7)
  expect_equal(unname(m["RM2"]), PRIMARY, tolerance = 1e-7)
  # Uncorrected values keep the drift
  u <- tapply(d$sr87_sr86_uncorrected, d$sample_id, mean)
  expect_equal(unname(u["fish2"]), drifted(0.70500, 55), tolerance = 1e-9)
  expect_true(all(d$correction_status == "corrected"))
})

test_that("without a primary reference material nothing is corrected", {
  p <- bracketed_project()
  p <- update_project_settings(p, list(calibration = list(primary = "")))
  d <- project_data(p)
  expect_false("correction_factor" %in% names(d))
  expect_equal(unname(run_means(d)["fish2"]), drifted(0.70500, 55), tolerance = 1e-9)
})

test_that("secondary reference materials report their accuracy", {
  p <- bracketed_project()
  r <- reference_material_results(p)
  sec <- r[r$sample_id == "SEC1", ]
  expect_equal(sec$role, "secondary")
  expect_equal(sec$measured, drifted(SECONDARY, 25), tolerance = 1e-9)
  expect_lt(abs(sec$deviation_ppm), 0.5)
  expect_true(sec$within_uncertainty)
  expect_gt(sec$se_combined, sec$se)  # the correction's uncertainty is added

  s <- summarise_reference_materials(p)
  expect_equal(s$runs[s$reference_material == "MMC"], 3)
  r_mmc <- r$mean[r$reference_material == "MMC"]
  expect_equal(s$sd_corrected[s$reference_material == "MMC"], stats::sd(r_mmc))
  expect_false(is.na(s$sd_uncorrected[s$reference_material == "MMC"]))
  expect_equal(s$mean_uncorrected[s$reference_material == "SEC"], drifted(SECONDARY, 25), tolerance = 1e-9)
  expect_equal(s$role[s$reference_material == "SEC"], "secondary")
})

test_that("the fit methods behave as documented", {
  p <- bracketed_project()
  model <- calibration_model(p)
  expect_equal(nrow(model$points), 3)
  f <- model$points$factor
  t <- model$points$time
  mid <- mean(t[1:2])

  model$fit <- "step"
  expect_equal(correction_at(model, "S1 2024-05-02", mid)$factor, f[1])
  model$fit <- "mean"
  expect_equal(correction_at(model, "S1 2024-05-02", mid)$factor, mean(f))
  model$fit <- "linear"
  expect_equal(correction_at(model, "S1 2024-05-02", mid)$factor, mean(f[1:2]))
  # Before the first and after the last primary run the factor is held
  expect_equal(correction_at(model, "S1 2024-05-02", t[1] - 600)$factor, f[1])
  expect_equal(correction_at(model, "S1 2024-05-02", t[3] + 600)$factor, f[3])
  model$fit <- "spline"
  expect_match(correction_at(model, "S1 2024-05-02", mid)$status, "spline needs 4")
})

test_that("corrections never cross sessions", {
  runs <- data.frame(id = c("RM1", "fish1", "fishNextDay"), minute = c(0, 20, 60 * 24),
                     ratio = c(PRIMARY * 1.001, 0.71, 0.71))
  p <- synthetic_project(runs)
  p <- set_reference_material(p, "MMC", PRIMARY)
  p <- set_run_type(p, "RM1", "reference", "MMC")
  p <- update_project_settings(p, list(calibration = list(primary = "MMC")))
  runs <- project_runs(p)
  expect_equal(length(unique(runs$session)), 2)

  d <- project_data(p)
  st <- tapply(d$correction_status, d$sample_id, unique)
  expect_equal(unname(st["fish1"]), "corrected")
  expect_equal(unname(st["fishNextDay"]), "no primary reference material in session")
  expect_equal(unname(run_means(d)["fishNextDay"]), 0.71, tolerance = 1e-9)

  # Moving the run into the first session makes it corrected
  p <- set_session(p, "fishNextDay", runs$session[1])
  d <- project_data(p)
  expect_equal(unique(d$correction_status[d$sample_id == "fishNextDay"]), "corrected")
  expect_equal(read_log(p)$new[read_log(p)$action == "session"], runs$session[1])
})

test_that("excluded primary runs are not used", {
  p <- bracketed_project()
  p <- update_sample(p, "RM2", status = "excluded")
  expect_equal(calibration_model(p)$points$sample_id, c("RM1", "RM3"))
})

test_that("baseline runs interpolated in time replace each run's own blank", {
  # Blank rises from 0.01 V to 0.03 V; the samples carry the interpolated
  # blank but no blank period of their own.
  runs <- data.frame(id = c("B1", "fish1", "B2"), minute = c(0, 30, 60),
                     ratio = 0.7092, blank_v = c(0.01, 0.02, 0.03), blank_s = c(0, 0, 0))
  p <- synthetic_project(runs)
  p <- set_run_type(p, c("B1", "B2"), "baseline")

  b <- project_baselines(p)
  expect_equal(b$blk88, c(4.01, 4.03), tolerance = 1e-9)  # whole baseline run averaged

  # Our synthetic baseline runs carry a 4 V signal; replace with a blank-only
  # file to model a real gas blank.
  write_runfile(file.path(p$path, p$samples$runfile[p$samples$sample_id == "B1"]), at_min(0),
                raw88 = 0, blank_v = 0.01)
  write_runfile(file.path(p$path, p$samples$runfile[p$samples$sample_id == "B2"]), at_min(60),
                raw88 = 0, blank_v = 0.03)
  b <- project_baselines(p)
  expect_equal(b$blk88, c(0.01, 0.03), tolerance = 1e-9)

  p <- update_project_settings(p, list(calibration = list(baseline = "baseline_runs")))
  d <- project_data(p)
  expect_equal(unique(d$sample_id), "fish1")  # baseline runs are not data
  expect_equal(unname(run_means(d)), 0.7092, tolerance = 1e-9)

  # With only per-run blanks (and none in the file) the result is biased
  p <- update_project_settings(p, list(calibration = list(baseline = "per_run")))
  biased <- suppressWarnings(project_data(p))
  expect_gt(abs(mean(biased$sr87_sr86) - 0.7092), 1e-5)
})

test_that("reference materials are validated and logged", {
  p <- bracketed_project()
  expect_error(set_run_type(p, "fish1", "reference", "NOPE"), "Unknown reference material")
  expect_error(remove_reference_material(p, "MMC"), "assigned")
  expect_error(update_project_settings(p, list(calibration = list(primary = "NOPE"))),
               "not defined")
  expect_error(update_project_settings(p, list(calibration = list(fit = "cubic"))), "fit")
  p <- set_reference_material(p, "MMC", 0.709175, 0.00002, "updated")
  log <- dplyr::filter(read_log(p), action == "reference_material", field == "MMC")
  expect_equal(nrow(log), 2)
  expect_match(log$new[2], "0.709175")
  rt <- dplyr::filter(read_log(p), action == "run_type")
  expect_setequal(rt$sample_id, c("RM1", "RM2", "RM3", "SEC1"))
})

test_that("exports carry corrected values, the correction and the QC tables", {
  p <- bracketed_project()
  p <- set_regions(p, "fish2", data.frame(region = "all", start_s = 0, end_s = 100))
  dir <- export_project(p)
  files <- list.files(dir)
  expect_true(all(c("runs.csv", "reference_materials.csv", "reference_material_summary.csv")
                  %in% files))
  read <- function(f) readr::read_csv(file.path(dir, f), show_col_types = FALSE, guess_max = Inf)

  s <- read("samples.csv")
  fish2 <- s[s$sample_id == "fish2", ]
  expect_equal(fish2$mean, 0.705, tolerance = 1e-6)
  expect_equal(fish2$mean_uncorrected, drifted(0.705, 55), tolerance = 1e-6)
  expect_equal(fish2$correction_status, "corrected")
  expect_gt(fish2$se_combined, fish2$se)
  expect_equal(fish2$session, "S1 2024-05-02")
  expect_equal(s$run_type[s$sample_id == "RM1"], "reference")

  r <- read("regions.csv")
  expect_equal(r$mean, 0.705, tolerance = 1e-6)
  expect_true(all(c("mean_uncorrected", "se_combined", "correction_factor") %in% names(r)))

  pr <- read("profiles.csv")
  expect_true(all(c("sr87_sr86_uncorrected", "correction_factor") %in% names(pr)))

  runs <- read("runs.csv")
  expect_equal(runs$sample_id, c("RM1", "fish1", "SEC1", "RM2", "fish2", "RM3"))

  qc <- read("reference_materials.csv")
  expect_true(qc$within_uncertainty[qc$sample_id == "SEC1"])
  settings <- read("settings.csv")
  expect_equal(unique(settings$calibration.primary), "MMC")
})

test_that("a project without runs works with the calibration functions", {
  # Regression: project_runs() failed on an empty project, which crashed the
  # app right after creating a project.
  path <- file.path(withr::local_tempdir(), "Empty")
  p <- create_project(path, user = "tester")
  on.exit(close_project(p))
  runs <- project_runs(p)
  expect_equal(nrow(runs), 0)
  expect_type(runs$session, "character")
  expect_equal(nrow(project_baselines(p)), 0)
  expect_equal(nrow(reference_material_results(p)), 0)
  p <- set_reference_material(p, "MMC", 0.70918)
  p <- update_project_settings(p, list(calibration = list(primary = "MMC", baseline = "baseline_runs")))
  expect_equal(nrow(calibration_model(p)$points), 0)
  expect_equal(nrow(project_runs(p)), 0)
})

test_that("the default regression fit follows the least-squares drift line", {
  # Primary runs with scatter around a linear drift: the regression line,
  # not the individual runs, sets the correction.
  set.seed(7)
  minutes <- c(0, 20, 40, 60, 80)
  runs <- data.frame(id = c(paste0("RM", 1:5), "fish"), minute = c(minutes, 50),
                     ratio = c(drifted(PRIMARY, minutes) + c(2, -3, 1, 3, -2) * 1e-5,
                               drifted(0.7100, 50)))
  p <- synthetic_project(runs)
  p <- set_reference_material(p, "MMC", PRIMARY)
  p <- set_run_type(p, paste0("RM", 1:5), "reference", "MMC")
  p <- update_project_settings(p, list(calibration = list(primary = "MMC")))
  expect_equal(p$settings$calibration$fit, "regression")

  model <- calibration_model(p)
  line <- stats::lm(measured ~ time, data = model$points)
  t_fish <- as.numeric(at_min(50)) + 35
  expected_factor <- PRIMARY / stats::predict(line, data.frame(time = t_fish))
  expect_equal(correction_at(model, model$points$session[1], t_fish)$factor,
               unname(expected_factor), tolerance = 1e-9)
  # The regression removes the drift on the sample within the scatter
  d <- project_data(p, "fish")
  expect_equal(mean(d$sr87_sr86), 0.7100, tolerance = 2e-5)
  # The model is cached: a second call returns the identical object quickly
  expect_identical(calibration_model(p), model)
})

test_that("SBO runs are suggested as the primary reference material with 0.70918", {
  runs <- data.frame(id = c("SBO_1", "fish1", "sbo_2", "fish2"), minute = c(0, 10, 20, 30),
                     ratio = c(0.70918, 0.71, 0.70918, 0.71))
  p <- synthetic_project(runs)
  expect_setequal(suggest_reference_runs(p), c("SBO_1", "sbo_2"))
  p <- apply_reference_suggestion(p)
  expect_equal(p$reference_materials$value[p$reference_materials$name == "SBO"], 0.70918)
  expect_equal(p$settings$calibration$primary, "SBO")
  expect_equal(p$samples$run_type, c("reference", "sample", "reference", "sample"))
  expect_length(suggest_reference_runs(p), 0)

  # A user-changed value is kept when the suggestion is applied again
  p <- set_reference_material(p, "SBO", 0.70917)
  p <- apply_reference_suggestion(p)
  expect_equal(p$reference_materials$value, 0.70917)
})

test_that("GSFR runs are suggested too, as a secondary reference material", {
  runs <- data.frame(id = c("SBO_1", "GSFR_1", "fish1", "gsfr_2"), minute = c(0, 10, 20, 30),
                     ratio = c(0.70918, 0.70918, 0.71, 0.70918))
  p <- synthetic_project(runs)
  found <- reference_suggestions(p)
  expect_equal(names(found), c("SBO", "GSFR"))
  expect_setequal(found$GSFR, c("GSFR_1", "gsfr_2"))
  p <- apply_reference_suggestion(p, "SBO")
  p <- apply_reference_suggestion(p, "GSFR")
  expect_equal(p$reference_materials$value[p$reference_materials$name == "GSFR"], 0.70918)
  expect_equal(p$settings$calibration$primary, "SBO")
  expect_equal(p$samples$reference_material, c("SBO", "GSFR", "", "GSFR"))
  expect_length(reference_suggestions(p), 0)
})

test_that("the export record documents settings, calibration and reference materials", {
  p <- bracketed_project()
  p <- set_sample_settings(p, "fish1", ma_window = 15)
  info <- export_report(p, files = "samples.csv", n_samples = 2)

  expect_equal(info$files, c("samples.csv", "export_report.md"))
  expect_equal(info$runs$reference_material_runs, 4)
  expect_equal(info$sessions[[1]]$runs, 6)

  cal <- info$calibration
  expect_equal(cal$correction, "on")
  expect_equal(cal$method, "regression")
  expect_match(cal$method_description, "regression")
  expect_equal(cal$baseline, "per_run")
  expect_equal(cal$primary$name, "MMC")
  expect_equal(cal$primary$accepted, PRIMARY)
  expect_equal(cal$primary$runs, 3)
  expect_equal(cal$primary$mean_corrected, PRIMARY, tolerance = 1e-6)
  expect_gt(cal$primary$sd_uncorrected, cal$primary$sd_corrected)
  expect_equal(cal$primary$per_session[[1]]$runs, 3)
  expect_length(cal$secondary, 1)
  expect_equal(cal$secondary[[1]]$name, "SEC")
  expect_equal(cal$secondary[[1]]$runs, 1)
  expect_true(is.na(cal$secondary[[1]]$sd_corrected))  # one run: no SD

  expect_equal(info$settings$reduction$integration, 1)
  expect_equal(info$settings$import$preset, "Nu_UCDavis")
  expect_equal(info$sample_setting_overrides$fish1, "ma_window=15")
})

test_that("export_report.md presents the record as readable Markdown", {
  p <- bracketed_project()
  p <- set_sample_settings(p, "fish1", ma_window = 15)
  dir <- export_project(p, files = "samples")
  expect_setequal(list.files(dir), c("samples.csv", "export_report.md"))
  md <- readLines(file.path(dir, "export_report.md"), encoding = "UTF-8")

  expect_match(md[1], "^# IsoFishR export: P$")
  expect_true(all(c("## Runs and sessions", "## Calibration", "### Reference materials",
                    "### Per session", "## Settings", "### Data reduction",
                    "## Samples with their own settings") %in% md))
  expect_true(any(grepl("**Method:** Linear regression through the standards of each session (`regression`)",
                        md, fixed = TRUE)))
  expect_true(any(grepl("**Primary reference material:** MMC", md, fixed = TRUE)))
  expect_true(any(grepl("**Secondary reference materials:** SEC", md, fixed = TRUE)))
  # Reference material rows: name, role, accepted, 2s, runs, ...
  expect_true(any(grepl("^\\| MMC \\| primary \\| 0\\.709180 \\| 2e-05 \\| 3 \\|", md)))
  expect_true(any(grepl("^\\| SEC \\| secondary \\| 0\\.710300 \\| 4e-05 \\| 1 \\|", md)))
  expect_true(any(grepl("| Integration (cycles per point) | 1 |", md, fixed = TRUE)))
  expect_true(any(grepl("| fish1 | ma_window=15 |", md, fixed = TRUE)))
  # Every table row has the same number of cells as its header
  tables <- split(md[grepl("^\\|", md)], cumsum(!grepl("^\\|", md))[grepl("^\\|", md)])
  for (tb in tables) {
    cells <- lengths(regmatches(tb, gregexpr("(?<!\\\\)\\|", tb, perl = TRUE)))
    expect_equal(length(unique(cells)), 1)
  }
})

test_that("the export record says when there is no calibration", {
  runs <- data.frame(id = c("a", "b"), minute = c(0, 10), ratio = 0.71)
  p <- synthetic_project(runs)
  info <- export_report(p)
  expect_match(info$calibration$correction, "off")
  expect_null(info$calibration$primary)
  expect_length(info$calibration$secondary, 0)
  md <- report_markdown(info)
  expect_true(any(grepl("No reference materials are defined.", md, fixed = TRUE)))
  expect_true(any(grepl("**Primary reference material:** none", md, fixed = TRUE)))
  expect_true(any(grepl("None: all samples use the project settings.", md, fixed = TRUE)))
})
