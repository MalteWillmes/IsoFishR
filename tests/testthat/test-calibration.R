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
  expect_equal(s$sd2[s$reference_material == "MMC"], 2 * stats::sd(r_mmc))
  expect_false(is.na(s$sd2[s$reference_material == "MMC"]))
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
