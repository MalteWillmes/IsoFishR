rm_warning_project <- function(env = parent.frame()) {
  runs <- data.frame(
    id = c("RM1", "SEC1", "RM2", "RM3", "SEC2", "RM4"),
    minute = c(0, 10, 20, 30, 40, 50),
    # RM3 is a bad primary run; SEC2 is NanoSr labelled as SEC.
    ratio = c(0.70918, 0.71030, 0.70918, 0.71018, 0.70795, 0.70918)
  )
  p <- synthetic_project(runs, env)
  p <- set_reference_material(p, "MMC", 0.70918)
  p <- set_reference_material(p, "SEC", 0.71030)
  p <- set_run_type(p, c("RM1", "RM2", "RM3", "RM4"), "reference", "MMC")
  p <- set_run_type(p, c("SEC1", "SEC2"), "reference", "SEC")
  update_project_settings(p, list(calibration = list(primary = "MMC", fit = "mean")))
}

test_that("reference material runs far from their expected value are flagged", {
  p <- rm_warning_project()
  expect_equal(p$settings$qc$rm_max_dev, 0.0005)
  r <- reference_material_results(p)
  expect_equal(r$sample_id[r$rm_flag], c("RM3", "SEC2"))

  # Primary runs: measured mean against the other primary runs of the session
  rm3 <- r[r$sample_id == "RM3", ]
  expect_equal(rm3$check_against, "session median")
  expect_equal(rm3$check_deviation, 0.001, tolerance = 1e-6)
  expect_equal(r$check_deviation[r$sample_id == "RM1"], 0, tolerance = 1e-6)
  # Secondary runs: corrected mean against the accepted value
  sec2 <- r[r$sample_id == "SEC2", ]
  expect_equal(sec2$check_against, "accepted")
  expect_equal(sec2$check_deviation, sec2$mean - 0.71030)
  expect_false(r$rm_flag[r$sample_id == "SEC1"])

  s <- summarise_reference_materials(p)
  expect_equal(s$runs_flagged[s$reference_material == "MMC"], 1)
  expect_equal(s$runs_flagged[s$reference_material == "SEC"], 1)
})

test_that("the warning level can be changed and is validated", {
  p <- rm_warning_project()
  p <- update_project_settings(p, list(qc = list(rm_max_dev = 0.01)))
  expect_false(any(reference_material_results(p)$rm_flag))
  expect_error(update_project_settings(p, list(qc = list(rm_max_dev = 0))), "rm_max_dev")
  expect_true("rm_max_dev" %in% settings_spec()$key)
})

test_that("a session with a single primary run is not checked", {
  runs <- data.frame(id = c("RM1", "SEC1"), minute = c(0, 10), ratio = c(0.71000, 0.71030))
  p <- synthetic_project(runs)
  p <- set_reference_material(p, "MMC", 0.70918)
  p <- set_reference_material(p, "SEC", 0.71030)
  p <- set_run_type(p, "RM1", "reference", "MMC")
  p <- set_run_type(p, "SEC1", "reference", "SEC")
  p <- update_project_settings(p, list(calibration = list(primary = "MMC")))
  r <- reference_material_results(p)
  expect_true(is.na(r$check_deviation[r$sample_id == "RM1"]))
  expect_false(r$rm_flag[r$sample_id == "RM1"])
})

test_that("without a primary, runs are checked against their accepted value", {
  p <- rm_warning_project()
  p <- update_project_settings(p, list(calibration = list(primary = "")))
  r <- reference_material_results(p)
  expect_true(all(r$check_against == "accepted"))
  expect_equal(r$sample_id[r$rm_flag], c("RM3", "SEC2"))
})

test_that("flags appear in the plot, the exports and the export record", {
  p <- rm_warning_project()
  r <- reference_material_results(p)
  plt <- plotly::plotly_build(reference_material_plot(calibration_model(p), r))
  flagged <- purrr::keep(plt$x$data, \(tr) identical(tr$name, "Flagged"))
  expect_length(flagged, 2)
  expect_equal(sum(purrr::map_lgl(flagged, \(tr) isTRUE(tr$showlegend))), 1)

  dir <- export_project(p)
  rm_csv <- utils::read.csv(file.path(dir, "reference_materials.csv"))
  expect_equal(rm_csv$sample_id[rm_csv$rm_flag], c("RM3", "SEC2"))
  md <- readLines(file.path(dir, "export_report.md"))
  expect_true(any(grepl("Runs flagged", md)))
  expect_true(any(grepl("Flagged \\(more than 0.0005 .*RM3, SEC2", md)))
  expect_true(any(grepl("Reference material warning", md)))
})
