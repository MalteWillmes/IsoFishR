rm_warning_project <- function(env = parent.frame()) {
  runs <- data.frame(
    id = c("SBO1", "SEC1", "SBO2", "SBO3", "SEC2", "SBO4"),
    minute = c(0, 10, 20, 30, 40, 50),
    # SBO3 is outside 0.7088-0.7093; SEC2 (NanoSr labelled as SEC) is far
    # from the other SEC run, so SEC's SD is above the limit.
    ratio = c(0.70920, 0.71030, 0.70921, 0.70940, 0.70795, 0.70919)
  )
  p <- synthetic_project(runs, env)
  p <- set_reference_material(p, "SBO", 0.70918)
  p <- set_reference_material(p, "SEC", 0.71030)
  p <- set_run_type(p, c("SBO1", "SBO2", "SBO3", "SBO4"), "reference", "SBO")
  p <- set_run_type(p, c("SEC1", "SEC2"), "reference", "SEC")
  update_project_settings(p, list(calibration = list(primary = "SBO", fit = "mean")))
}

test_that("runs with the uncorrected mean outside the accepted range are flagged", {
  p <- rm_warning_project()
  rms <- p$reference_materials
  # SBO gets its default range; SEC has none
  expect_equal(c(rms$range_min[rms$name == "SBO"], rms$range_max[rms$name == "SBO"]), c(0.7088, 0.7093))
  expect_true(is.na(rms$range_min[rms$name == "SEC"]))

  r <- reference_material_results(p)
  expect_equal(r$sample_id[r$outside_range], "SBO3")
  expect_equal(r$measured[r$sample_id == "SBO3"], 0.70940, tolerance = 1e-7)

  s <- summarise_reference_materials(p)
  expect_equal(s$runs_outside_range[s$reference_material == "SBO"], 1)
  expect_equal(p$settings$qc$rm_max_sd, 6.1e-5)
  # SBO: SD of 0.70920, 0.70921, 0.70940, 0.70919 is about 1e-4
  expect_equal(s$sd_uncorrected[s$reference_material == "SBO"],
               stats::sd(c(0.70920, 0.70921, 0.70940, 0.70919)), tolerance = 1e-6)
  expect_true(s$sd_flag[s$reference_material == "SBO"])
  expect_true(s$sd_flag[s$reference_material == "SEC"])
  # Without SBO3 the SBO runs are within the limit
  p <- update_sample(p, "SBO3", status = "excluded")
  s <- summarise_reference_materials(p)
  expect_false(s$sd_flag[s$reference_material == "SBO"])
})

test_that("ranges and the SD limit can be changed", {
  p <- rm_warning_project()
  p <- set_reference_material(p, "SEC", 0.71030, range_min = 0.7100, range_max = 0.7106)
  r <- reference_material_results(p)
  expect_equal(r$sample_id[r$outside_range], c("SBO3", "SEC2"))
  # Updating the value without a range keeps the range
  p <- set_reference_material(p, "SEC", 0.71031)
  expect_equal(p$reference_materials$range_max[p$reference_materials$name == "SEC"], 0.7106)
  # NA removes a limit
  p <- set_reference_material(p, "SBO", 0.70918, range_min = NA, range_max = NA)
  expect_equal(reference_material_results(p)$sample_id[reference_material_results(p)$outside_range], "SEC2")
  expect_error(set_reference_material(p, "SBO", 0.70918, range_min = 0.71, range_max = 0.70), "smaller")

  p <- update_project_settings(p, list(qc = list(rm_max_sd = 0.01)))
  expect_false(any(summarise_reference_materials(p)$sd_flag))
  expect_error(update_project_settings(p, list(qc = list(rm_max_sd = 0))), "rm_max_sd")
  expect_true("rm_max_sd" %in% settings_spec()$key)
})

test_that("a session with one run has no SD and is not flagged", {
  runs <- data.frame(id = c("SBO1", "SBO2"), minute = c(0, 10), ratio = c(0.70918, 0.70990))
  p <- synthetic_project(runs)
  p <- set_reference_material(p, "SBO", 0.70918)
  p <- set_run_type(p, "SBO1", "reference", "SBO")
  s <- summarise_reference_materials(p)
  expect_true(is.na(s$sd_uncorrected))
  expect_false(s$sd_flag)
})

test_that("older reference material tables get the default ranges once", {
  p <- rm_warning_project()
  path <- p$path
  close_project(p)
  f <- file.path(path, "reference_materials.csv")
  old <- utils::read.csv(f)[c("name", "value", "uncertainty_2s", "note")]
  utils::write.csv(old, f, row.names = FALSE, na = "")
  p <- open_project(path, user = "tester")
  rms <- p$reference_materials
  expect_equal(rms$range_min[rms$name == "SBO"], 0.7088)
  expect_true(is.na(rms$range_max[rms$name == "SEC"]))
  # Removed later, it stays removed
  p <- set_reference_material(p, "SBO", 0.70918, range_min = NA, range_max = NA)
  close_project(p)
  p <- open_project(path, user = "tester")
  withr::defer(close_project(p))
  expect_true(is.na(p$reference_materials$range_min[p$reference_materials$name == "SBO"]))
})

test_that("the short-lived rm_max_dev setting is dropped", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "s.yml")
  s <- default_settings()
  s$qc$rm_max_dev <- 0.0005
  yaml::write_yaml(s, f)
  expect_null(read_settings(f)$qc$rm_max_dev)
})

test_that("flags appear in the plot, the exports and the export record", {
  p <- rm_warning_project()
  r <- reference_material_results(p)
  plt <- plotly::plotly_build(reference_material_plot(calibration_model(p), r))
  rings <- purrr::keep(plt$x$data, \(tr) identical(tr$name, "Outside range"))
  expect_length(rings, 1)
  range_lines <- purrr::keep(plt$x$data, \(tr) identical(tr$name, "Accepted range"))
  expect_length(range_lines, 2)

  dir <- export_project(p)
  rm_csv <- utils::read.csv(file.path(dir, "reference_materials.csv"))
  expect_equal(rm_csv$sample_id[rm_csv$outside_range], "SBO3")
  summ <- utils::read.csv(file.path(dir, "reference_material_summary.csv"))
  expect_true(all(c("runs_outside_range", "sd_flag") %in% names(summ)))
  md <- readLines(file.path(dir, "export_report.md"))
  expect_true(any(grepl("0.7088 to 0.7093", md, fixed = TRUE)))
  expect_true(any(grepl("outside the accepted range: SBO3", md, fixed = TRUE)))
  expect_true(any(grepl("(above limit)", md, fixed = TRUE)))
  expect_true(any(grepl("above 0.000061", md, fixed = TRUE)))
})
