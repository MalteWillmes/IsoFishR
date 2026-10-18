export_fixture <- function(env = parent.frame()) {
  path <- file.path(withr::local_tempdir(.local_envir = env), "ExportProject")
  p <- create_project(path, user = "tester")
  withr::defer(close_project(p), envir = env)
  files <- example_runfiles()[grepl("PC_2016", example_runfiles())][1:3]
  p <- add_runfiles(p, files)
  ids <- p$samples$sample_id
  p <- set_regions(p, ids[1], data.frame(region = c("core", "edge"), start_s = c(200, 1000),
                                         end_s = c(260, 1100)))
  p <- update_sample(p, ids[1], trim_start_s = 10, comment = "good", cp_enabled = TRUE)
  p <- set_sample_settings(p, ids[2], ma_window = 20)
  update_sample(p, ids[3], status = "excluded")
}

test_that("sample summary has one row per included sample with its edits and statistics", {
  p <- export_fixture()
  s <- summarise_project_samples(p)
  expect_equal(s$sample_id, p$samples$sample_id[1:2])
  expect_equal(s$trim_start_um, c(100, 0))
  expect_equal(s$comment, c("good", ""))
  expect_equal(s$n_regions, c(2L, 0L))
  expect_gt(s$n_cp_segments[1], 0)
  expect_equal(s$n_cp_segments[2], 0L)
  expect_equal(s$setting_overrides, c("", "ma_window=20"))
  expect_true(all(s$mean > 0.703 & s$mean < 0.712))
  expect_equal(s$se, s$sd / sqrt(s$n_points))

  expect_equal(nrow(summarise_project_samples(p, include_excluded = TRUE)), 3)
})

test_that("region summary uses the individual points and reports the smoothed mean", {
  p <- export_fixture()
  r <- summarise_project_regions(p)
  expect_equal(r$region, c("core", "edge"))
  d <- project_data(p, p$samples$sample_id[1])
  core <- dplyr::filter(d, time_s >= 200, time_s <= 260)
  expect_equal(r$mean[1], mean(core$sr87_sr86, na.rm = TRUE))
  expect_equal(r$n[1], sum(!is.na(core$sr87_sr86)))
  expect_equal(r$mean_smoothed[1], mean(core$sr87_sr86_value, na.rm = TRUE))
  expect_equal(r$start_s, c(200, 1000))
})

test_that("changepoint summary covers only samples with changepoints enabled", {
  p <- export_fixture()
  cp <- summarise_project_changepoints(p)
  expect_equal(unique(cp$sample_id), p$samples$sample_id[1])
  expect_equal(cp$segment, seq_len(nrow(cp)))
  expect_true(all(cp$method == "PELT"))
  # Segments tile the profile without overlapping
  expect_true(all(cp$distance_min_um[-1] > cp$distance_max_um[-nrow(cp)]))
})

test_that("settings table shows each sample's effective settings", {
  p <- export_fixture()
  s <- project_settings_table(p)
  expect_equal(s$reduction.ma_window, c("10", "20"))
  expect_true(all(c("sample.run_speed_um_s", "import.skip_header") %in% names(s)))
})

test_that("export_project writes the summaries to a new folder and logs it", {
  p <- export_fixture()
  dir <- export_project(p)
  expect_setequal(list.files(dir),
                  c("samples.csv", "regions.csv", "changepoints.csv", "settings.csv", "profiles.csv", "runs.csv",
                    "export_info.yml"))
  info <- yaml::read_yaml(file.path(dir, "export_info.yml"))
  expect_equal(info$exported_by, "tester")
  expect_equal(info$samples, 2)
  regions <- readr::read_csv(file.path(dir, "regions.csv"), show_col_types = FALSE)
  expect_equal(regions$region, c("core", "edge"))
  expect_equal(utils::tail(read_log(p)$action, 1), "export")

  Sys.sleep(1.1)
  dir2 <- export_project(p, files = EXPORT_FILES, include_excluded = TRUE)
  expect_false(dir2 == dir)
  expect_true(file.exists(file.path(dir2, "data.csv")))
  expect_equal(nrow(readr::read_csv(file.path(dir2, "samples.csv"), show_col_types = FALSE)), 3)
})

test_that("a project without samples cannot be exported", {
  path <- file.path(withr::local_tempdir(), "Empty")
  p <- create_project(path, user = "tester")
  on.exit(close_project(p))
  expect_error(export_project(p), "no samples")
})

test_that("profiles hold every point of every included sample, after edits", {
  p <- export_fixture()
  ids <- p$samples$sample_id
  pr <- project_profiles(p)
  d <- project_data(p, ids[1:2])

  expect_equal(unique(pr$sample_id), ids[1:2])          # excluded sample left out
  expect_equal(nrow(pr), nrow(d))                         # one row per point
  expect_equal(pr$distance_um, d$distance_um)
  expect_equal(min(pr$distance_um[pr$sample_id == ids[1]]), 0)   # trimmed and re-zeroed
  expect_equal(pr$sr87_sr86, d$sr87_sr86)
  expect_equal(pr$sr87_sr86_smoothed, d$sr87_sr86_ma)
  expect_equal(pr$sr87_sr86_smoothed_se, d$sr87_sr86_ma_se)
  expect_equal(unique(pr$smoothing), "ma")

  # Region labels per point match the region definitions
  core <- pr$sample_id == ids[1] & pr$time_s >= 200 & pr$time_s <= 260
  expect_true(all(pr$region[core] == "core"))
  expect_true(all(pr$region[pr$sample_id == ids[2]] == ""))

  # Changepoints only for the sample that has them enabled
  expect_false(anyNA(pr$cp_segment[pr$sample_id == ids[1] & !is.na(pr$sr87_sr86)]))
  expect_true(all(is.na(pr$cp_segment[pr$sample_id == ids[2]])))

  expect_equal(nrow(project_profiles(p, include_excluded = TRUE)),
               nrow(project_data(p, ids)))
})

test_that("profiles report the spline and its SE for spline-smoothed samples", {
  p <- export_fixture()
  id <- p$samples$sample_id[2]
  p <- set_sample_settings(p, id, smoothing = "spline")
  pr <- dplyr::filter(project_profiles(p), sample_id == id)
  d <- project_data(p, id)
  expect_equal(unique(pr$smoothing), "spline")
  expect_equal(pr$sr87_sr86_smoothed, d$sr87_sr86_spline)
  expect_equal(pr$sr87_sr86_smoothed_se, d$sr87_sr86_spline_se)
})

test_that("profiles.csv is written with every point and rounded values", {
  p <- export_fixture()
  dir <- export_project(p)
  pr <- readr::read_csv(file.path(dir, "profiles.csv"), show_col_types = FALSE)
  expect_equal(nrow(pr), nrow(project_profiles(p)))
  expect_equal(pr$sr87_sr86, round(project_profiles(p)$sr87_sr86, 6))
  expect_true("profiles.csv" %in% yaml::read_yaml(file.path(dir, "export_info.yml"))$files)
})
