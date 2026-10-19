test_that("example runfiles are read with the default import settings", {
  files <- example_runfiles()
  expect_length(files, 6)
  raw <- read_runfiles(files[1])
  expect_named(raw, c("sample_id", "cycle", "time_s", "raw88", "raw87", "raw86",
                      "raw85", "raw84", "raw83"))
  expect_equal(nrow(raw), 6895)
  expect_equal(raw$time_s[1:2], c(1.1, 1.2))
  expect_true(all(vapply(raw[-1], is.numeric, logical(1))))
  expect_equal(unique(raw$sample_id), "PC_2016_01_A2_4R")
})

test_that("read_runfiles uses supplied sample IDs and rejects duplicates", {
  f <- example_runfiles()[1]
  expect_equal(unique(read_runfiles(f, sample_ids = "fish1")$sample_id), "fish1")
  expect_error(read_runfiles(c(f, f), sample_ids = c("a", "a")), "unique")
})

test_that("a wrong column layout gives a clear error", {
  imp <- default_settings()$import
  imp$columns$time_s <- 50L
  expect_error(read_runfiles(example_runfiles()[1], imp), "No data found")
})

test_that("settings round-trip through YAML and fill in missing values", {
  path <- withr::local_tempfile(fileext = ".yml")
  s <- default_settings()
  s$reduction$outlier_sd <- 0
  s$sample$run_speed_um_s <- 5
  write_settings(s, path)
  expect_equal(read_settings(path), s)

  yaml::write_yaml(list(reduction = list(integration = 3L)), path)
  partial <- read_settings(path)
  expect_equal(partial$reduction$integration, 3L)
  expect_equal(partial$reduction$ma_window, default_settings()$reduction$ma_window)
})

test_that("invalid settings are rejected", {
  expect_error(merge_settings(default_settings(), list(reduction = list(smoothing = "Spline"))),
               "smoothing")
  expect_error(merge_settings(default_settings(), list(reduction = list(raw88_min_v = 20))),
               "raw88_min_v")
})

test_that("v1 settings files are converted", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    '"raw88","raw87","raw86","raw85","raw84","raw83","cyclesec","vskip","fskip","header","sep","smoother","raw88lowerthresh","raw88upperthresh","average_num","spline_k","outlier_num","SD_outlier","speed","fluency","spotsize","energy","integration","blanksubcheck","sampletype","analysistype","sampledirection","blanktime","Sr8688ratio","Rb8587ratio","username","range1_label","range2_label","range3_label","range4_label","range5_label","range6_label","range7_label","range8_label"',
    '"1","2","3","4","5","6","8","46","0","FALSE",",","Spline","0.05","9.8","10","10","10","NA","5","1.85","40","55","5","TRUE","Otolith","Line","Ventral to Dorsal Edge","30","0.1194","2.59712","MW","Core","Natal","Range 3","Range 4","Range 5","Range 6","Range 7","Range 8"'
  ), path)
  s <- import_v1_settings(path)
  expect_equal(s$reduction$smoothing, "spline")
  expect_equal(s$reduction$outlier_sd, 0)
  expect_equal(s$reduction$raw88_min_v, 0.05)
  expect_equal(s$sample$run_speed_um_s, 5)
  expect_equal(s$sample$analysis_type, "line")
  expect_equal(s$import$columns$time_s, 8L)
  expect_equal(s$regions$default_labels[1:2], c("Core", "Natal"))
})
