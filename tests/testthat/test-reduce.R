test_that("mass bias and Rb correction recover the true 87Sr/86Sr", {
  raw <- make_raw(sr87_sr86 = 0.7092, beta = -1.6, raw85 = 0.02)
  d <- reduce_runs(raw, no_filter_settings())$data
  expect_equal(d$sr87_sr86, rep(0.7092, nrow(d)), tolerance = 1e-10)
  expect_equal(d$mass_bias, rep(-1.6, nrow(d)), tolerance = 1e-10)
  expect_equal(d$sr84_sr86_mb, rep(0.0565, nrow(d)), tolerance = 1e-10)
})

test_that("blank correction subtracts the mean blank of each sample", {
  raw <- dplyr::bind_rows(
    make_raw("a", blank_s = 10, blank_v = 0.01),
    make_raw("b", blank_s = 10, blank_v = 0.03)
  )
  s <- merge_settings(default_settings(), list(reduction = list(
    integration = 1L, blank_time_s = 10, raw88_min_v = 0.5, outlier_sd = 0
  )))
  res <- reduce_runs(raw, s)
  expect_equal(res$blank$blk88, c(0.01, 0.03))
  expect_true(all(res$data$time_s > 10))
  expect_equal(res$data$sr87_sr86, rep(0.7092, nrow(res$data)), tolerance = 1e-10)
})

test_that("a sample without blank cycles warns instead of silently producing NAs", {
  raw <- dplyr::bind_rows(make_raw("a", blank_s = 10), make_raw("b"))
  s <- merge_settings(default_settings(), list(reduction = list(blank_time_s = 10, outlier_sd = 0)))
  expect_warning(res <- reduce_runs(raw, s), "No blank cycles found for: b")
  expect_false(anyNA(res$data$sr87_sr86))
})

test_that("v1 bug: integration blocks never span two samples", {
  # 7 cycles per sample with integration 5: v1 averaged cycles 6-7 of sample a
  # together with cycles 1-3 of sample b.
  raw <- dplyr::bind_rows(
    make_raw("a", n = 7, raw88 = 1),
    make_raw("b", n = 7, raw88 = 9)
  )
  int <- integrate_cycles(raw, 5)
  expect_equal(int$sample_id, c("a", "a", "b", "b"))
  expect_equal(int$raw88, c(1, 1, 9, 9))
  expect_equal(int$n_cycles, c(5L, 2L, 5L, 2L))
  expect_equal(int$time_s[1], mean(raw$time_s[1:5]))
})

test_that("distance starts at zero for each sample", {
  raw <- dplyr::bind_rows(make_raw("a", n = 50), make_raw("b", n = 80))
  s <- no_filter_settings()
  s$sample$run_speed_um_s <- 5
  d <- reduce_runs(raw, s)$data
  expect_equal(as.vector(tapply(d$distance_um, d$sample_id, min)), c(0, 0))
  expect_equal(diff(d$distance_um[1:2]), 0.2 * 5)
})

test_that("voltage thresholds remove low and saturated cycles", {
  raw <- make_raw(n = 10)
  raw$raw88[c(2, 3)] <- c(0.1, 10)
  d <- reduce_runs(raw, no_filter_settings(raw88_min_v = 0.5))$data
  expect_equal(nrow(d), 8)
})

test_that("outliers are flagged and removed from the ratio", {
  ratio <- rep(0.7092, 101)
  set.seed(2)
  ratio <- ratio + stats::rnorm(101, sd = 5e-5)
  ratio[60] <- 0.7300
  raw <- make_raw(n = 101, sr87_sr86 = ratio)
  d <- reduce_runs(raw, no_filter_settings(outlier_sd = 3))$data
  expect_equal(which(d$is_outlier), 60)
  expect_true(is.na(d$sr87_sr86[60]))
  expect_equal(d$sr87_sr86_outlier[60], 0.73, tolerance = 1e-8)
  # The moving average skips the rejected point
  expect_lt(abs(d$sr87_sr86_ma[60] - 0.7092), 1e-4)
})

test_that("moving average standard error uses the points actually in the window", {
  raw <- make_raw(n = 30, sr87_sr86 = 0.709 + (1:30) * 1e-5)
  d <- reduce_runs(raw, no_filter_settings(ma_window = 10L))$data
  expect_equal(d$sr87_sr86_ma_n[1], 6L)
  expect_equal(d$sr87_sr86_ma_n[15], 10L)
  expect_equal(d$sr87_sr86_ma_se, d$sr87_sr86_ma_sd / sqrt(d$sr87_sr86_ma_n))
})

test_that("v1 bug: spline smoothing produces the analysis value", {
  # v1 compared the setting to "spline" while the button sent "Spline", so the
  # analysis column was never created.
  raw <- make_raw(n = 150, sr87_sr86 = 0.709 + sin(seq(0, 3, length.out = 150)) * 1e-3)
  d <- reduce_runs(raw, no_filter_settings(smoothing = "spline"))$data
  expect_false(anyNA(d$sr87_sr86_value))
  expect_equal(d$sr87_sr86_value, d$sr87_sr86_spline)
  expect_lt(max(abs(d$sr87_sr86_spline - d$sr87_sr86)), 1e-4)
})

test_that("smoothing 'none' and 'ma' select the matching column", {
  raw <- make_raw(n = 40)
  expect_equal(reduce_runs(raw, no_filter_settings(smoothing = "none"))$data$sr87_sr86_value,
               reduce_runs(raw, no_filter_settings())$data$sr87_sr86)
  ma <- reduce_runs(raw, no_filter_settings(smoothing = "ma"))$data
  expect_equal(ma$sr87_sr86_value, ma$sr87_sr86_ma)
  expect_false("sr87_sr86_spline" %in% names(ma))
})

test_that("example runfiles reduce to plausible values", {
  raw <- read_runfiles(example_runfiles())
  res <- reduce_runs(raw)
  d <- res$data
  expect_setequal(unique(d$sample_id), unique(raw$sample_id))
  expect_equal(nrow(res$blank), 6)
  means <- tapply(d$sr87_sr86_value, d$sample_id, mean, na.rm = TRUE)
  expect_true(all(means > 0.703 & means < 0.712))
})

test_that("the adaptive spline follows a sharp step and smooths noise", {
  set.seed(11)
  x <- seq(0, 3000, length.out = 900)
  truth <- ifelse(x < 1500, 0.7080, 0.7092)
  d <- tibble::tibble(sample_id = "s", distance_um = x, total_sr = 5,
                      sr87_sr86 = truth + stats::rnorm(length(x), sd = 4e-4))
  s <- smooth_spline(d)
  expect_named(s, c(names(d), "sr87_sr86_spline", "sr87_sr86_spline_se", "sr87_sr86_spline_edf"))
  # Plateaus: close to the true values, much less scatter than the points
  plateau <- abs(x - 1500) > 300
  expect_lt(stats::sd(s$sr87_sr86_spline[plateau] - truth[plateau]), 1e-4)
  # The step is followed within 150 um
  mid <- s$sr87_sr86_spline[which.min(abs(x - 1650))]
  expect_gt(mid, 0.7088)
  expect_gt(s$sr87_sr86_spline_edf[1], 2)
  # Pure noise around a constant gives an almost straight line
  flat <- dplyr::mutate(d, sr87_sr86 = 0.709 + stats::rnorm(length(x), sd = 4e-4))
  expect_lt(smooth_spline(flat)$sr87_sr86_spline_edf[1], 3)
  # No scatter at all still works (falls back to a line)
  const <- dplyr::mutate(d, sr87_sr86 = 0.709)
  expect_equal(smooth_spline(const)$sr87_sr86_spline, rep(0.709, length(x)), tolerance = 1e-9)
})

test_that("moving average + spline uses the spline as the smoothed value", {
  raw <- read_runfiles(example_runfiles()[1])
  s <- default_settings()
  expect_equal(s$reduction$smoothing, "ma_spline")
  d <- reduce_runs(raw, s)$data
  expect_equal(d$sr87_sr86_value, d$sr87_sr86_spline)
  expect_false(anyNA(d$sr87_sr86_ma[!is.na(d$sr87_sr86)]))
  s$reduction$smoothing <- "ma"
  expect_false("sr87_sr86_spline" %in% names(reduce_runs(raw, s)$data))
})

test_that("settings from older versions with spline_k still load", {
  path <- withr::local_tempfile(fileext = ".yml")
  old <- default_settings()
  old$reduction$spline_k <- 10L
  old$reduction$smoothing <- "spline"
  yaml::write_yaml(old, path)
  s <- read_settings(path)
  expect_null(s$reduction$spline_k)
  expect_equal(s$reduction$smoothing, "spline")
  expect_equal(parse_overrides("spline_k=12; ma_window=5"), list(ma_window = 5L))
})

test_that("a run with no points above the thresholds reduces without error", {
  raw <- read_runfiles(example_runfiles()[1])
  s <- default_settings()
  s$reduction$raw88_min_v <- 9.7   # nothing passes
  d <- reduce_runs(raw, s)$data
  expect_equal(nrow(d), 0)
  expect_true(all(c("sr87_sr86_spline", "sr87_sr86_value") %in% names(d)))
})
