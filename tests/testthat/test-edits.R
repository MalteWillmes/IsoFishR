two_samples <- function() {
  raw <- dplyr::bind_rows(make_raw("a", n = 100), make_raw("b", n = 100))
  reduce_runs(raw, no_filter_settings())$data
}

test_that("v1 bug: trimming one sample leaves the other samples untouched", {
  # In v1, `&` bound tighter than `|` in the trim filter, so the right trim was
  # applied to every sample.
  d <- two_samples()
  edited <- apply_edits(d, tibble::tibble(sample_id = "a", trim_start_s = 2, trim_end_s = 4))
  a <- dplyr::filter(edited, sample_id == "a")
  b <- dplyr::filter(edited, sample_id == "b")
  expect_equal(nrow(b), 100)
  expect_equal(nrow(a), 100 - 10 - 20)
  expect_equal(min(a$time_s), 0.2 + 2, tolerance = 1e-9)
  expect_equal(max(a$time_s), 20 - 4, tolerance = 1e-9)
})

test_that("trimming re-zeroes distance unless asked not to", {
  d <- two_samples()
  trimmed <- apply_edits(d, tibble::tibble(sample_id = "a", trim_start_s = 2))
  expect_equal(min(trimmed$distance_um[trimmed$sample_id == "a"]), 0)
  kept <- apply_edits(d, tibble::tibble(sample_id = "a", trim_start_s = 2, rezero = FALSE))
  expect_equal(min(kept$distance_um[kept$sample_id == "a"]), 2 * 10, tolerance = 1e-9)
})

test_that("reversing a profile mirrors distance but keeps analysis time", {
  d <- two_samples()
  rev <- apply_edits(d, tibble::tibble(sample_id = "a", reverse = TRUE))
  a_orig <- dplyr::filter(d, sample_id == "a")
  a_rev <- dplyr::filter(rev, sample_id == "a")
  expect_equal(range(a_rev$distance_um), range(a_orig$distance_um))
  expect_equal(a_rev$time_s[1], max(a_orig$time_s))
  expect_equal(dplyr::filter(rev, sample_id == "b"), dplyr::filter(d, sample_id == "b"))
})

test_that("apply_edits without edits returns the data unchanged", {
  d <- two_samples()
  expect_equal(apply_edits(d), d)
})

test_that("regions are summarised per sample using analysis time", {
  raw <- dplyr::bind_rows(
    make_raw("a", n = 100, sr87_sr86 = rep(c(0.7050, 0.7100), each = 50)),
    make_raw("b", n = 100, sr87_sr86 = 0.7120)
  )
  d <- reduce_runs(raw, no_filter_settings(smoothing = "none"))$data
  regions <- tibble::tribble(
    ~sample_id, ~region,   ~start_s, ~end_s,
    "a",        "core",    0,        10,
    "a",        "edge",    10.1,     20,
    "b",        "core",    0,        10
  )
  out <- summarise_regions(d, regions)
  expect_equal(out$mean, c(0.7050, 0.7100, 0.7120), tolerance = 1e-10)
  expect_equal(out$n, c(50L, 50L, 50L))
  expect_equal(out$region, c("core", "edge", "core"))
})

test_that("v1 bug: changepoint segment means are calculated within each sample", {
  # v1 grouped by segment number only, so segment 1 of sample a was averaged
  # with segment 1 of sample b.
  set.seed(3)
  noise <- function(n) stats::rnorm(n, sd = 2e-5)
  raw <- dplyr::bind_rows(
    make_raw("a", n = 100, sr87_sr86 = c(rep(0.705, 50), rep(0.710, 50)) + noise(100)),
    make_raw("b", n = 100, sr87_sr86 = c(rep(0.708, 30), rep(0.712, 70)) + noise(100))
  )
  d <- reduce_runs(raw, no_filter_settings(smoothing = "none"))$data
  cp <- find_changepoints(d, method = "PELT", penalty = 1e-6)
  seg_means <- dplyr::distinct(cp, sample_id, cp_segment, cp_mean)
  expect_equal(seg_means$cp_segment, c(1L, 2L, 1L, 2L))
  expect_equal(seg_means$cp_mean, c(0.705, 0.710, 0.708, 0.712), tolerance = 1e-5)
})
