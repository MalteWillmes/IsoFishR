naive_roll <- function(x, width, f) {
  before <- (width - 1L) %/% 2L
  after <- width - 1L - before
  n <- length(x)
  vapply(seq_len(n), \(i) {
    w <- x[max(1, i - before):min(n, i + after)]
    w <- w[!is.na(w)]
    if (length(w) == 0) NA_real_ else f(w)
  }, numeric(1))
}

test_that("roll_stats matches a naive implementation, including partial ends and NAs", {
  set.seed(1)
  x <- 0.7092 + stats::rnorm(101, sd = 1e-4)
  x[c(5, 50, 51)] <- NA
  for (w in c(1, 4, 5, 10, 21)) {
    r <- IsoFishR:::roll_stats(x, w)
    expect_equal(r$mean, naive_roll(x, w, mean), tolerance = 1e-12)
    expect_equal(r$sd, naive_roll(x, w, \(v) if (length(v) > 1) stats::sd(v) else NA), tolerance = 1e-9)
    expect_equal(r$n, as.integer(dplyr::coalesce(naive_roll(x, w, length), 0)))
  }
})

test_that("non-partial windows extend the nearest full window to the ends", {
  x <- c(1, 2, 3, 4, 5, 6, 7)
  expect_equal(IsoFishR:::roll_stats(x, 3, partial = FALSE)$mean, c(2, 2, 3, 4, 5, 6, 6))
  expect_equal(IsoFishR:::roll_median(x, 3, partial = FALSE), c(2, 2, 3, 4, 5, 6, 6))
  expect_equal(IsoFishR:::roll_median(x, 4, partial = FALSE),
               IsoFishR:::extend_ends(naive_roll(x, 4, median), c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)))
})

test_that("roll_median handles NAs", {
  x <- c(1, NA, 3, 100, 5)
  expect_equal(IsoFishR:::roll_median(x, 3), naive_roll(x, 3, median))
})
