# Centred rolling statistics.
#
# Means and standard deviations are computed from cumulative sums, so the cost
# is O(n) regardless of the window width (v1 called zoo::rollapply, which
# evaluates an R function for every window). Missing values are ignored and
# `n` reports how many values each window actually used.
#
# A window of width w covers (w - 1) %/% 2 points before and the rest after
# the centre point. With `partial = TRUE` windows are truncated at the ends of
# the series; with `partial = FALSE` the ends take the value of the nearest
# full window (zoo's fill = "extend").

roll_bounds <- function(n, width) {
  before <- (width - 1L) %/% 2L
  after <- width - 1L - before
  i <- seq_len(n)
  list(lo = pmax(i - before, 1L), hi = pmin(i + after, n))
}

roll_stats <- function(x, width, partial = TRUE) {
  n <- length(x)
  if (n == 0) {
    return(list(mean = numeric(), sd = numeric(), n = integer()))
  }
  width <- as.integer(width)
  b <- roll_bounds(n, width)

  ok <- !is.na(x)
  # Centre the data first so the sum-of-squares difference stays accurate for
  # ratios with a tiny variance relative to their mean.
  centre <- if (any(ok)) mean(x[ok]) else 0
  xc <- ifelse(ok, x - centre, 0)
  cs_n <- c(0L, cumsum(ok))
  cs_s <- c(0, cumsum(xc))
  cs_ss <- c(0, cumsum(xc^2))

  cnt <- cs_n[b$hi + 1L] - cs_n[b$lo]
  s <- cs_s[b$hi + 1L] - cs_s[b$lo]
  ss <- cs_ss[b$hi + 1L] - cs_ss[b$lo]
  m <- ifelse(cnt > 0, s / cnt, NA_real_)
  v <- ifelse(cnt > 1, pmax((ss - s^2 / cnt) / (cnt - 1), 0), NA_real_)

  out <- list(mean = m + centre, sd = sqrt(v), n = cnt)
  if (!partial) {
    full <- (b$hi - b$lo + 1L) == width
    out <- lapply(out, extend_ends, keep = full)
  }
  out
}

roll_median <- function(x, width, partial = TRUE) {
  n <- length(x)
  width <- as.integer(width)
  # stats::runmed is fast but needs an odd window, no NAs and constant ends.
  if (!partial && !anyNA(x) && width %% 2L == 1L && n >= width) {
    return(as.numeric(stats::runmed(x, width, endrule = "constant")))
  }
  b <- roll_bounds(n, width)
  out <- vapply(seq_len(n), \(i) {
    w <- x[b$lo[i]:b$hi[i]]
    if (all(is.na(w))) NA_real_ else stats::median(w, na.rm = TRUE)
  }, numeric(1))
  if (!partial) out <- extend_ends(out, keep = (b$hi - b$lo + 1L) == width)
  out
}

# Robust standard deviation of residuals `r` (1.4826 x median absolute
# value), rolling over `width` values, or over all of them if fewer.
robust_spread <- function(r, width) {
  n <- length(r)
  if (n == 0) return(numeric())
  a <- abs(r)
  width <- as.integer(width)
  if (width %% 2L == 0L) width <- width + 1L
  if (n <= width) return(rep(1.4826 * stats::median(a, na.rm = TRUE), n))
  1.4826 * roll_median(a, width, partial = FALSE)
}

# Rolling median of each value's neighbours, leaving the value itself out
# (so an outlier doesn't pull its own reference towards it). Windows at the
# ends are shifted inwards to keep their full width.
roll_median_others <- function(x, width) {
  n <- length(x)
  if (n < 2) return(rep(NA_real_, n))
  width <- min(as.integer(width), n)
  b <- roll_bounds(n, width)
  full <- (b$hi - b$lo + 1L) == width
  if (any(full)) {
    first <- which(full)[1]
    last <- utils::tail(which(full), 1)
    b$lo[seq_len(first - 1L)] <- b$lo[first]
    b$hi[seq_len(first - 1L)] <- b$hi[first]
    if (last < n) {
      b$lo[(last + 1L):n] <- b$lo[last]
      b$hi[(last + 1L):n] <- b$hi[last]
    }
  }
  vapply(seq_len(n), \(i) {
    w <- x[setdiff(b$lo[i]:b$hi[i], i)]
    if (all(is.na(w))) NA_real_ else stats::median(w, na.rm = TRUE)
  }, numeric(1))
}

# Replace values outside the `keep` range with the first/last kept value.
# Falls back to the unchanged values if no window is complete.
extend_ends <- function(x, keep) {
  idx <- which(keep)
  if (length(idx) == 0) return(x)
  first <- idx[1]
  last <- idx[length(idx)]
  x[seq_len(first - 1L)] <- x[first]
  if (last < length(x)) x[(last + 1L):length(x)] <- x[last]
  x
}
