example_runfiles <- function() {
  list.files(system.file("extdata", "runfiles", package = "IsoFishR"),
             pattern = "\\.run$", recursive = TRUE, full.names = TRUE)
}

# Synthetic raw cycle data for one sample with known isotope ratios.
#
# Voltages are generated from a true 87Sr/86Sr ratio, an exponential-law mass
# bias `beta` and an optional Rb signal, so a correct reduction must return
# `sr87_sr86` exactly. The first `blank_s` seconds are a blank at `blank_v`.
make_raw <- function(sample_id = "s1", n = 200, sr87_sr86 = 0.7092, beta = -1.6,
                     raw88 = 4, raw85 = 0, blank_s = 0, blank_v = 0, dt = 0.2) {
  m <- IsoFishR:::MASS
  sr86_sr88_true <- 0.1194
  time <- seq_len(n) * dt
  sr88 <- rep(raw88, n)
  # measured = true / (mass ratio)^beta
  sr86 <- sr88 * sr86_sr88_true / (m[["sr86"]] / m[["sr88"]])^beta
  sr87 <- sr86 * rep_len(sr87_sr86, n) / (m[["sr87"]] / m[["sr86"]])^beta
  rb87 <- (raw85 / 2.59265) * (m[["rb85"]] / m[["rb87"]])^beta
  sr84 <- sr86 * 0.0565 / (m[["sr84"]] / m[["sr86"]])^beta

  d <- tibble::tibble(
    sample_id = sample_id,
    cycle = seq_len(n),
    time_s = time,
    raw88 = sr88, raw87 = sr87 + rb87, raw86 = sr86,
    raw85 = raw85, raw84 = sr84, raw83 = 0.001
  )
  blank <- time <= blank_s
  d[blank, c("raw88", "raw87", "raw86", "raw85", "raw84", "raw83")] <- 0
  cols <- c("raw88", "raw87", "raw86", "raw85", "raw84", "raw83")
  d[cols] <- d[cols] + blank_v
  d
}

no_filter_settings <- function(...) {
  reduction <- utils::modifyList(
    list(integration = 1L, blank_correction = FALSE, raw88_min_v = 0.01, outlier_sd = 0),
    list(...)
  )
  merge_settings(default_settings(), list(reduction = reduction))
}

# The stored trim (s after the run's first cycle) that cuts `seconds` from
# the start of the profile (the cycles passing the thresholds).
profile_trim <- function(project, sample_id, seconds) {
  b <- profile_bounds(project, sample_id)
  b$profile_start - b$run_start + seconds
}
