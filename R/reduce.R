# Isotope masses (u) used for the exponential mass bias law and Rb correction.
MASS <- c(
  sr84 = 83.9134191,
  rb85 = 84.9117897,
  sr86 = 85.9092607,
  sr87 = 86.9088775,
  rb87 = 86.9091805,
  sr88 = 87.9056123
)

#' Reduce laser-ablation Sr isotope data
#'
#' Runs the full data reduction for one or more samples: integration, blank
#' correction, voltage thresholds, distance, Rb interference and mass bias
#' correction, outlier rejection and smoothing. Every step is applied within
#' each sample.
#'
#' @param raw Raw cycle data from [read_runfiles()].
#' @param settings A settings list (see [default_settings()]).
#' @param blank Optional blank voltages to subtract instead of each run's own
#'   blank: a data frame with `sample_id` and `blk88` to `blk83`, e.g.
#'   interpolated from baseline runs (see [project_baselines()]). When given,
#'   the start of each run is not treated as blank; laser-off cycles are
#'   removed by the 88Sr threshold.
#' @param trims Optional trims: a data frame with `sample_id`, `trim_start_s`
#'   and `trim_end_s`, seconds removed after the first and before the last
#'   cycle of the run (see [apply_edits()]). They are applied before outlier
#'   rejection and smoothing, so trimmed data never affects the result.
#' @return A list with `data` (one row per integrated cycle) and `blank`
#'   (mean blank voltages per sample, or `NULL` without blank correction).
#'   `data` includes `run_start_s` and `run_end_s` (time of the run's first
#'   and last cycle) and `profile_start_s` and `profile_end_s` (first and
#'   last cycle passing the thresholds, before trims).
#' @export
reduce_runs <- function(raw, settings = default_settings(), blank = NULL, trims = NULL) {
  check_settings(settings)
  r <- settings$reduction

  run_range <- dplyr::summarise(raw, run_start_s = min(time_s), run_end_s = max(time_s),
                                .by = sample_id)
  data <- integrate_cycles(raw, r$integration)

  if (!is.null(blank)) {
    blank <- dplyr::mutate(blank, n_blank = blank$n_blank %||% NA_integer_)
  } else if (isTRUE(r$blank_correction)) {
    blank <- summarise_blank(data, r$blank_time_s, r$raw88_min_v)
    data <- dplyr::filter(data, time_s > r$blank_time_s)
  }

  # Thresholds are applied to the uncorrected 88Sr signal, as in v1.
  data <- dplyr::filter(data, raw88 >= r$raw88_min_v, raw88 <= r$raw88_max_v)

  if (!is.null(blank)) {
    data <- subtract_blank(data, blank)
  }

  data <- data |>
    dplyr::left_join(run_range, by = "sample_id") |>
    dplyr::mutate(
      distance_um = (time_s - min(time_s)) * settings$sample$run_speed_um_s,
      profile_start_s = min(time_s),
      profile_end_s = max(time_s),
      .by = sample_id
    ) |>
    trim_runs(trims) |>
    correct_ratios(r$sr86_sr88_ref, r$rb85_rb87) |>
    reject_outliers(r$outlier_sd, r$outlier_window) |>
    smooth_ma(r$ma_window)

  if (r$smoothing %in% c("spline", "ma_spline")) {
    data <- smooth_spline(data)
  }
  # The smoothed value used in summaries; with both, the spline.
  data <- dplyr::mutate(data, sr87_sr86_value = switch(
    r$smoothing,
    none = sr87_sr86,
    ma = sr87_sr86_ma,
    spline = ,
    ma_spline = sr87_sr86_spline
  ))

  list(data = data, blank = blank)
}

# Keep only the cycles inside each run's trims. Trims are counted from the
# run's first and last cycle (`run_start_s`, `run_end_s`, or the data's own
# first and last time without them), so they stay on the same part of the
# sample when the thresholds, blank time or integration change.
trim_runs <- function(data, trims = NULL) {
  if (is.null(trims) || nrow(trims) == 0 || nrow(data) == 0) return(data)
  trims <- tibble::tibble(
    sample_id = trims$sample_id,
    .trim_start = dplyr::coalesce(as.numeric(trims$trim_start_s %||% 0), 0),
    .trim_end = dplyr::coalesce(as.numeric(trims$trim_end_s %||% 0), 0)
  )
  if (!"run_start_s" %in% names(data)) {
    data <- dplyr::mutate(data, run_start_s = min(time_s), run_end_s = max(time_s), .by = sample_id)
  }
  data |>
    dplyr::left_join(trims, by = "sample_id") |>
    dplyr::filter(
      is.na(.data$.trim_start) |
        (time_s >= .data$run_start_s + .data$.trim_start &
           time_s <= .data$run_end_s - .data$.trim_end)
    ) |>
    dplyr::select(-".trim_start", -".trim_end")
}

#' Average consecutive cycles
#'
#' Averages blocks of `n` consecutive cycles within each sample. A shorter
#' final block is kept. Blocks never span two samples (in v1 they could).
#'
#' @param raw Raw cycle data from [read_runfiles()].
#' @param n Number of cycles per block.
#' @return A tibble with one row per block and columns `sample_id`,
#'   `time_s`, `raw88` to `raw83` and `n_cycles`.
#' @export
integrate_cycles <- function(raw, n = 1L) {
  value_cols <- c("time_s", "raw88", "raw87", "raw86", "raw85", "raw84", "raw83")
  raw <- dplyr::mutate(raw, .block = (dplyr::row_number() - 1L) %/% as.integer(n), .by = sample_id)
  key <- paste(raw$sample_id, raw$.block, sep = "\r")
  key <- factor(key, levels = unique(key))

  # rowsum() sums all blocks in one vectorised pass.
  sums <- rowsum(as.matrix(raw[value_cols]), key, reorder = FALSE)
  counts <- as.vector(table(key))
  first <- !duplicated(key)

  tibble::tibble(sample_id = raw$sample_id[first]) |>
    dplyr::bind_cols(tibble::as_tibble(sums / counts)) |>
    dplyr::mutate(n_cycles = counts)
}

#' Mean blank voltages per sample
#'
#' Uses the cycles recorded before `blank_time_s` whose 88Sr signal is below
#' `raw88_min_v` (i.e. before the laser hits the sample).
#'
#' @param data Integrated cycle data.
#' @param blank_time_s Length of the blank period (s).
#' @param raw88_min_v 88Sr voltage below which a cycle counts as blank.
#' @return A tibble with `sample_id`, `blk88` to `blk83` and `n_blank`.
#' @export
summarise_blank <- function(data, blank_time_s, raw88_min_v) {
  data |>
    dplyr::filter(time_s <= blank_time_s, raw88 < raw88_min_v) |>
    dplyr::summarise(
      dplyr::across(raw88:raw83, mean),
      n_blank = dplyr::n(),
      .by = sample_id
    ) |>
    dplyr::rename_with(\(x) sub("^raw", "blk", x), raw88:raw83)
}

subtract_blank <- function(data, blank) {
  missing <- setdiff(unique(data$sample_id), blank$sample_id)
  if (length(missing) > 0) {
    warning("No blank cycles found for: ", paste(missing, collapse = ", "),
            ". These samples were not blank corrected; check the blank time ",
            "and minimum 88Sr voltage.", call. = FALSE)
  }
  data |>
    dplyr::left_join(blank, by = "sample_id") |>
    dplyr::mutate(
      dplyr::across(blk88:blk83, \(x) dplyr::coalesce(x, 0)),
      raw88 = raw88 - blk88,
      raw87 = raw87 - blk87,
      raw86 = raw86 - blk86,
      raw85 = raw85 - blk85,
      raw84 = raw84 - blk84,
      raw83 = raw83 - blk83
    ) |>
    dplyr::select(-(blk88:blk83), -"n_blank")
}

#' Rb interference and mass bias correction
#'
#' Mass bias is calculated per cycle from the measured 86Sr/88Sr ratio using
#' the exponential law, and applied to the Rb-corrected 87Sr/86Sr ratio. 87Rb
#' is estimated from 85Rb with the natural 85Rb/87Rb ratio, corrected for the
#' same mass bias.
#'
#' @param data Blank-corrected cycle data.
#' @param sr86_sr88_ref Reference 86Sr/88Sr ratio.
#' @param rb85_rb87 Natural 85Rb/87Rb ratio.
#' @return `data` with added ratio columns. `sr87_sr86` is the fully
#'   corrected ratio; `sr84_sr86_mb` is 84Sr/86Sr corrected for mass bias
#'   (v1 only reported the uncorrected `sr84_sr86`).
#' @export
correct_ratios <- function(data, sr86_sr88_ref = 0.1194, rb85_rb87 = 2.59265) {
  data |>
    dplyr::mutate(
      sr86_sr88 = raw86 / raw88,
      mass_bias = log(sr86_sr88_ref / sr86_sr88) / log(MASS[["sr86"]] / MASS[["sr88"]]),
      rb87 = (raw85 / rb85_rb87) * (MASS[["rb85"]] / MASS[["rb87"]])^mass_bias,
      sr87 = raw87 - rb87,
      sr87_sr86_rb = sr87 / raw86,
      sr87_sr86 = sr87_sr86_rb * (MASS[["sr87"]] / MASS[["sr86"]])^mass_bias,
      sr84_sr86 = raw84 / raw86,
      sr84_sr86_mb = sr84_sr86 * (MASS[["sr84"]] / MASS[["sr86"]])^mass_bias,
      rb85_sr88 = raw85 / raw88,
      total_sr = raw88 + raw86 + raw84 + sr87
    )
}

#' Reject outliers in 87Sr/86Sr
#'
#' Flags cycles that deviate by more than `sd` robust standard deviations
#' from the median of their neighbours (the `window` cycles around them,
#' without the cycle itself). The robust standard deviation is 1.4826 times
#' the median absolute deviation from that median, taken over a wider window
#' (at least 201 cycles, or the whole run if shorter) so it is stable.
#'
#' A rolling standard deviation (as in v1) is inflated by the outliers
#' themselves: clusters of spikes, and single spikes in short windows, could
#' not be found. With pure noise this filter flags close to the expected
#' share of points (0.27% at 3 SD). Where the robust spread is zero (many
#' identical values) the ordinary standard deviation is used. Rejected values
#' move from `sr87_sr86` to `sr87_sr86_outlier`.
#'
#' @param data Data from [correct_ratios()].
#' @param sd Number of (robust) standard deviations; 0 turns rejection off.
#' @param window Rolling window width (cycles).
#' @return `data` with `is_outlier` and `sr87_sr86_outlier` added.
#' @export
reject_outliers <- function(data, sd = 3, window = 20L) {
  if (sd <= 0) {
    return(dplyr::mutate(data, is_outlier = FALSE, sr87_sr86_outlier = NA_real_))
  }
  data |>
    dplyr::mutate(
      .median = roll_median_others(sr87_sr86, window),
      .sd = robust_spread(sr87_sr86 - .data$.median, max(201L, 5L * as.integer(window))),
      .sd = dplyr::if_else(.data$.sd > 0, .data$.sd, stats::sd(sr87_sr86 - .data$.median, na.rm = TRUE)),
      .by = sample_id
    ) |>
    dplyr::mutate(
      is_outlier = dplyr::coalesce(abs(sr87_sr86 - .data$.median) > sd * .data$.sd, FALSE),
      sr87_sr86_outlier = dplyr::if_else(is_outlier, sr87_sr86, NA_real_),
      sr87_sr86 = dplyr::if_else(is_outlier, NA_real_, sr87_sr86)
    ) |>
    dplyr::select(-".median", -".sd")
}

#' Moving average of 87Sr/86Sr
#'
#' Centred moving average with standard deviation, standard error and 95%
#' confidence interval. Missing values (e.g. rejected outliers) are skipped
#' and the standard error uses the number of values actually in each window
#' (v1 always divided by the full window width).
#'
#' @param data Data from [reject_outliers()].
#' @param window Window width (cycles).
#' @return `data` with `sr87_sr86_ma`, `_ma_sd`, `_ma_n`, `_ma_se`,
#'   `_ma_ci95_low` and `_ma_ci95_up` added.
#' @export
smooth_ma <- function(data, window = 10L) {
  data |>
    dplyr::mutate(
      sr87_sr86_ma = roll_stats(sr87_sr86, window)$mean,
      sr87_sr86_ma_sd = roll_stats(sr87_sr86, window)$sd,
      sr87_sr86_ma_n = roll_stats(sr87_sr86, window)$n,
      .by = sample_id
    ) |>
    dplyr::mutate(
      sr87_sr86_ma_se = sr87_sr86_ma_sd / sqrt(sr87_sr86_ma_n),
      sr87_sr86_ma_ci95_low = sr87_sr86_ma - 1.96 * sr87_sr86_ma_se,
      sr87_sr86_ma_ci95_up = sr87_sr86_ma + 1.96 * sr87_sr86_ma_se
    )
}

#' Adaptive spline fit of 87Sr/86Sr
#'
#' Fits a penalised regression spline against distance for each sample,
#' with the amount of smoothing chosen from the data:
#'
#' * The penalty is estimated by restricted maximum likelihood (`mgcv::bam`,
#'   `method = "fREML"`), so noisy profiles are smoothed more and clean ones
#'   less.
#' * The penalty is adaptive (`bs = "ad"`): it varies along the profile, so
#'   the curve stays smooth on plateaus but can follow sharp transitions.
#' * Points are weighted by their total Sr signal, so low-signal (noisier)
#'   stretches pull the curve less.
#' * The basis size grows with the profile length (one basis function per
#'   10 points, 10 to 80); it only caps how wiggly the curve can be.
#'
#' Profiles of fewer than 60 points get a thin plate spline (`bs = "tp"`)
#' instead, which needs fewer data. The fitted effective degrees of freedom
#' (how flexible the curve ended up, 1 = a straight line) are in
#' `sr87_sr86_spline_edf`.
#'
#' @param data Data from [reject_outliers()].
#' @return `data` with `sr87_sr86_spline`, `sr87_sr86_spline_se` (standard
#'   error of the fitted curve) and `sr87_sr86_spline_edf` added.
#' @export
smooth_spline <- function(data) {
  fit_one <- function(d) {
    ok <- !is.na(d$sr87_sr86) & !is.na(d$distance_um)
    n_ok <- sum(ok)
    if (n_ok < 5) {
      return(dplyr::mutate(d, sr87_sr86_spline = NA_real_, sr87_sr86_spline_se = NA_real_,
                           sr87_sr86_spline_edf = NA_real_))
    }
    fit_data <- d[ok, ]
    fit_data$w <- if ("total_sr" %in% names(d)) pmax(fit_data$total_sr, 1e-6) else 1
    fit_data$w <- fit_data$w / mean(fit_data$w)
    k <- as.integer(min(80, max(10, n_ok %/% 10), n_ok - 1))
    # Gaps (trims, rejected stretches) leave some basis functions without
    # data; the penalty handles them, so mgcv's warning about it is muted.
    # Profiles without scatter (e.g. constant values) can make the fit
    # fail; then a simpler spline, and finally a straight line, is used.
    attempt <- function(expr) tryCatch(suppressWarnings(expr), error = function(e) NULL)
    fit <- if (n_ok >= 60) {
      attempt(mgcv::bam(sr87_sr86 ~ s(distance_um, bs = "ad", k = k), data = fit_data,
                        weights = fit_data$w, method = "fREML"))
    }
    fit <- fit %||% attempt(mgcv::gam(sr87_sr86 ~ s(distance_um, bs = "tp", k = min(k, 20L)),
                                      data = fit_data, weights = fit_data$w, method = "REML"))
    fit <- fit %||% mgcv::gam(sr87_sr86 ~ distance_um, data = fit_data, weights = fit_data$w)
    pred <- stats::predict(fit, newdata = d, se.fit = TRUE)
    dplyr::mutate(d,
                  sr87_sr86_spline = as.numeric(pred$fit),
                  sr87_sr86_spline_se = as.numeric(pred$se.fit),
                  sr87_sr86_spline_edf = sum(fit$edf) - 1)
  }
  if (nrow(data) == 0) {
    # e.g. no cycles pass the 88Sr thresholds
    return(dplyr::mutate(data, sr87_sr86_spline = double(), sr87_sr86_spline_se = double(),
                         sr87_sr86_spline_edf = double()))
  }
  data |>
    split(~ factor(sample_id, levels = unique(sample_id))) |>
    purrr::map(fit_one) |>
    purrr::list_rbind()
}
