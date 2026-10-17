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
#' @return A list with `data` (one row per integrated cycle) and `blank`
#'   (mean blank voltages per sample, or `NULL` without blank correction).
#' @export
reduce_runs <- function(raw, settings = default_settings(), blank = NULL) {
  check_settings(settings)
  r <- settings$reduction

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
    dplyr::mutate(
      distance_um = (time_s - min(time_s)) * settings$sample$run_speed_um_s,
      .by = sample_id
    ) |>
    correct_ratios(r$sr86_sr88_ref, r$rb85_rb87) |>
    reject_outliers(r$outlier_sd, r$outlier_window) |>
    smooth_ma(r$ma_window)

  if (r$smoothing == "spline") {
    data <- smooth_spline(data, r$spline_k)
  }
  data <- dplyr::mutate(data, sr87_sr86_value = switch(
    r$smoothing,
    none = sr87_sr86,
    ma = sr87_sr86_ma,
    spline = sr87_sr86_spline
  ))

  list(data = data, blank = blank)
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
correct_ratios <- function(data, sr86_sr88_ref = 0.1194, rb85_rb87 = 2.59712) {
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
#' Flags cycles that deviate from the rolling median by more than `sd`
#' rolling standard deviations. Rejected values move from `sr87_sr86` to
#' `sr87_sr86_outlier`.
#'
#' @param data Data from [correct_ratios()].
#' @param sd Number of standard deviations; 0 turns rejection off.
#' @param window Rolling window width (cycles).
#' @return `data` with `is_outlier` and `sr87_sr86_outlier` added.
#' @export
reject_outliers <- function(data, sd = 3, window = 20L) {
  if (sd <= 0) {
    return(dplyr::mutate(data, is_outlier = FALSE, sr87_sr86_outlier = NA_real_))
  }
  data |>
    dplyr::mutate(
      .median = roll_median(sr87_sr86, window, partial = FALSE),
      .sd = roll_stats(sr87_sr86, window, partial = FALSE)$sd,
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

#' Spline fit of 87Sr/86Sr
#'
#' Fits a thin plate regression spline (`mgcv::gam`) against distance for
#' each sample. This is the slowest reduction step, so [reduce_runs()] only
#' runs it when the smoothing setting is `"spline"`.
#'
#' @param data Data from [reject_outliers()].
#' @param k Basis dimension; reduced automatically for short profiles.
#' @return `data` with `sr87_sr86_spline` and `sr87_sr86_spline_se` added.
#' @export
smooth_spline <- function(data, k = 10L) {
  fit_one <- function(d) {
    n_ok <- sum(!is.na(d$sr87_sr86))
    kk <- min(k, n_ok - 1L)
    if (kk < 3) {
      return(dplyr::mutate(d, sr87_sr86_spline = NA_real_, sr87_sr86_spline_se = NA_real_))
    }
    fit <- mgcv::gam(sr87_sr86 ~ s(distance_um, k = kk, bs = "tp"), data = d)
    pred <- stats::predict(fit, newdata = d, se.fit = TRUE)
    dplyr::mutate(d,
                  sr87_sr86_spline = as.numeric(pred$fit),
                  sr87_sr86_spline_se = as.numeric(pred$se.fit))
  }
  data |>
    split(~ factor(sample_id, levels = unique(sample_id))) |>
    purrr::map(fit_one) |>
    purrr::list_rbind()
}
