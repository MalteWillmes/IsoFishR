# Baseline runs and reference material correction.
#
# Runs are placed in time by their acquisition timestamp and grouped into
# analytical sessions (project_runs()). Everything below works within a
# session: baselines and the primary reference material correction are never
# interpolated across sessions.

# ---- baseline runs -------------------------------------------------------------

#' Baseline runs of a project
#'
#' Mean voltages of each baseline (gas blank) run, after its trims, with its
#' acquisition time and session. Used when `calibration$baseline` is
#' `"baseline_runs"`: the blank subtracted from a run is then interpolated
#' linearly in time between the baseline runs of its session, at the run's
#' start time (and held constant before the first and after the last).
#'
#' @param project A project from [open_project()].
#' @return A tibble with `sample_id`, `session`, `acquired`, `n_cycles` and
#'   `blk88` to `blk83`.
#' @export
project_baselines <- function(project) {
  runs <- project_runs(project)
  b <- dplyr::filter(runs, .data$run_type == "baseline", .data$status != "excluded")
  if (nrow(b) == 0) {
    return(tibble::tibble(sample_id = character(), session = character(),
                          acquired = as.POSIXct(character(), tz = "UTC"), n_cycles = integer(),
                          blk88 = double(), blk87 = double(), blk86 = double(),
                          blk85 = double(), blk84 = double(), blk83 = double()))
  }
  purrr::pmap(b, \(sample_id, session, acquired, ...) {
    i <- sample_index(project, sample_id)
    s <- sample_settings(project, sample_id)
    row <- project$samples[i, ]
    raw <- read_runfiles(project_file(project$path, row$runfile), s$import, sample_id)
    d <- integrate_cycles(raw, s$reduction$integration)
    d <- dplyr::filter(d, time_s >= min(time_s) + row$trim_start_s,
                       time_s <= max(time_s) - row$trim_end_s)
    tibble::tibble(sample_id = sample_id, session = session, acquired = acquired,
                   n_cycles = nrow(d), blk88 = mean(d$raw88), blk87 = mean(d$raw87),
                   blk86 = mean(d$raw86), blk85 = mean(d$raw85), blk84 = mean(d$raw84),
                   blk83 = mean(d$raw83))
  }) |>
    purrr::list_rbind()
}

# The blank for one run from the baseline runs of its session, or a note why
# there is none (the run then uses its own blank).
interpolate_baseline <- function(baselines, session, acquired, sample_id) {
  b <- dplyr::filter(baselines, .data$session == !!session, !is.na(.data$acquired))
  if (is.na(acquired)) {
    return(list(blank = NULL, note = "No acquisition time, so baseline runs can't be used; the run's own blank was used."))
  }
  if (nrow(b) == 0) {
    return(list(blank = NULL, note = paste0("No baseline runs in session ", session,
                                            "; the run's own blank was used.")))
  }
  t <- as.numeric(b$acquired)
  at <- as.numeric(acquired)
  interp <- function(v) if (length(v) == 1) v else stats::approx(t, v, xout = at, rule = 2)$y
  blank <- tibble::tibble(
    sample_id = sample_id,
    blk88 = interp(b$blk88), blk87 = interp(b$blk87), blk86 = interp(b$blk86),
    blk85 = interp(b$blk85), blk84 = interp(b$blk84), blk83 = interp(b$blk83)
  )
  list(blank = blank, note = NULL)
}

# ---- reference material correction ---------------------------------------------------

#' Reference material correction model
#'
#' Measures every run of the primary reference material (mean 87Sr/86Sr of
#' its points after trims and outlier rejection, uncorrected) and fits the
#' correction factor `accepted / measured` through time within each session,
#' using `calibration$fit`:
#'
#' * `"regression"` (default): a least-squares line through the primary
#'   runs' measured means against time (instrument drift); the factor is
#'   `accepted / predicted`. Its uncertainty is the standard error of the
#'   regression line at each time (with 2 runs, the runs' own SE).
#' * `"linear"`: linear interpolation between the primary runs bracketing
#'   each point (standard-sample bracketing); constant before the first and
#'   after the last primary run of the session.
#' * `"spline"`: a smoothing spline through the session's primary runs
#'   (needs at least 4; with fewer, linear interpolation is used).
#' * `"step"`: the factor of the most recent primary run ("step forward").
#' * `"mean"`: one factor per session, the mean of its primary runs.
#'
#' @param project A project from [open_project()].
#' @return `NULL` when no primary reference material is set; otherwise a list
#'   with `primary`, `accepted`, `uncertainty_2s`, `fit` and `points` (one
#'   row per primary run: `sample_id`, `session`, `time` (run midpoint),
#'   `measured`, `se`, `n`, `factor`, `u_rel`).
#' @export
calibration_model <- function(project) {
  cal <- project$settings$calibration
  if (!nzchar(cal$primary)) return(NULL)
  rm <- project$reference_materials[project$reference_materials$name == cal$primary, ]
  if (nrow(rm) == 0) return(NULL)

  # The model is needed for every run that is shown or exported, so it is
  # kept in memory, keyed by everything it depends on.
  key <- rlang::hash(list(project$path, calibration_inputs(project), isofishr_version()))
  if (!is.null(model_cache[[key]])) return(model_cache[[key]])
  model <- build_calibration_model(project, cal, rm)
  if (length(ls(model_cache)) > 20) rm(list = ls(model_cache), envir = model_cache)
  assign(key, model, envir = model_cache)
  model
}

model_cache <- new.env(parent = emptyenv())

build_calibration_model <- function(project, cal, rm) {
  runs <- project_runs(project)
  prim <- dplyr::filter(runs, .data$rm_role == "primary", .data$status != "excluded",
                        !is.na(.data$acquired))
  points <- tibble::tibble(sample_id = character(), session = character(), time = double(),
                           measured = double(), se = double(), n = integer(),
                           factor = double(), u_rel = double())
  if (nrow(prim) > 0) {
    d <- project_data(project, prim$sample_id, calibrate = FALSE)
    points <- d |>
      dplyr::summarise(
        measured = mean(sr87_sr86, na.rm = TRUE),
        n = sum(!is.na(sr87_sr86)),
        se = stats::sd(sr87_sr86, na.rm = TRUE) / sqrt(n),
        mid_s = (min(time_s) + max(time_s)) / 2,
        .by = sample_id
      ) |>
      dplyr::inner_join(dplyr::select(prim, "sample_id", "session", "acquired"), by = "sample_id") |>
      dplyr::transmute(
        sample_id, session = .data$session,
        time = as.numeric(.data$acquired) + .data$mid_s,
        measured = .data$measured, se = .data$se, n = .data$n,
        factor = rm$value / .data$measured,
        u_rel = .data$se / .data$measured
      ) |>
      dplyr::arrange(.data$time)
  }
  list(primary = rm$name, accepted = rm$value, uncertainty_2s = rm$uncertainty_2s,
       fit = cal$fit, points = points)
}

# Correction factor and its relative uncertainty (1 sigma, from the primary
# measurements and the accepted value) at times `t` (seconds) in a session.
correction_at <- function(model, session, t) {
  pts <- dplyr::filter(model$points, .data$session == !!session)
  n <- nrow(pts)
  u_rm <- if (is.na(model$uncertainty_2s)) 0 else model$uncertainty_2s / 2 / model$accepted
  if (n == 0 || all(is.na(t))) {
    return(list(factor = rep(NA_real_, length(t)), u_rel = rep(NA_real_, length(t)),
                status = "no primary reference material in session"))
  }
  tp <- pts$time
  fp <- pts$factor
  up <- pts$u_rel
  fit <- model$fit
  status <- "corrected"
  if (fit == "spline" && n < 4) {
    fit <- "linear"
    status <- "corrected (linear: spline needs 4+ primary runs)"
  }

  if (n == 1) {
    f <- rep(fp, length(t))
    u <- rep(up, length(t))
  } else if (fit == "regression") {
    # Centre time first: seconds since 1970 are large numbers.
    t0 <- mean(tp)
    line <- stats::lm(measured ~ x, data = data.frame(measured = pts$measured, x = tp - t0))
    pred <- stats::predict(line, newdata = data.frame(x = t - t0), se.fit = TRUE)
    f <- model$accepted / unname(pred$fit)
    u <- if (n > 2) unname(pred$se.fit / pred$fit) else rep(sqrt(mean(up^2)), length(t))
  } else if (fit == "linear") {
    f <- stats::approx(tp, fp, xout = t, rule = 2)$y
    i <- findInterval(t, tp, all.inside = TRUE)
    w <- pmin(pmax((t - tp[i]) / (tp[i + 1] - tp[i]), 0), 1)
    u <- sqrt(((1 - w) * up[i])^2 + (w * up[i + 1])^2)
  } else if (fit == "step") {
    i <- pmax(findInterval(t, tp), 1)
    f <- fp[i]
    u <- up[i]
  } else if (fit == "mean") {
    f <- rep(mean(fp), length(t))
    u <- rep(stats::sd(fp) / mean(fp) / sqrt(n), length(t))
  } else {
    sp <- stats::smooth.spline(tp, fp)
    f <- stats::predict(sp, t)$y
    resid <- (fp - stats::predict(sp, tp)$y) / fp
    u <- rep(sqrt(sum(resid^2) / max(n - sp$df, 1)) / sqrt(n), length(t))
  }
  list(factor = f, u_rel = sqrt(u^2 + u_rm^2), status = status)
}

# Everything the correction or baseline of any run depends on, without
# computing it: used by the app to tell when a plotted run needs to be
# recomputed. NULL when neither a primary reference material nor baseline
# runs are in use.
calibration_inputs <- function(project) {
  cal <- project$settings$calibration
  if (!nzchar(cal$primary) && cal$baseline == "per_run") return(NULL)
  runs <- project_runs(project)
  standards <- runs$sample_id[runs$run_type %in% c("reference", "baseline")]
  list(
    calibration = cal,
    reference_materials = project$reference_materials,
    runs = runs[, c("sample_id", "run_type", "reference_material", "session", "acquired")],
    standards = project$samples[project$samples$sample_id %in% standards,
                                c("sample_id", "md5", "settings", "trim_start_s", "trim_end_s",
                                  "status")],
    reduction = project$settings$reduction,
    import = import_values(project$settings$import)
  )
}

# 87Sr/86Sr columns that the correction factor applies to.
RATIO_COLUMNS <- c("sr87_sr86", "sr87_sr86_outlier", "sr87_sr86_value", "sr87_sr86_ma",
                   "sr87_sr86_ma_sd", "sr87_sr86_ma_se", "sr87_sr86_ma_ci95_low",
                   "sr87_sr86_ma_ci95_up", "sr87_sr86_spline", "sr87_sr86_spline_se")

# Apply the correction to reduced data, point by point at each point's
# acquisition time. Adds `sr87_sr86_uncorrected`, `correction_factor` (per
# point), `correction_u_rel` and `correction_status` (per run).
apply_calibration <- function(data, project, model) {
  if (is.null(model)) return(data)  # correction off: data unchanged
  runs <- project_runs(project)
  parts <- split(data, factor(data$sample_id, levels = unique(data$sample_id)))
  out <- purrr::map(parts, \(d) {
    run <- runs[runs$sample_id == d$sample_id[1], ]
    d$sr87_sr86_uncorrected <- d$sr87_sr86
    if (is.na(run$acquired)) {
      corr <- list(u_rel = NA_real_, status = "no acquisition time")
      f <- rep(NA_real_, nrow(d))
    } else {
      t0 <- as.numeric(run$acquired)
      corr <- correction_at(model, run$session, t0 + (min(d$time_s) + max(d$time_s)) / 2)
      f <- correction_at(model, run$session, t0 + d$time_s)$factor
    }
    for (col in intersect(RATIO_COLUMNS, names(d))) d[[col]] <- d[[col]] * dplyr::coalesce(f, 1)
    d$correction_factor <- f
    d$correction_u_rel <- corr$u_rel[1]
    d$correction_status <- corr$status
    d
  })
  purrr::list_rbind(out)
}

#' Reference material results
#'
#' One row per reference material run: the measured (uncorrected) and
#' corrected mean 87Sr/86Sr, standard errors with and without the correction
#' uncertainty, and the deviation from the accepted value. For secondary
#' reference materials this is the accuracy check of the correction; primary
#' runs are corrected to their accepted value by construction (exactly so
#' with the linear and step fits).
#'
#' @param project A project from [open_project()].
#' @return A tibble.
#' @export
reference_material_results <- function(project) {
  runs <- project_runs(project) |>
    dplyr::filter(.data$run_type == "reference", .data$status != "excluded")
  if (nrow(runs) == 0) {
    return(tibble::tibble(sample_id = character(), reference_material = character(),
                          role = character(), session = character()))
  }
  d <- project_data(project, runs$sample_id)
  if (!"correction_factor" %in% names(d)) {
    # No primary reference material yet: values are uncorrected.
    d <- dplyr::mutate(d, sr87_sr86_uncorrected = sr87_sr86, correction_factor = NA_real_,
                       correction_u_rel = NA_real_, correction_status = "off")
  }
  d |>
    dplyr::summarise(
      n = sum(!is.na(sr87_sr86)),
      measured = mean(.data$sr87_sr86_uncorrected, na.rm = TRUE),
      mean = mean(sr87_sr86, na.rm = TRUE),
      se = stats::sd(sr87_sr86, na.rm = TRUE) / sqrt(n),
      correction_factor = mean(.data$correction_factor, na.rm = TRUE),
      correction_u_rel = dplyr::first(.data$correction_u_rel),
      correction_status = dplyr::first(.data$correction_status),
      .by = sample_id
    ) |>
    dplyr::inner_join(dplyr::select(runs, "sample_id", "sequence", "reference_material", role = "rm_role",
                                    "session", "acquired"), by = "sample_id") |>
    dplyr::left_join(dplyr::select(project$reference_materials, reference_material = "name",
                                   accepted = "value", accepted_2s = "uncertainty_2s"),
                     by = "reference_material") |>
    dplyr::mutate(
      se_combined = sqrt(.data$se^2 + (.data$mean * dplyr::coalesce(.data$correction_u_rel, 0))^2),
      deviation = .data$mean - .data$accepted,
      deviation_ppm = .data$deviation / .data$accepted * 1e6,
      within_uncertainty = abs(.data$deviation) <=
        sqrt((2 * .data$se_combined)^2 + dplyr::coalesce(.data$accepted_2s, 0)^2)
    ) |>
    dplyr::arrange(.data$sequence) |>
    dplyr::select("sample_id", "sequence", "reference_material", "role", "session", "acquired", "n",
                  "measured", "mean", "se", "se_combined", "accepted", "accepted_2s",
                  "deviation", "deviation_ppm", "within_uncertainty", "correction_factor",
                  "correction_status")
}

#' @rdname reference_material_results
#' @return `summarise_reference_materials()`: one row per reference material
#'   and session: number of runs, mean and SD of the run means before and after
#'   correction, accepted value and deviation in ppm (of the corrected mean).
#' @export
summarise_reference_materials <- function(project) {
  r <- reference_material_results(project)
  if (nrow(r) == 0) return(r)
  sd_or_na <- function(x) if (length(x) > 1) stats::sd(x) else NA_real_
  r |>
    dplyr::summarise(
      role = dplyr::first(.data$role),
      runs = dplyr::n(),
      mean_uncorrected = mean(.data$measured),
      sd_uncorrected = sd_or_na(.data$measured),
      mean_corrected = mean(.data$mean),
      sd_corrected = sd_or_na(.data$mean),
      accepted = dplyr::first(.data$accepted),
      deviation_ppm = (mean(.data$mean) / dplyr::first(.data$accepted) - 1) * 1e6,
      runs_within_uncertainty = sum(.data$within_uncertainty),
      .by = c("reference_material", "session")
    )
}
