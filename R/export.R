#' Project summaries
#'
#' Summary tables for the samples in a project, as written by
#' [export_project()].
#'
#' * `summarise_project_samples()`: one row per sample with its status,
#'   comment, edits, profile statistics and setting overrides.
#' * `summarise_project_regions()`: one row per sample and region.
#' * `summarise_project_changepoints()`: one row per changepoint segment, for
#'   samples with changepoint detection enabled.
#' * `project_settings_table()`: the settings each sample was reduced with.
#'
#' Means, standard deviations and standard errors are calculated from the
#' individual 87Sr/86Sr points (outliers removed). Smoothed values are
#' autocorrelated, so their standard error would understate the
#' uncertainty; the mean of the smoothed values is reported alongside as
#' `mean_smoothed`.
#'
#' When a primary reference material is set (see [calibration_model()]),
#' all 87Sr/86Sr values are corrected, and the tables add
#' `mean_uncorrected`, `correction_factor`, `correction_status` and
#' `se_combined`: the SE combined with the uncertainty of the correction.
#'
#' @param project A project from [open_project()].
#' @param data Reduced and edited data from [project_data()]; computed if
#'   not supplied.
#' @param include_excluded Include samples with status `"excluded"`.
#' @return A tibble.
#' @name project_summaries
NULL

#' @rdname project_summaries
#' @export
summarise_project_samples <- function(project, data = NULL, include_excluded = FALSE) {
  samples <- export_samples(project, include_excluded)
  if (nrow(samples) == 0) stop("The project has no samples to summarise.", call. = FALSE)
  data <- data %||% project_data(project, samples$sample_id)
  speed <- purrr::map_dbl(samples$sample_id, \(id) sample_settings(project, id)$sample$run_speed_um_s)
  # Trims as micrometres cut from the profile, as shown in the app.
  run_times <- if ("run_start_s" %in% names(data)) {
    dplyr::summarise(data, run_start = run_start_s[1], run_end = run_end_s[1],
                     profile_start = profile_start_s[1], profile_end = profile_end_s[1],
                     .by = sample_id)
  }
  trim_um <- function(ids, start_s, end_s, which) {
    purrr::pmap_dbl(list(ids, start_s, end_s, speed[match(ids, samples$sample_id)]), \(id, a, b, v) {
      tm <- if (!is.null(run_times)) run_times[run_times$sample_id == id, ]
      if (is.null(tm) || nrow(tm) == 0) return(if (which == "start") a * v else b * v)
      trims_to_um(a, b, tm, v)[[which]]
    })
  }

  stats <- data |>
    dplyr::summarise(
      n_points = sum(!is.na(sr87_sr86)),
      n_outliers = sum(is_outlier),
      distance_max_um = max(distance_um),
      mean = mean(sr87_sr86, na.rm = TRUE),
      sd = stats::sd(sr87_sr86, na.rm = TRUE),
      se = sd / sqrt(n_points),
      mean_smoothed = mean(sr87_sr86_value, na.rm = TRUE),
      .by = sample_id
    )
  n_regions <- dplyr::count(project$regions, sample_id, name = "n_regions")
  cp <- summarise_project_changepoints(project, data, include_excluded) |>
    dplyr::count(sample_id, name = "n_cp_segments")

  runs <- project_runs(project)
  samples |>
    dplyr::left_join(dplyr::select(runs, "sample_id", "session", acquired_at = "acquired"),
                     by = "sample_id") |>
    dplyr::transmute(
      sample_id, source = .data$source, run_type = .data$run_type,
      reference_material = .data$reference_material, session = .data$session.y,
      acquired = .data$acquired, status = .data$status, flag = .data$flag,
      comment = .data$comment, added = .data$added, added_by = .data$added_by,
      trim_start_um = trim_um(.data$sample_id, .data$trim_start_s, .data$trim_end_s, "start"),
      trim_end_um = trim_um(.data$sample_id, .data$trim_start_s, .data$trim_end_s, "end"),
      reverse = .data$reverse,
      setting_overrides = .data$settings
    ) |>
    dplyr::left_join(add_correction(stats, data), by = "sample_id") |>
    dplyr::left_join(n_regions, by = "sample_id") |>
    dplyr::left_join(cp, by = "sample_id") |>
    dplyr::mutate(dplyr::across(c("n_regions", "n_cp_segments"), \(x) dplyr::coalesce(x, 0L))) |>
    dplyr::relocate("setting_overrides", .after = dplyr::last_col())
}

#' @rdname project_summaries
#' @export
summarise_project_regions <- function(project, data = NULL, include_excluded = FALSE) {
  samples <- export_samples(project, include_excluded)
  regions <- dplyr::filter(project$regions, sample_id %in% samples$sample_id)
  if (nrow(regions) == 0) {
    return(tibble::tibble(sample_id = character(), region = character(), mean = double(),
                          sd = double(), n = integer(), se = double(), mean_smoothed = double(),
                          distance_min_um = double(), distance_max_um = double(),
                          start_s = double(), end_s = double()))
  }
  data <- data %||% project_data(project, unique(regions$sample_id))
  points <- summarise_regions(data, regions, value = "sr87_sr86")
  smoothed <- summarise_regions(data, regions) |>
    dplyr::select("sample_id", "region", mean_smoothed = "mean")

  regions |>
    dplyr::select("sample_id", "region", "start_s", "end_s") |>
    dplyr::left_join(add_correction(points, data), by = c("sample_id", "region")) |>
    dplyr::left_join(smoothed, by = c("sample_id", "region")) |>
    dplyr::relocate("start_s", "end_s", .after = dplyr::last_col()) |>
    dplyr::relocate("mean_smoothed", .after = "se")
}

#' @rdname project_summaries
#' @export
summarise_project_changepoints <- function(project, data = NULL, include_excluded = FALSE) {
  samples <- export_samples(project, include_excluded) |> dplyr::filter(.data$cp_enabled)
  empty <- tibble::tibble(sample_id = character(), segment = integer(), mean = double(),
                          sd = double(), n = integer(), se = double(),
                          distance_min_um = double(), distance_max_um = double(),
                          method = character(), penalty = double())
  if (nrow(samples) == 0) return(empty)
  data <- data %||% project_data(project, samples$sample_id)

  purrr::pmap(samples, \(sample_id, cp_method, cp_penalty, ...) {
    d <- dplyr::filter(data, .data$sample_id == !!sample_id)
    if (nrow(d) < 3) return(NULL)
    find_changepoints(d, cp_method, cp_penalty) |>
      dplyr::filter(!is.na(cp_segment)) |>
      dplyr::rename(segment = "cp_segment") |>
      dplyr::summarise(
        mean = mean(sr87_sr86, na.rm = TRUE),
        sd = stats::sd(sr87_sr86, na.rm = TRUE),
        n = sum(!is.na(sr87_sr86)),
        se = sd / sqrt(n),
        distance_min_um = min(distance_um),
        distance_max_um = max(distance_um),
        .by = c(sample_id, segment)
      ) |>
      add_correction(d) |>
      dplyr::mutate(method = cp_method, penalty = cp_penalty)
  }) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(empty)
}

#' @rdname project_summaries
#' @export
project_settings_table <- function(project, include_excluded = FALSE) {
  samples <- export_samples(project, include_excluded)
  purrr::map(samples$sample_id, \(id) {
    s <- sample_settings(project, id)
    flat <- flatten_settings(list(reduction = s$reduction, sample = s$sample, import = s$import,
                                  calibration = s$calibration))
    tibble::tibble(sample_id = id, !!!as.list(flat))
  }) |>
    purrr::list_rbind()
}

#' Full profiles of all samples
#'
#' One row per measurement point for every sample, after trims and reversal:
#' the profile as plotted in the app, in a single table.
#'
#' Columns: `sample_id`, `distance_um`, `time_s`, `sr87_sr86` (the point,
#' `NA` if rejected), `sr87_sr86_outlier` (rejected value), `sr87_sr86_ma` and
#' `sr87_sr86_ma_se` (moving average and its SE) and `sr87_sr86_spline` and
#' `sr87_sr86_spline_se` (spline and its SE), each filled when the sample's
#' smoothing method (in `smoothing`) uses it, `region` (region name(s) the point falls
#' in, separated by `;`), `cp_segment` and `cp_mean` (for samples with
#' changepoints enabled), and the signal columns `total_sr_v`, `kr83_v`,
#' `rb85_sr88` and `sr84_sr86`.
#'
#' @inheritParams project_summaries
#' @return A tibble.
#' @export
project_profiles <- function(project, data = NULL, include_excluded = FALSE) {
  samples <- export_samples(project, include_excluded)
  if (nrow(samples) == 0) stop("The project has no samples to export.", call. = FALSE)
  data <- data %||% project_data(project, samples$sample_id)
  data <- dplyr::filter(data, sample_id %in% samples$sample_id)

  smoothing <- tibble::tibble(
    sample_id = samples$sample_id,
    smoothing = purrr::map_chr(samples$sample_id, \(id) sample_settings(project, id)$reduction$smoothing)
  )
  spline <- if ("sr87_sr86_spline" %in% names(data)) data$sr87_sr86_spline else NA_real_
  spline_se <- if ("sr87_sr86_spline_se" %in% names(data)) data$sr87_sr86_spline_se else NA_real_

  # Changepoint segments for samples that have them enabled
  cp_samples <- dplyr::filter(samples, .data$cp_enabled)
  cp <- purrr::pmap(cp_samples, \(sample_id, cp_method, cp_penalty, ...) {
    d <- dplyr::filter(data, .data$sample_id == !!sample_id)
    if (nrow(d) < 3) return(NULL)
    find_changepoints(d, cp_method, cp_penalty) |>
      dplyr::select("sample_id", "time_s", "cp_segment", "cp_mean")
  }) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(tibble::tibble(sample_id = character(), time_s = double(),
                                    cp_segment = integer(), cp_mean = double()))

  # Region name(s) per point
  regions <- dplyr::filter(project$regions, sample_id %in% samples$sample_id)
  point_regions <- data |>
    dplyr::select("sample_id", "time_s") |>
    dplyr::inner_join(regions, by = dplyr::join_by(sample_id, dplyr::between(time_s, start_s, end_s))) |>
    dplyr::summarise(region = paste(region, collapse = "; "), .by = c(sample_id, time_s))

  out <- data |>
    dplyr::mutate(.spline = spline, .spline_se = spline_se) |>
    dplyr::left_join(smoothing, by = "sample_id") |>
    dplyr::left_join(point_regions, by = c("sample_id", "time_s")) |>
    dplyr::left_join(cp, by = c("sample_id", "time_s")) |>
    dplyr::transmute(
      sample_id,
      distance_um,
      time_s,
      sr87_sr86,
      sr87_sr86_outlier,
      sr87_sr86_ma = dplyr::if_else(.data$smoothing %in% c("ma", "ma_spline"), sr87_sr86_ma, NA_real_),
      sr87_sr86_ma_se = dplyr::if_else(.data$smoothing %in% c("ma", "ma_spline"), sr87_sr86_ma_se, NA_real_),
      sr87_sr86_spline = dplyr::if_else(.data$smoothing %in% c("spline", "ma_spline"), .data$.spline, NA_real_),
      sr87_sr86_spline_se = dplyr::if_else(.data$smoothing %in% c("spline", "ma_spline"),
                                           .data$.spline_se, NA_real_),
      smoothing = .data$smoothing,
      region = dplyr::coalesce(.data$region, ""),
      cp_segment = .data$cp_segment,
      cp_mean = .data$cp_mean,
      total_sr_v = .data$total_sr,
      kr83_v = raw83,
      rb85_sr88 = .data$rb85_sr88,
      sr84_sr86
    )
  # The joins above keep the row order of `data`.
  if ("correction_factor" %in% names(data)) {
    out <- tibble::add_column(out, sr87_sr86_uncorrected = data$sr87_sr86_uncorrected,
                              correction_factor = data$correction_factor,
                              .after = "sr87_sr86_outlier")
  }
  out
}

#' Export project summaries
#'
#' Writes the summary tables to a new time-stamped folder in the project's
#' `exports/` folder, together with `export_report.md` (a record of the
#' settings, calibration and reference materials). Earlier exports are kept.
#'
#' Files written:
#' * `samples.csv`: [summarise_project_samples()]
#' * `regions.csv`: [summarise_project_regions()]
#' * `changepoints.csv`: [summarise_project_changepoints()]
#' * `settings.csv`: [project_settings_table()]
#' * `profiles.csv`: [project_profiles()], every point of every profile
#' * `runs.csv`: [project_runs()], all runs in acquisition order with type
#'   and session
#' * `reference_materials.csv` and `reference_material_summary.csv` (when
#'   the project has reference material runs):
#'   [reference_material_results()] and [summarise_reference_materials()]
#' * `export_report.md` (always): [export_report()] as Markdown, a record of the export:
#'   all project settings, the calibration method, the primary and secondary
#'   reference materials with their number of runs, mean and SD, the
#'   sessions, and per-sample setting overrides.
#' * `data.csv` (not by default): all reduction columns per point (raw
#'   voltages, mass bias, intermediate ratios), for checking the reduction
#'   itself.
#'
#' @inheritParams project_summaries
#' @param files Which files to write, any of `EXPORT_FILES`: `"samples"`,
#'   `"regions"`, `"changepoints"`, `"profiles"`, `"runs"`,
#'   `"reference_materials"`, `"settings"`, `"data"`. Defaults to all but
#'   `"data"`.
#' @return The path of the export folder, invisibly.
#' @export
export_project <- function(project, files = setdiff(EXPORT_FILES, "data"),
                           include_excluded = FALSE) {
  files <- match.arg(files, EXPORT_FILES, several.ok = TRUE)
  check_writable(project)
  samples <- export_samples(project, include_excluded)
  if (nrow(samples) == 0) stop("The project has no samples to export.", call. = FALSE)
  data <- project_data(project, samples$sample_id)

  stamp <- unique_stamp(project_file(project$path, "exports"), "%Y-%m-%d_%H%M%S")
  dir <- project_file(project$path, "exports", stamp)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  # Only the chosen tables are computed.
  build <- list(
    samples = \() summarise_project_samples(project, data, include_excluded),
    regions = \() summarise_project_regions(project, data, include_excluded),
    changepoints = \() summarise_project_changepoints(project, data, include_excluded),
    profiles = \() project_profiles(project, data, include_excluded),
    runs = \() dplyr::mutate(project_runs(project), acquired = format_time(.data$acquired)),
    settings = \() project_settings_table(project, include_excluded)
  )
  tables <- purrr::map(build[intersect(names(build), files)], \(f) f())
  if ("reference_materials" %in% files && any(project$samples$run_type == "reference")) {
    tables$reference_materials <- reference_material_results(project) |>
      dplyr::mutate(acquired = format_time(.data$acquired))
    tables$reference_material_summary <- summarise_reference_materials(project)
  }
  tables <- purrr::map(tables, round_summary)
  if ("data" %in% files) tables$data <- data
  purrr::iwalk(tables, \(x, name) write_table_file(x, file.path(dir, paste0(name, ".csv"))))

  report <- export_report(project, files = paste0(names(tables), ".csv"),
                          include_excluded = include_excluded, n_samples = nrow(samples))
  writeLines(enc2utf8(report_markdown(report)), file.path(dir, "export_report.md"), useBytes = TRUE)

  append_log(project_file(project$path, "log.csv"),
             log_entry(project$user, "export", new = file.path("exports", stamp)))
  invisible(dir)
}

# Round columns to a sensible precision for the CSV files: 87Sr/86Sr values
# and means to 6 decimals, their spread and the small signal ratios to 7,
# voltages to 5, distances and times to 0.1.
round_summary <- function(x) {
  x |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("mean", "mean_smoothed", "mean_uncorrected", "mean_corrected", "sr87_sr86",
                                    "sr87_sr86_outlier", "sr87_sr86_uncorrected",
                                    "sr87_sr86_ma", "sr87_sr86_spline", "cp_mean", "sr84_sr86", "measured",
                                    "accepted")), \(v) round(v, 6)),
      dplyr::across(dplyr::any_of(c("sd", "sd_uncorrected", "sd_corrected", "se", "se_combined", "sr87_sr86_ma_se", "sr87_sr86_spline_se",
                                    "rb85_sr88", "kr83_v", "deviation", "accepted_2s")),
                    \(v) round(v, 7)),
      dplyr::across(dplyr::any_of("correction_factor"), \(v) round(v, 8)),
      dplyr::across(dplyr::any_of("deviation_ppm"), \(v) round(v, 1)),
      dplyr::across(dplyr::any_of("total_sr_v"), \(v) round(v, 5)),
      dplyr::across(dplyr::matches("_um$|_s$") & dplyr::where(is.double), \(v) round(v, 1))
    )
}

# Runs that have data to summarise: not baseline runs, and not excluded
# unless asked for.
export_samples <- function(project, include_excluded) {
  s <- dplyr::filter(project$samples, .data$run_type != "baseline")
  if (include_excluded) s else dplyr::filter(s, .data$status != "excluded")
}

# Add the reference material correction to a per-sample summary table (with
# `sample_id`, `mean` and `se`): the uncorrected mean, the mean correction
# factor, its status, and the SE combined with the correction uncertainty.
# Tables are returned unchanged when the correction is off.
add_correction <- function(tbl, data) {
  if (!"correction_factor" %in% names(data)) return(tbl)
  info <- data |>
    dplyr::summarise(
      correction_factor = mean(.data$correction_factor, na.rm = TRUE),
      correction_u_rel = dplyr::first(.data$correction_u_rel),
      correction_status = dplyr::first(.data$correction_status),
      .by = sample_id
    )
  tbl |>
    dplyr::left_join(info, by = "sample_id") |>
    dplyr::mutate(
      mean_uncorrected = .data$mean / dplyr::coalesce(.data$correction_factor, 1),
      se_combined = sqrt(.data$se^2 + (.data$mean * dplyr::coalesce(.data$correction_u_rel, 0))^2),
      .after = "se"
    ) |>
    dplyr::select(-"correction_u_rel")
}

#' @rdname export_project
#' @format NULL
#' @export
EXPORT_FILES <- c("samples", "regions", "changepoints", "profiles", "runs",
                  "reference_materials", "settings", "data")

#' Record of an export
#'
#' Everything needed to understand and reproduce the exported values, as
#' written to `export_report.md` by [export_project()]: the project, IsoFishR
#' version and user; the runs and sessions; the calibration (method, baseline
#' mode, session gap, and for the primary and each secondary reference
#' material its accepted value, number of runs and the mean and SD of its run
#' means before and after correction, overall and per session); all project
#' settings; and the samples with their own setting overrides.
#'
#' @param project A project from [open_project()].
#' @param files Names of the files written with it.
#' @param include_excluded Whether excluded samples were exported.
#' @param n_samples Number of samples exported.
#' @return A nested list.
#' @export
export_report <- function(project, files = character(), include_excluded = FALSE,
                          n_samples = NA_integer_) {
  runs <- project_runs(project)
  cal <- project$settings$calibration
  in_use <- runs$status != "excluded"

  sessions <- runs |>
    dplyr::filter(in_use) |>
    dplyr::summarise(runs = dplyr::n(),
                     first_run = time_range(.data$acquired)[1],
                     last_run = time_range(.data$acquired)[2], .by = "session")

  rm_results <- if (any(runs$run_type == "reference")) reference_material_results(project) else NULL
  rm_summary <- if (!is.null(rm_results) && nrow(rm_results) > 0) summarise_reference_materials(project)
  describe_rm <- function(name) {
    rm <- project$reference_materials[project$reference_materials$name == name, ]
    r <- if (is.null(rm_results)) rm_results else rm_results[rm_results$reference_material == name, ]
    sd_or_na <- function(x) if (length(x) > 1) stats::sd(x) else NA_real_
    out <- list(name = name, accepted = rm$value, accepted_2s = rm$uncertainty_2s, note = rm$note,
                runs = if (is.null(r)) 0L else nrow(r))
    if (!is.null(r) && nrow(r) > 0) {
      out <- c(out, list(
        mean_uncorrected = mean(r$measured), sd_uncorrected = sd_or_na(r$measured),
        mean_corrected = mean(r$mean), sd_corrected = sd_or_na(r$mean),
        deviation_ppm = (mean(r$mean) / rm$value - 1) * 1e6,
        runs_within_uncertainty = sum(r$within_uncertainty),
        per_session = rm_summary |>
          dplyr::filter(.data$reference_material == name) |>
          dplyr::select("session", "runs", "mean_uncorrected", "sd_uncorrected",
                        "mean_corrected", "sd_corrected", "deviation_ppm") |>
          purrr::transpose()
      ))
    }
    out
  }
  secondary <- setdiff(project$reference_materials$name, cal$primary)

  overrides <- project$samples |>
    dplyr::filter(nzchar(.data$settings)) |>
    dplyr::select("sample_id", "settings")

  list(
    project = project$meta$name,
    exported = timestamp_now(),
    exported_by = project$user,
    isofishr_version = isofishr_version(),
    files = c(files, "export_report.md"),
    include_excluded = include_excluded,
    samples = n_samples,
    runs = list(
      total = nrow(runs),
      samples = sum(runs$run_type == "sample"),
      reference_material_runs = sum(runs$run_type == "reference"),
      baseline_runs = sum(runs$run_type == "baseline"),
      excluded = sum(!in_use)
    ),
    sessions = purrr::transpose(sessions),
    calibration = list(
      correction = if (nzchar(cal$primary)) "on" else "off (values are uncorrected)",
      method = cal$fit,
      method_description = unname(FIT_METHOD_NAMES[cal$fit]),
      baseline = cal$baseline,
      baseline_description = c(per_run = "Each run's own blank (start of run)",
                               baseline_runs = "Baseline runs, interpolated in time")[[cal$baseline]],
      session_gap_h = cal$session_gap_h,
      primary = if (nzchar(cal$primary)) describe_rm(cal$primary),
      secondary = purrr::map(secondary, describe_rm)
    ),
    settings = project$settings[c("sample", "import", "reduction", "regions")],
    sample_setting_overrides = as.list(rlang::set_names(overrides$settings, overrides$sample_id))
  )
}

# First and last time as text ("" when there are no timestamps).
time_range <- function(t) {
  t <- t[!is.na(t)]
  if (length(t) == 0) return(c("", ""))
  format_time(range(t))
}
