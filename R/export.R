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

  samples |>
    dplyr::transmute(
      sample_id, source = .data$source, status = .data$status, flag = .data$flag,
      comment = .data$comment, added = .data$added, added_by = .data$added_by,
      trim_start_um = .data$trim_start_s * speed, trim_end_um = .data$trim_end_s * speed,
      reverse = .data$reverse,
      setting_overrides = .data$settings
    ) |>
    dplyr::left_join(stats, by = "sample_id") |>
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
    dplyr::left_join(points, by = c("sample_id", "region")) |>
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
    flat <- flatten_settings(list(reduction = s$reduction, sample = s$sample, import = s$import))
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
#' `NA` if rejected), `sr87_sr86_outlier` (rejected value), `sr87_sr86_smoothed`
#' and `sr87_sr86_smoothed_se` (by the sample's smoothing method, named in
#' `smoothing`; empty for `"none"`), `region` (region name(s) the point falls
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

  data |>
    dplyr::mutate(.spline_se = spline_se) |>
    dplyr::left_join(smoothing, by = "sample_id") |>
    dplyr::left_join(point_regions, by = c("sample_id", "time_s")) |>
    dplyr::left_join(cp, by = c("sample_id", "time_s")) |>
    dplyr::transmute(
      sample_id,
      distance_um,
      time_s,
      sr87_sr86,
      sr87_sr86_outlier,
      sr87_sr86_smoothed = dplyr::if_else(.data$smoothing == "none", NA_real_, sr87_sr86_value),
      sr87_sr86_smoothed_se = dplyr::case_when(
        .data$smoothing == "ma" ~ sr87_sr86_ma_se,
        .data$smoothing == "spline" ~ .data$.spline_se,
        .default = NA_real_
      ),
      smoothing = .data$smoothing,
      region = dplyr::coalesce(.data$region, ""),
      cp_segment = .data$cp_segment,
      cp_mean = .data$cp_mean,
      total_sr_v = .data$total_sr,
      kr83_v = raw83,
      rb85_sr88 = .data$rb85_sr88,
      sr84_sr86
    )
}

#' Export project summaries
#'
#' Writes the summary tables to a new time-stamped folder in the project's
#' `exports/` folder, together with `export_info.yml` (IsoFishR version,
#' date, user and options). Earlier exports are kept.
#'
#' Files written:
#' * `samples.csv`: [summarise_project_samples()]
#' * `regions.csv`: [summarise_project_regions()]
#' * `changepoints.csv`: [summarise_project_changepoints()]
#' * `settings.csv`: [project_settings_table()]
#' * `profiles.csv`: [project_profiles()], every point of every profile
#' * `data.csv` (only with `include_data = TRUE`): all reduction columns per
#'   point (raw voltages, mass bias, intermediate ratios), for checking the
#'   reduction itself.
#'
#' @inheritParams project_summaries
#' @param include_data Also write all reduction columns point by point.
#' @return The path of the export folder, invisibly.
#' @export
export_project <- function(project, include_excluded = FALSE, include_data = FALSE) {
  check_writable(project)
  samples <- export_samples(project, include_excluded)
  if (nrow(samples) == 0) stop("The project has no samples to export.", call. = FALSE)
  data <- project_data(project, samples$sample_id)

  stamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  dir <- project_file(project$path, "exports", stamp)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  tables <- list(
    samples = summarise_project_samples(project, data, include_excluded),
    regions = summarise_project_regions(project, data, include_excluded),
    changepoints = summarise_project_changepoints(project, data, include_excluded),
    settings = project_settings_table(project, include_excluded),
    profiles = project_profiles(project, data, include_excluded)
  )
  tables <- purrr::map(tables, round_summary)
  if (include_data) tables$data <- data
  purrr::iwalk(tables, \(x, name) write_table_file(x, file.path(dir, paste0(name, ".csv"))))

  yaml::write_yaml(list(
    project = project$meta$name,
    exported = timestamp_now(),
    exported_by = project$user,
    isofishr_version = isofishr_version(),
    include_excluded = include_excluded,
    include_data = include_data,
    samples = nrow(samples),
    files = paste0(names(tables), ".csv")
  ), file.path(dir, "export_info.yml"))

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
      dplyr::across(dplyr::any_of(c("mean", "mean_smoothed", "sr87_sr86", "sr87_sr86_outlier",
                                    "sr87_sr86_smoothed", "cp_mean", "sr84_sr86")), \(v) round(v, 6)),
      dplyr::across(dplyr::any_of(c("sd", "se", "sr87_sr86_smoothed_se", "rb85_sr88", "kr83_v")),
                    \(v) round(v, 7)),
      dplyr::across(dplyr::any_of("total_sr_v"), \(v) round(v, 5)),
      dplyr::across(dplyr::matches("_um$|_s$") & dplyr::where(is.double), \(v) round(v, 1))
    )
}

export_samples <- function(project, include_excluded) {
  s <- project$samples
  if (include_excluded) s else dplyr::filter(s, .data$status != "excluded")
}
