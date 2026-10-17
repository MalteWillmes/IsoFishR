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
#' * `data.csv` (only with `include_data = TRUE`): every reduced point.
#'
#' @inheritParams project_summaries
#' @param include_data Also write the reduced data point by point.
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
    settings = project_settings_table(project, include_excluded)
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

# Round summary columns to a sensible precision for the CSV files: ratio
# means to 6 decimals, their spread to 7, distances and times to 0.1.
round_summary <- function(x) {
  x |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("mean", "mean_smoothed")), \(v) round(v, 6)),
      dplyr::across(dplyr::any_of(c("sd", "se")), \(v) round(v, 7)),
      dplyr::across(dplyr::matches("_um$|_s$") & dplyr::where(is.double), \(v) round(v, 1))
    )
}

export_samples <- function(project, include_excluded) {
  s <- project$samples
  if (include_excluded) s else dplyr::filter(s, .data$status != "excluded")
}
