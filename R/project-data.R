#' Reduced data for project samples
#'
#' Reduces each sample with its own settings and applies its trims and
#' direction. Results are cached in the project's `cache/` folder, keyed by
#' the raw file, the sample's import and reduction settings, the run speed
#' and the IsoFishR version. A sample is only reduced again when one of
#' those changes, so adding samples or changing one sample's settings does
#' not reprocess the others.
#'
#' @param project A project from [open_project()].
#' @param sample_ids Samples to return; defaults to all runs that are not
#'   excluded or baseline runs.
#' @param edited Apply trims, reversal and re-zeroing from `samples.csv`.
#' @param calibrate Apply the reference material correction (see
#'   [calibration_model()]). The 87Sr/86Sr columns are then corrected, and
#'   `sr87_sr86_uncorrected`, `correction_factor`, `correction_u_rel` and
#'   `correction_status` are added. Without a primary reference material the
#'   values are unchanged and the factor is `NA`.
#' @return A tibble of reduced data. Any warnings raised during reduction
#'   (e.g. no blank cycles) are attached as the `"warnings"` attribute, a
#'   character vector named by sample.
#' @export
project_data <- function(project, sample_ids = NULL, edited = TRUE, calibrate = TRUE) {
  if (is.null(sample_ids)) {
    s <- project$samples
    sample_ids <- s$sample_id[s$status != "excluded" & s$run_type != "baseline"]
  }
  baseline_mode <- project$settings$calibration$baseline == "baseline_runs"
  runs <- if (baseline_mode) project_runs(project)
  baselines <- if (baseline_mode) project_baselines(project)

  results <- purrr::map(sample_ids, \(id) {
    if (!baseline_mode) return(reduce_project_sample(project, id))
    run <- runs[runs$sample_id == id, ]
    b <- interpolate_baseline(baselines, run$session, run$acquired, id)
    res <- reduce_project_sample(project, id, blank = b$blank)
    res$warnings <- c(res$warnings, b$note)
    res
  })
  data <- purrr::list_rbind(purrr::map(results, "data"))
  if (nrow(data) == 0) return(data)
  if (edited) data <- apply_edits(data, project$samples)
  if (calibrate) data <- apply_calibration(data, project, calibration_model(project))

  warns <- purrr::map(results, "warnings") |> rlang::set_names(sample_ids) |> unlist()
  if (length(warns) > 0) attr(data, "warnings") <- warns
  data
}

#' Region statistics for project samples
#'
#' @inheritParams project_data
#' @return One row per sample and region; see [summarise_regions()].
#' @export
project_regions <- function(project, sample_ids = NULL) {
  data <- project_data(project, sample_ids)
  summarise_regions(data, project$regions)
}

reduce_project_sample <- function(project, sample_id, blank = NULL) {
  i <- sample_index(project, sample_id)
  settings <- sample_settings(project, sample_id)
  key <- rlang::hash(list(
    md5 = project$samples$md5[i],
    import = import_values(settings$import),  # the preset name alone doesn't matter
    reduction = settings$reduction,
    run_speed = settings$sample$run_speed_um_s,
    blank = blank,
    version = isofishr_version()
  ))
  prefix <- paste0(rlang::hash(sample_id), "_")
  cache_dir <- project_file(project$path, "cache")
  cache_path <- file.path(cache_dir, paste0(prefix, key, ".rds"))

  if (file.exists(cache_path)) {
    cached <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (!is.null(cached)) return(cached)
  }

  warnings <- character()
  result <- withCallingHandlers({
    raw <- read_runfiles(project_file(project$path, project$samples$runfile[i]),
                         settings$import, sample_id)
    reduce_runs(raw, settings, blank = blank)
  }, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  result$warnings <- warnings

  # The cache is a convenience: a read-only project or a failed write just
  # means the sample is reduced again next time.
  if (!project$read_only && dir.exists(cache_dir)) {
    stale <- list.files(cache_dir, pattern = paste0("^", prefix), full.names = TRUE)
    unlink(setdiff(stale, cache_path))
    try(saveRDS(result, cache_path), silent = TRUE)
  }
  result
}
