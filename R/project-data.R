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

  # Blanks from baseline runs, per sample (NULL: the run's own blank).
  blanks <- purrr::map(sample_ids, \(id) {
    if (!baseline_mode) return(NULL)
    run <- runs[runs$sample_id == id, ]
    interpolate_baseline(baselines, run$session, run$acquired, id)
  })
  # Report progress for the runs that have to be reduced (not cached).
  todo <- !vapply(seq_along(sample_ids), \(i) {
    file.exists(sample_cache_path(project, sample_ids[i], blanks[[i]]$blank))
  }, logical(1))
  done <- 0L

  results <- purrr::map(seq_along(sample_ids), \(i) {
    id <- sample_ids[i]
    if (todo[i]) {
      done <<- done + 1L
      report_progress(done, sum(todo), id)
    }
    res <- reduce_project_sample(project, id, blank = blanks[[i]]$blank)
    res$warnings <- c(res$warnings, blanks[[i]]$note)
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

# Where a sample's reduced data is cached: named by the sample and by
# everything the reduction depends on.
sample_cache_path <- function(project, sample_id, blank = NULL) {
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
  file.path(project_file(project$path, "cache"), paste0(rlang::hash(sample_id), "_", key, ".rds"))
}

# Progress reporting for long reductions. The app sets a hook (see
# with_reduction_progress()); otherwise nothing is reported.
progress_env <- new.env(parent = emptyenv())

report_progress <- function(done, total, sample_id) {
  hook <- progress_env$hook
  if (is.function(hook)) try(hook(done, total, sample_id), silent = TRUE)
  invisible()
}

reduce_project_sample <- function(project, sample_id, blank = NULL) {
  i <- sample_index(project, sample_id)
  settings <- sample_settings(project, sample_id)
  cache_path <- sample_cache_path(project, sample_id, blank)
  cache_dir <- dirname(cache_path)
  prefix <- paste0(rlang::hash(sample_id), "_")

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
