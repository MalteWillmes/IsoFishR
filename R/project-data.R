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
#' @param sample_ids Samples to return; defaults to all samples that are not
#'   excluded.
#' @param edited Apply trims, reversal and re-zeroing from `samples.csv`.
#' @return A tibble of reduced data. Any warnings raised during reduction
#'   (e.g. no blank cycles) are attached as the `"warnings"` attribute, a
#'   character vector named by sample.
#' @export
project_data <- function(project, sample_ids = NULL, edited = TRUE) {
  if (is.null(sample_ids)) {
    sample_ids <- project$samples$sample_id[project$samples$status != "excluded"]
  }
  results <- purrr::map(sample_ids, \(id) reduce_project_sample(project, id))
  data <- purrr::list_rbind(purrr::map(results, "data"))
  if (nrow(data) == 0) return(data)
  if (edited) data <- apply_edits(data, project$samples)

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

reduce_project_sample <- function(project, sample_id) {
  i <- sample_index(project, sample_id)
  settings <- sample_settings(project, sample_id)
  key <- rlang::hash(list(
    md5 = project$samples$md5[i],
    import = import_values(settings$import),  # the preset name alone doesn't matter
    reduction = settings$reduction,
    run_speed = settings$sample$run_speed_um_s,
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
    reduce_runs(raw, settings)
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
