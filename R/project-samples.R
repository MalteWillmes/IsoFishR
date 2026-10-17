#' Add runfiles to a project
#'
#' Copies runfiles into the project's `raw/` folder and registers them as new
#' samples. Each file is read first with the project's import settings, so a
#' wrong layout is caught before anything is copied. Files whose contents are
#' already in the project are skipped.
#'
#' @param project A project from [open_project()].
#' @param paths Paths to runfiles.
#' @param sample_ids Sample IDs; default to the file names without extension.
#' @param source_names Original file names to record (useful when `paths`
#'   are temporary upload paths).
#' @return The updated project.
#' @export
add_runfiles <- function(project, paths, sample_ids = NULL, source_names = basename(paths)) {
  check_writable(project)
  if (is.null(sample_ids)) sample_ids <- tools::file_path_sans_ext(source_names)
  stopifnot(length(sample_ids) == length(paths), length(source_names) == length(paths))

  bad <- grepl("[\\\\/:*?\"<>|]", sample_ids) | !nzchar(trimws(sample_ids))
  if (any(bad)) {
    stop("Sample IDs cannot be empty or contain \\ / : * ? \" < > |: ",
         paste(sample_ids[bad], collapse = ", "), call. = FALSE)
  }

  md5 <- unname(tools::md5sum(paths))
  known <- md5 %in% project$samples$md5 | duplicated(md5)
  if (any(known)) {
    message("Skipped (already in the project): ", paste(source_names[known], collapse = ", "))
  }
  paths <- paths[!known]
  sample_ids <- sample_ids[!known]
  source_names <- source_names[!known]
  md5 <- md5[!known]
  if (length(paths) == 0) return(project)

  taken <- sample_ids %in% project$samples$sample_id | duplicated(sample_ids)
  if (any(taken)) {
    stop("Sample ID(s) already used in this project: ",
         paste(unique(sample_ids[taken]), collapse = ", "),
         ". Rename the files or pass different `sample_ids`.", call. = FALSE)
  }

  # Fails with a clear message if the files don't match the import settings.
  read_runfiles(paths, project$settings$import, sample_ids)

  runfiles <- file.path("raw", paste0(sample_ids, tolower(file_ext_dot(source_names))))
  ok <- file.copy(paths, project_file(project$path, runfiles), overwrite = FALSE)
  if (!all(ok)) {
    stop("Could not copy: ", paste(source_names[!ok], collapse = ", "), call. = FALSE)
  }

  new_rows <- tibble::tibble(
    sample_id = sample_ids, runfile = runfiles, source = source_names, md5 = md5,
    added = timestamp_now(), added_by = project$user, !!!sample_defaults()
  )
  project$samples <- dplyr::bind_rows(project$samples, new_rows)
  write_table_file(project$samples, project_file(project$path, "samples.csv"))
  append_log(project_file(project$path, "log.csv"),
             log_entry(project$user, "add_sample", sample_ids, new = source_names))
  project
}

#' Change a sample's status, comment or edits
#'
#' @param project A project from [open_project()].
#' @param sample_id One sample ID.
#' @param ... Named values for any of `status` (`"new"`, `"reviewed"`,
#'   `"excluded"`), `flag`, `comment`, `trim_start_s`, `trim_end_s`,
#'   `reverse`, `rezero`, `cp_enabled`, `cp_method`, `cp_penalty`.
#' @return The updated project.
#' @export
update_sample <- function(project, sample_id, ...) {
  check_writable(project)
  i <- sample_index(project, sample_id)
  changes <- list(...)
  unknown <- setdiff(names(changes), EDITABLE_SAMPLE_FIELDS)
  if (length(unknown) > 0) {
    stop("Cannot change: ", paste(unknown, collapse = ", "), ". Editable fields: ",
         paste(EDITABLE_SAMPLE_FIELDS, collapse = ", "), call. = FALSE)
  }
  if ("status" %in% names(changes) && !changes$status %in% SAMPLE_STATUS) {
    stop("status must be one of: ", paste(SAMPLE_STATUS, collapse = ", "), call. = FALSE)
  }
  if ("cp_method" %in% names(changes) &&
      !changes$cp_method %in% c("PELT", "AMOC", "SegNeigh", "BinSeg")) {
    stop("cp_method must be one of PELT, AMOC, SegNeigh, BinSeg.", call. = FALSE)
  }

  log <- list()
  for (field in names(changes)) {
    old <- project$samples[[field]][i]
    new <- coerce_like(changes[[field]], old)
    if (length(new) != 1 || is.na(new)) {
      stop("`", field, "` must be a single ", typeof(old), " value.", call. = FALSE)
    }
    if (!identical(old, new)) {
      project$samples[[field]][i] <- new
      log[[field]] <- log_entry(project$user, "update_sample", sample_id, field, old, new)
    }
  }
  save_sample_changes(project, log)
}

#' Per-sample reduction settings
#'
#' `set_sample_settings()` overrides project reduction settings for one
#' sample (e.g. a different blank time). Pass `NULL` to go back to the
#' project value. `sample_settings()` returns the settings a sample is
#' reduced with.
#'
#' @param project A project from [open_project()].
#' @param sample_id One sample ID.
#' @param ... Named reduction settings, e.g. `blank_time_s = 25`.
#' @return `set_sample_settings()`: the updated project.
#'   `sample_settings()`: a settings list.
#' @export
set_sample_settings <- function(project, sample_id, ...) {
  check_writable(project)
  i <- sample_index(project, sample_id)
  changes <- list(...)
  defaults <- project$settings$reduction
  unknown <- setdiff(names(changes), names(defaults))
  if (length(unknown) > 0) {
    stop("Unknown reduction setting(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }

  old <- parse_overrides(project$samples$settings[i])
  new <- old
  for (key in names(changes)) {
    new[[key]] <- if (is.null(changes[[key]])) NULL else coerce_like(changes[[key]], defaults[[key]])
  }
  # Validate the combination before saving anything.
  merge_settings(project$settings, list(reduction = new))

  log <- purrr::map(names(changes), \(key) {
    if (identical(old[[key]], new[[key]])) return(NULL)
    log_entry(project$user, "sample_setting", sample_id, key,
              old = old[[key]] %||% paste0("(project: ", defaults[[key]], ")"),
              new = new[[key]] %||% paste0("(project: ", defaults[[key]], ")"))
  })
  project$samples$settings[i] <- format_overrides(new[sort(names(new))])
  save_sample_changes(project, log)
}

#' @rdname set_sample_settings
#' @export
sample_settings <- function(project, sample_id) {
  i <- sample_index(project, sample_id)
  merge_settings(project$settings,
                 list(reduction = parse_overrides(project$samples$settings[i])))
}

#' Set the regions of a sample
#'
#' Replaces all regions of one sample. Regions are given in seconds of
#' analysis time (the `time_s` column of reduced data), so they stay in place
#' if the run speed changes.
#'
#' @param project A project from [open_project()].
#' @param sample_id One sample ID.
#' @param regions A data frame with columns `region`, `start_s` and `end_s`;
#'   an empty data frame removes all regions.
#' @return The updated project.
#' @export
set_regions <- function(project, sample_id, regions) {
  check_writable(project)
  sample_index(project, sample_id)
  new <- tibble::tibble(
    sample_id = rep(sample_id, nrow(regions)),
    region = as.character(regions$region),
    start_s = as.numeric(regions$start_s),
    end_s = as.numeric(regions$end_s)
  )
  if (any(new$start_s >= new$end_s)) stop("Each region must start before it ends.", call. = FALSE)
  if (anyDuplicated(new$region)) stop("Region names must be unique within a sample.", call. = FALSE)

  old <- dplyr::filter(project$regions, .data$sample_id == !!sample_id)
  if (identical(format_regions(old), format_regions(new))) return(project)

  project$regions <- project$regions |>
    dplyr::filter(.data$sample_id != !!sample_id) |>
    dplyr::bind_rows(new)
  write_table_file(project$regions, project_file(project$path, "regions.csv"))
  append_log(project_file(project$path, "log.csv"),
             log_entry(project$user, "set_regions", sample_id, "regions",
                       format_regions(old), format_regions(new)))
  project
}

#' Change project settings
#'
#' @param project A project from [open_project()].
#' @param overrides A nested list of settings to change, e.g.
#'   `list(reduction = list(ma_window = 15L))`.
#' @return The updated project.
#' @export
update_project_settings <- function(project, overrides) {
  check_writable(project)
  new <- merge_settings(project$settings, overrides)
  old_flat <- flatten_settings(project$settings)
  new_flat <- flatten_settings(new)
  keys <- union(names(old_flat), names(new_flat))
  changed <- keys[old_flat[keys] %|% "" != new_flat[keys] %|% ""]
  if (length(changed) == 0) return(project)

  project$settings <- new
  write_yaml_file(c(list(project = project$meta), new), project_file(project$path, "project.yml"))
  append_log(project_file(project$path, "log.csv"),
             log_entry(project$user, "project_setting", "", changed,
                       old_flat[changed], new_flat[changed]))
  project
}

# ---- helpers ---------------------------------------------------------------------

sample_index <- function(project, sample_id) {
  i <- match(sample_id, project$samples$sample_id)
  if (length(sample_id) != 1 || is.na(i)) {
    stop("Unknown sample: ", paste(sample_id, collapse = ", "), call. = FALSE)
  }
  i
}

save_sample_changes <- function(project, log) {
  log <- purrr::list_rbind(purrr::compact(unname(log)))
  if (nrow(log) == 0) return(project)
  write_table_file(project$samples, project_file(project$path, "samples.csv"))
  append_log(project_file(project$path, "log.csv"), log)
  project
}

file_ext_dot <- function(x) {
  ext <- tools::file_ext(x)
  ifelse(nzchar(ext), paste0(".", ext), "")
}

`%|%` <- function(x, y) ifelse(is.na(x), y, x)
`%||%` <- function(x, y) if (is.null(x)) y else x
