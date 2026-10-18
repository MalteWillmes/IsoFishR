#' Add runfiles to a project
#'
#' Copies runfiles into the project's `raw/` folder and registers them as new
#' runs, so a project can be built up batch by batch. Each file is read first
#' with the project's import settings, so a wrong layout is caught before
#' anything is copied.
#'
#' Files identical to one already in the project are always skipped. For a
#' file whose name matches a run already in the project but whose content
#' differs (e.g. a re-exported runfile):
#'
#' * `mode = "add"` skips it and reports it;
#' * `mode = "replace"` gives that run the new file. Its edits, regions,
#'   comment and settings are kept; the previous file is moved to
#'   `raw/removed/` and the replacement is logged.
#'
#' @param project A project from [open_project()].
#' @param paths Paths to runfiles.
#' @param sample_ids Sample IDs; default to the file names without extension.
#' @param source_names Original file names to record (useful when `paths`
#'   are temporary upload paths).
#' @param mode `"add"` or `"replace"`, see above.
#' @return The updated project. A message summarises what was added,
#'   replaced and skipped.
#' @export
add_runfiles <- function(project, paths, sample_ids = NULL, source_names = basename(paths),
                         mode = c("add", "replace")) {
  check_writable(project)
  mode <- match.arg(mode)
  if (is.null(sample_ids)) sample_ids <- tools::file_path_sans_ext(source_names)
  stopifnot(length(sample_ids) == length(paths), length(source_names) == length(paths))

  bad <- grepl("[\\\\/:*?\"<>|]", sample_ids) | !nzchar(trimws(sample_ids))
  if (any(bad)) {
    stop("Sample IDs cannot be empty or contain \\ / : * ? \" < > |: ",
         paste(sample_ids[bad], collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(sample_ids)) {
    stop("The batch contains more than one file named: ",
         paste(unique(sample_ids[duplicated(sample_ids)]), collapse = ", "), call. = FALSE)
  }

  md5 <- unname(tools::md5sum(paths))
  identical_file <- md5 %in% project$samples$md5 | duplicated(md5)
  existing <- match(sample_ids, project$samples$sample_id)
  name_taken <- !is.na(existing) & !identical_file
  is_new <- is.na(existing) & !identical_file
  replace <- name_taken & mode == "replace"
  todo <- is_new | replace

  notes <- c(
    if (any(identical_file)) paste0("Skipped (identical file already in the project): ",
                                    paste(source_names[identical_file], collapse = ", ")),
    if (any(name_taken & !replace)) paste0(
      "Skipped (a run with this name is already in the project; choose Replace to update it): ",
      paste(source_names[name_taken & !replace], collapse = ", "))
  )
  if (!any(todo)) {
    if (length(notes)) message(paste(notes, collapse = "\n"))
    return(project)
  }

  # Fails with a clear message if the files don't match the import settings.
  read_runfiles(paths[todo], project$settings$import, sample_ids[todo])
  acquired <- format_time(read_acquisition_times(paths, project$settings$import))
  runfiles <- file.path("raw", paste0(sample_ids, tolower(file_ext_dot(source_names))))

  # Replace: move the old file aside, then copy in the new one.
  if (any(replace)) {
    removed_dir <- project_file(project$path, "raw", "removed")
    dir.create(removed_dir, recursive = TRUE, showWarnings = FALSE)
    stamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    for (i in which(replace)) {
      j <- existing[i]
      old <- project$samples[j, ]
      moved <- file.path("raw", "removed",
                         unique_file(removed_dir, paste0(stamp, "_", basename(old$runfile))))
      if (file.exists(project_file(project$path, old$runfile)) &&
          !file.rename(project_file(project$path, old$runfile), project_file(project$path, moved))) {
        stop("Could not move '", old$runfile, "'. Is it open in another program?", call. = FALSE)
      }
      if (!file.copy(paths[i], project_file(project$path, runfiles[i]), overwrite = FALSE)) {
        stop("Could not copy ", source_names[i], ".", call. = FALSE)
      }
      project$samples$runfile[j] <- runfiles[i]
      project$samples$source[j] <- source_names[i]
      project$samples$md5[j] <- md5[i]
      project$samples$acquired[j] <- acquired[i]
      append_log(project_file(project$path, "log.csv"),
                 log_entry(project$user, "replace_runfile", old$sample_id, "runfile",
                           paste0(old$source, " (", moved, ")"), source_names[i]))
    }
  }

  if (any(is_new)) {
    ok <- file.copy(paths[is_new], project_file(project$path, runfiles[is_new]), overwrite = FALSE)
    if (!all(ok)) {
      stop("Could not copy: ", paste(source_names[is_new][!ok], collapse = ", "), call. = FALSE)
    }
    new_rows <- tibble::tibble(
      sample_id = sample_ids[is_new], runfile = runfiles[is_new], source = source_names[is_new],
      md5 = md5[is_new], added = timestamp_now(), added_by = project$user, !!!sample_defaults()
    )
    new_rows$acquired <- acquired[is_new]
    project$samples <- dplyr::bind_rows(project$samples, new_rows)
    append_log(project_file(project$path, "log.csv"),
               log_entry(project$user, "add_sample", sample_ids[is_new], new = source_names[is_new]))
  }
  write_table_file(project$samples, project_file(project$path, "samples.csv"))

  summary <- paste0(
    c(if (any(is_new)) paste("Added", sum(is_new), "run(s)."),
      if (any(replace)) paste("Replaced", sum(replace), "run(s):", paste(sample_ids[replace], collapse = ", "))),
    collapse = " ")
  message(paste(c(summary, notes), collapse = "\n"))
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
  primary <- new$calibration$primary
  if (nzchar(primary) && !primary %in% project$reference_materials$name) {
    stop("The primary reference material '", primary, "' is not defined in this project.",
         call. = FALSE)
  }
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
  if (any(grepl("^import\\.timestamp_", changed))) {
    project <- fill_acquisition_times(project, force = TRUE)
  }
  project
}

#' Remove samples from a project
#'
#' Removes samples, their regions and cached results from the project. The
#' raw runfile is not deleted but moved to `raw/removed/`, so a removed
#' sample can be recovered and the same file can be added again later. To
#' keep a sample but leave it out of exports, set its status to
#' `"excluded"` with [update_sample()] instead.
#'
#' @param project A project from [open_project()].
#' @param sample_ids Samples to remove.
#' @return The updated project.
#' @export
remove_samples <- function(project, sample_ids) {
  check_writable(project)
  purrr::walk(sample_ids, \(id) sample_index(project, id))

  removed_dir <- project_file(project$path, "raw", "removed")
  dir.create(removed_dir, recursive = TRUE, showWarnings = FALSE)
  stamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  rows <- dplyr::filter(project$samples, .data$sample_id %in% sample_ids)

  moved_to <- file.path("raw", "removed", paste0(stamp, "_", basename(rows$runfile)))
  for (i in seq_len(nrow(rows))) {
    moved_to[i] <- file.path("raw", "removed", unique_file(removed_dir, basename(moved_to[i])))
    from <- project_file(project$path, rows$runfile[i])
    if (file.exists(from) && !file.rename(from, project_file(project$path, moved_to[i]))) {
      stop("Could not move '", rows$runfile[i], "'. Is it open in another program?", call. = FALSE)
    }
    cache <- list.files(project_file(project$path, "cache"),
                        pattern = paste0("^", rlang::hash(rows$sample_id[i]), "_"), full.names = TRUE)
    unlink(cache)
  }

  project$samples <- dplyr::filter(project$samples, !.data$sample_id %in% sample_ids)
  project$regions <- dplyr::filter(project$regions, !.data$sample_id %in% sample_ids)
  write_table_file(project$samples, project_file(project$path, "samples.csv"))
  write_table_file(project$regions, project_file(project$path, "regions.csv"))
  append_log(project_file(project$path, "log.csv"),
             log_entry(project$user, "remove_sample", rows$sample_id, "runfile",
                       old = rows$runfile, new = moved_to))
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
