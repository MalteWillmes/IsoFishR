#' Add runfiles to a project
#'
#' Copies runfiles into the project's `raw/` folder and registers them as new
#' runs, so a project can be built up batch by batch. Each file is read first
#' with the project's import settings. A file that can't be read is not added
#' and is reported, without stopping the rest of the batch. If a file's
#' header doesn't match the import settings (e.g. lines missing after a bad
#' save on the instrument), the start of its data is found automatically and
#' the file is added with a note.
#'
#' Files identical to one already in the project are always skipped. For a
#' file whose name matches a run already in the project but whose content
#' differs (e.g. a re-exported runfile):
#'
#' * `mode = "add"` skips it and reports it;
#' * `mode = "replace"` gives that run the new file and starts it afresh:
#'   its edits (trims, status, flag, comment, changepoint and per-sample
#'   settings) and regions are removed. Its run type, reference material and
#'   session are kept. The previous file is moved to `raw/removed/` and the
#'   replacement is logged.
#'
#' @param project A project from [open_project()].
#' @param paths Paths to runfiles.
#' @param sample_ids Sample IDs; default to the file names without extension.
#' @param source_names Original file names to record (useful when `paths`
#'   are temporary upload paths).
#' @param mode `"add"` or `"replace"`, see above.
#' @return The updated project. A message summarises what happened; the
#'   attribute `"import_report"` holds one row per file with `file`,
#'   `sample_id`, `result` (`"added"`, `"replaced"`, `"skipped"` or
#'   `"failed"`) and `detail`.
#' @export
add_runfiles <- function(project, paths, sample_ids = NULL, source_names = basename(paths),
                         mode = c("add", "replace")) {
  check_writable(project)
  mode <- match.arg(mode)
  if (is.null(sample_ids)) sample_ids <- tools::file_path_sans_ext(source_names)
  stopifnot(length(sample_ids) == length(paths), length(source_names) == length(paths))
  import <- project$settings$import
  n <- length(paths)
  result <- rep(NA_character_, n)
  detail <- rep("", n)

  # Files that can't be added at all.
  bad_name <- grepl("[\\\\/:*?\"<>|]", sample_ids) | !nzchar(trimws(sample_ids))
  result[bad_name] <- "failed"
  detail[bad_name] <- "the name contains characters that are not allowed (\\ / : * ? \" < > |)"
  dup_name <- duplicated(sample_ids) & is.na(result)
  result[dup_name] <- "skipped"
  detail[dup_name] <- "another file in this batch has the same name"

  md5 <- unname(tools::md5sum(paths))
  identical_file <- (md5 %in% project$samples$md5 | duplicated(md5)) & is.na(result)
  result[identical_file] <- "skipped"
  detail[identical_file] <- "identical file already in the project"
  existing <- match(sample_ids, project$samples$sample_id)
  name_taken <- !is.na(existing) & is.na(result)
  if (mode == "add") {
    result[name_taken] <- "skipped"
    detail[name_taken] <- "a run with this name is already in the project (choose Replace to update it)"
  }

  # Read each remaining file on its own, so one bad file doesn't stop the rest.
  for (i in which(is.na(result))) {
    checked <- tryCatch(read_runfile(paths[i], import, sample_ids[i]),
                        error = function(e) e)
    if (inherits(checked, "error")) {
      result[i] <- "failed"
      detail[i] <- conditionMessage(checked)
    } else if (!is.null(attr(checked, "data_start"))) {
      detail[i] <- sprintf(paste0("the header doesn't match the import settings; data found ",
                                  "from line %d (expected line %d)"),
                           attr(checked, "data_start"), import$skip_header + 1L)
    }
  }
  replace <- is.na(result) & name_taken
  is_new <- is.na(result) & is.na(existing)

  acquired <- rep("", n)
  if (any(replace | is_new)) {
    acquired[replace | is_new] <- format_time(read_acquisition_times(paths[replace | is_new], import))
    no_time <- (replace | is_new) & !nzchar(acquired) & (import$timestamp_line %||% 0) > 0
    detail[no_time] <- trimws(paste(detail[no_time], if (any(no_time)) {
      "(no readable acquisition time: it can't be placed in the run sequence)"
    }))
  }
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
        result[i] <- "failed"
        detail[i] <- paste0("could not move the old file '", old$runfile, "' (is it open in another program?)")
        next
      }
      if (!file.copy(paths[i], project_file(project$path, runfiles[i]), overwrite = FALSE)) {
        file.rename(project_file(project$path, moved), project_file(project$path, old$runfile))
        result[i] <- "failed"
        detail[i] <- "could not copy the file into the project"
        next
      }
      project$samples$runfile[j] <- runfiles[i]
      project$samples$source[j] <- source_names[i]
      project$samples$md5[j] <- md5[i]
      project$samples$acquired[j] <- acquired[i]
      # A new file starts afresh: edits and regions made for the old file
      # don't apply to it.
      for (field in c(EDITABLE_SAMPLE_FIELDS, "settings")) {
        project$samples[[field]][j] <- sample_defaults()[[field]]
      }
      project$regions <- dplyr::filter(project$regions, .data$sample_id != old$sample_id)
      result[i] <- "replaced"
      append_log(project_file(project$path, "log.csv"),
                 log_entry(project$user, "replace_runfile", old$sample_id, "runfile",
                           paste0(old$source, " (", moved, ")"),
                           paste0(source_names[i], " (edits and regions cleared)")))
    }
  }

  if (any(is_new)) {
    copied <- file.copy(paths[is_new], project_file(project$path, runfiles[is_new]), overwrite = FALSE)
    new_idx <- which(is_new)
    result[new_idx[!copied]] <- "failed"
    detail[new_idx[!copied]] <- "could not copy the file into the project"
    add <- new_idx[copied]
    result[add] <- "added"
    if (length(add) > 0) {
      new_rows <- tibble::tibble(
        sample_id = sample_ids[add], runfile = runfiles[add], source = source_names[add],
        md5 = md5[add], added = timestamp_now(), added_by = project$user, !!!sample_defaults()
      )
      new_rows$acquired <- acquired[add]
      project$samples <- dplyr::bind_rows(project$samples, new_rows)
      append_log(project_file(project$path, "log.csv"),
                 log_entry(project$user, "add_sample", sample_ids[add], new = source_names[add]))
    }
  }
  if (any(result %in% c("added", "replaced"))) {
    write_table_file(project$samples, project_file(project$path, "samples.csv"))
  }
  if (any(result == "replaced")) {
    write_table_file(project$regions, project_file(project$path, "regions.csv"))
  }

  report <- tibble::tibble(file = source_names, sample_id = sample_ids, result = result,
                           detail = detail)
  message(import_summary(report))
  attr(project, "import_report") <- report
  project
}

# One-paragraph summary of an import report.
import_summary <- function(report) {
  count <- function(r) sum(report$result == r)
  list_files <- function(rows) {
    paste0(report$file[rows], ifelse(nzchar(report$detail[rows]),
                                     paste0(" (", report$detail[rows], ")"), ""),
           collapse = "; ")
  }
  noted <- report$result %in% c("added", "replaced") & nzchar(report$detail)
  c(
    paste0(c(if (count("added")) paste("Added", count("added"), "run(s)."),
             if (count("replaced")) paste("Replaced", count("replaced"), "run(s):",
                                          paste(report$sample_id[report$result == "replaced"],
                                                collapse = ", "))),
           collapse = " "),
    if (count("failed")) paste0("Could not read ", count("failed"), " file(s): ",
                                list_files(report$result == "failed")),
    if (count("skipped")) paste0("Skipped ", count("skipped"), " file(s): ",
                                 list_files(report$result == "skipped")),
    if (any(noted)) paste0("Added with a note: ", list_files(noted))
  ) |>
    purrr::discard(\(x) !nzchar(x)) |>
    paste(collapse = "\n")
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
#' `set_sample_settings()` overrides project reduction settings, or the run
#' speed, for one sample (e.g. a different blank time). Pass `NULL` to go
#' back to the project value. `sample_settings()` returns the settings a
#' sample is reduced with.
#'
#' @param project A project from [open_project()].
#' @param sample_id One sample ID.
#' @param ... Named reduction settings, e.g. `blank_time_s = 25`, or
#'   `run_speed_um_s`.
#' @return `set_sample_settings()`: the updated project.
#'   `sample_settings()`: a settings list.
#' @export
set_sample_settings <- function(project, sample_id, ...) {
  check_writable(project)
  i <- sample_index(project, sample_id)
  changes <- list(...)
  defaults <- override_defaults(project$settings)
  unknown <- setdiff(names(changes), names(defaults))
  if (length(unknown) > 0) {
    stop("Unknown sample setting(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }

  old <- parse_overrides(project$samples$settings[i])
  new <- old
  for (key in names(changes)) {
    new[[key]] <- if (is.null(changes[[key]])) NULL else coerce_like(changes[[key]], defaults[[key]])
  }
  # Validate the combination before saving anything.
  merge_settings(project$settings, overrides_to_settings(new))

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
  merge_settings(project$settings, overrides_to_settings(parse_overrides(project$samples$settings[i])))
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
