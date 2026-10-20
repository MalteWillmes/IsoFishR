#' Runs in acquisition order
#'
#' Lists the runs of a project in the order they were measured, with their
#' type and analytical session. Sessions are found automatically: a new
#' session starts when the gap between two runs is longer than
#' `calibration$session_gap_h` hours (default 6). A session set with
#' [set_session()] replaces the automatic one for that run.
#'
#' @param project A project from [open_project()].
#' @return A tibble with `sequence`, `sample_id`, `acquired` (POSIXct, UTC),
#'   `run_type`, `reference_material`, `rm_role` (`"primary"`, `"secondary"`
#'   or `""`), `status`, `session_auto` and `session`.
#' @export
project_runs <- function(project) {
  s <- project$samples
  gap_h <- project$settings$calibration$session_gap_h
  primary <- project$settings$calibration$primary
  runs <- tibble::tibble(
    sample_id = s$sample_id,
    acquired = parse_time(s$acquired),
    run_type = s$run_type,
    reference_material = s$reference_material,
    status = s$status,
    session_override = s$session,
    .added = seq_len(nrow(s))
  ) |>
    dplyr::arrange(is.na(.data$acquired), .data$acquired, .data$.added) |>
    dplyr::mutate(sequence = dplyr::row_number(), .before = 1)

  t <- runs$acquired
  gap <- c(TRUE, as.numeric(diff(t), units = "hours") > gap_h)[seq_along(t)]
  gap[is.na(gap)] <- FALSE
  idx <- cumsum(gap)
  first <- t[match(idx, idx)]
  # as.character(): for a project without runs ifelse() returns logical(0)
  runs$session_auto <- as.character(ifelse(
    is.na(t), "No timestamp", sprintf("S%d %s", idx, format(first, "%Y-%m-%d", tz = "UTC"))
  ))
  runs |>
    dplyr::mutate(
      session = dplyr::if_else(nzchar(.data$session_override), .data$session_override,
                               .data$session_auto),
      rm_role = dplyr::case_when(
        .data$run_type != "reference" ~ "",
        nzchar(primary) & .data$reference_material == primary ~ "primary",
        .default = "secondary"
      )
    ) |>
    dplyr::select("sequence", "sample_id", "acquired", "run_type", "reference_material",
                  "rm_role", "status", "session_auto", "session")
}

#' Set the type of runs
#'
#' Marks runs as samples, baseline (gas blank) runs, or measurements of a
#' reference material. Reference materials must first be defined with
#' [set_reference_material()].
#'
#' @param project A project from [open_project()].
#' @param sample_ids Runs to change.
#' @param run_type `"sample"`, `"baseline"` or `"reference"`.
#' @param reference_material Name of the reference material (only for
#'   `run_type = "reference"`).
#' @return The updated project.
#' @export
set_run_type <- function(project, sample_ids, run_type, reference_material = "") {
  check_writable(project)
  purrr::walk(sample_ids, \(id) sample_index(project, id))
  run_type <- match.arg(run_type, RUN_TYPES)
  if (run_type == "reference") {
    if (!reference_material %in% project$reference_materials$name) {
      stop("Unknown reference material '", reference_material, "'. Define it first with ",
           "set_reference_material().", call. = FALSE)
    }
  } else {
    reference_material <- ""
  }
  i <- match(sample_ids, project$samples$sample_id)
  describe <- function(type, rm) ifelse(type == "reference", paste0("reference: ", rm), type)
  old <- describe(project$samples$run_type[i], project$samples$reference_material[i])
  new <- describe(run_type, reference_material)
  changed <- old != new
  if (!any(changed)) return(project)
  project$samples$run_type[i] <- run_type
  project$samples$reference_material[i] <- reference_material
  write_table_file(project$samples, project_file(project$path, "samples.csv"))
  append_log(project_file(project$path, "log.csv"),
             log_entry(project$user, "run_type", sample_ids[changed], "run_type",
                       old[changed], new))
  project
}

#' Assign runs to an analytical session
#'
#' Overrides the automatic session of runs, e.g. to split a long day into
#' two sessions after re-tuning. Corrections are calculated within sessions.
#'
#' @param project A project from [open_project()].
#' @param sample_ids Runs to change.
#' @param session Session name; `""` returns the runs to their automatic
#'   session.
#' @return The updated project.
#' @export
set_session <- function(project, sample_ids, session) {
  check_writable(project)
  purrr::walk(sample_ids, \(id) sample_index(project, id))
  i <- match(sample_ids, project$samples$sample_id)
  old <- project$samples$session[i]
  changed <- old != session
  if (!any(changed)) return(project)
  project$samples$session[i] <- session
  write_table_file(project$samples, project_file(project$path, "samples.csv"))
  append_log(project_file(project$path, "log.csv"),
             log_entry(project$user, "session", sample_ids[changed], "session",
                       ifelse(nzchar(old[changed]), old[changed], "(automatic)"),
                       if (nzchar(session)) session else "(automatic)"))
  project
}

#' Reference materials of a project
#'
#' `set_reference_material()` adds a reference material or updates its
#' accepted value. `remove_reference_material()` removes one that no run is
#' assigned to. The primary reference material, used to correct samples, is
#' chosen with `update_project_settings(p, list(calibration = list(primary =
#' name)))`; all others are secondary and are reported as accuracy checks.
#'
#' @param project A project from [open_project()].
#' @param name Reference material name.
#' @param value Accepted 87Sr/86Sr.
#' @param uncertainty_2s Uncertainty of the accepted value (2 sigma).
#' @param note Optional note, e.g. the source of the value.
#' @return The updated project.
#' @export
set_reference_material <- function(project, name, value, uncertainty_2s = NA_real_, note = "") {
  check_writable(project)
  name <- trimws(name)
  if (!nzchar(name)) stop("The reference material needs a name.", call. = FALSE)
  if (!is.numeric(value) || length(value) != 1 || is.na(value) || value <= 0) {
    stop("The accepted value must be a positive number.", call. = FALSE)
  }
  if (!is.na(uncertainty_2s) && uncertainty_2s < 0) {
    stop("The uncertainty must be 0 or more.", call. = FALSE)
  }
  rms <- project$reference_materials
  new <- tibble::tibble(name = name, value = value, uncertainty_2s = as.numeric(uncertainty_2s),
                        note = note)
  i <- match(name, rms$name)
  old_text <- if (is.na(i)) "" else format_rm(rms[i, ])
  if (is.na(i)) rms <- dplyr::bind_rows(rms, new) else rms[i, ] <- new
  if (identical(old_text, format_rm(new))) return(project)
  project$reference_materials <- rms
  write_table_file(rms, project_file(project$path, "reference_materials.csv"))
  append_log(project_file(project$path, "log.csv"),
             log_entry(project$user, "reference_material", "", name, old_text, format_rm(new)))
  project
}

#' @rdname set_reference_material
#' @export
remove_reference_material <- function(project, name) {
  check_writable(project)
  if (!name %in% project$reference_materials$name) {
    stop("Unknown reference material '", name, "'.", call. = FALSE)
  }
  n_runs <- sum(project$samples$run_type == "reference" & project$samples$reference_material == name)
  if (n_runs > 0) {
    stop(n_runs, " run(s) are assigned to ", name, ". Change their run type first.", call. = FALSE)
  }
  old <- format_rm(project$reference_materials[project$reference_materials$name == name, ])
  project$reference_materials <- dplyr::filter(project$reference_materials, .data$name != !!name)
  write_table_file(project$reference_materials,
                   project_file(project$path, "reference_materials.csv"))
  append_log(project_file(project$path, "log.csv"),
             log_entry(project$user, "reference_material", "", name, old, "(removed)"))
  if (identical(project$settings$calibration$primary, name)) {
    project <- update_project_settings(project, list(calibration = list(primary = "")))
  }
  project
}

format_rm <- function(rm) {
  paste0(format(rm$value, digits = 7),
         if (!is.na(rm$uncertainty_2s)) paste0(" \u00b1 ", format(rm$uncertainty_2s, digits = 3)),
         if (nzchar(rm$note)) paste0(" (", rm$note, ")"))
}

# Read acquisition times from the raw files for runs that don't have one yet
# (or for all runs with `force = TRUE`, e.g. after the timestamp settings
# changed). Saved when the project is writable.
fill_acquisition_times <- function(project, force = FALSE) {
  s <- project$samples
  if (nrow(s) == 0 || (project$settings$import$timestamp_line %||% 0) < 1) return(project)
  todo <- if (force) seq_len(nrow(s)) else which(!nzchar(s$acquired))
  if (length(todo) == 0) return(project)
  paths <- project_file(project$path, s$runfile[todo])
  times <- format_time(read_acquisition_times(paths, project$settings$import))
  changed <- times != s$acquired[todo]
  if (!any(changed)) return(project)
  project$samples$acquired[todo] <- times
  if (!project$read_only && owns_lock(project)) {
    write_table_file(project$samples, project_file(project$path, "samples.csv"))
    append_log(project_file(project$path, "log.csv"),
               log_entry(project$user, "acquisition_time", s$sample_id[todo][changed],
                         "acquired", s$acquired[todo][changed], times[changed]))
  }
  project
}

format_time <- function(t) ifelse(is.na(t), "", format(t, "%Y-%m-%d %H:%M:%S", tz = "UTC"))

parse_time <- function(x) {
  as.POSIXct(ifelse(nzchar(x), x, NA_character_), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

#' Suggest reference material runs from their names
#'
#' Finds runs whose name contains `pattern` (case-insensitive), e.g. "SBO"
#' for the seabass otolith reference material run at UC Davis (see
#' [default_reference_materials()]).
#' `apply_reference_suggestion()` marks them as runs of that reference
#' material, defines it with `value` if it isn't defined yet, and makes it the
#' primary reference material if none is set.
#'
#' @param project A project from [open_project()].
#' @param pattern Text to look for in run names.
#' @param name Reference material name.
#' @param value Accepted 87Sr/86Sr used when the reference material is not
#'   yet defined (defaults: [default_reference_materials()]).
#' @return `suggest_reference_runs()`: sample IDs of matching runs that are
#'   still samples. `apply_reference_suggestion()`: the updated project.
#' @export
suggest_reference_runs <- function(project, pattern = "SBO") {
  s <- project$samples
  s$sample_id[s$run_type == "sample" & grepl(pattern, s$sample_id, ignore.case = TRUE)]
}

#' @rdname suggest_reference_runs
#' @export
apply_reference_suggestion <- function(project, pattern = "SBO", name = pattern,
                                       value = default_rm_value(name)) {
  ids <- suggest_reference_runs(project, pattern)
  if (!name %in% project$reference_materials$name) {
    project <- set_reference_material(project, name, value, note = "suggested default")
  }
  if (length(ids) > 0) project <- set_run_type(project, ids, "reference", name)
  if (!nzchar(project$settings$calibration$primary)) {
    project <- update_project_settings(project, list(calibration = list(primary = name)))
  }
  project
}

#' Default reference materials
#'
#' Reference materials IsoFishR recognises in run names, with default
#' accepted 87Sr/86Sr values set by the IsoFishR authors. They are only used
#' to suggest marking runs (see [suggest_reference_runs()]); the value can be
#' changed when the suggestion is applied, and afterwards under Calibration.
#'
#' @return A tibble with `name` (also the text looked for in run names) and
#'   `value`.
#' @export
default_reference_materials <- function() {
  tibble::tribble(
    ~name,  ~value,
    "SBO",  0.70918,
    "GSFR", 0.70918
  )
}

# The default accepted value of a reference material (NA if there is none).
default_rm_value <- function(name) {
  d <- default_reference_materials()
  if (name %in% d$name) d$value[d$name == name] else NA_real_
}

# Runs that look like runs of each default reference material, as a named
# list (only reference materials with matching runs).
reference_suggestions <- function(project) {
  defaults <- default_reference_materials()
  ids <- purrr::map(defaults$name, \(nm) suggest_reference_runs(project, nm)) |>
    rlang::set_names(defaults$name)
  purrr::keep(ids, \(x) length(x) > 0)
}

#' Find runfiles in a folder
#'
#' Lists files with the runfile extension of the import settings in a folder
#' and all its subfolders.
#'
#' @param dir Folder to search.
#' @param import The `import` section of a settings list (for `extension`).
#' @return A tibble with `path`, `file`, `sample_id`, `folder` (relative to
#'   `dir`) and `duplicate` (`TRUE` when another file in the folder tree has
#'   the same name; such files can't be told apart and are not added).
#' @export
find_runfiles <- function(dir, import = default_settings()$import) {
  ext <- sub("^[.]", "", import$extension %||% "run")
  paths <- list.files(dir, pattern = paste0("[.]", ext, "$"), recursive = TRUE,
                      full.names = TRUE, ignore.case = TRUE)
  ids <- tools::file_path_sans_ext(basename(paths))
  tibble::tibble(
    path = paths,
    file = basename(paths),
    sample_id = ids,
    folder = dirname(substring(paths, nchar(dir) + 2)),
    duplicate = ids %in% ids[duplicated(ids)]
  ) |>
    dplyr::arrange(.data$folder, .data$file)
}

# The step of the app a project was last worked on, kept in the project
# folder (app_state.yml) so the project reopens there. Not a project change,
# so it is not logged.
read_last_step <- function(project) {
  f <- project_file(project$path, "app_state.yml")
  if (!file.exists(f)) return(NULL)
  tryCatch(yaml::read_yaml(f)$last_step, error = function(e) NULL)
}

write_last_step <- function(project, step) {
  if (project$read_only || !owns_lock(project)) return(invisible(FALSE))
  try(write_yaml_file(list(last_step = step), project_file(project$path, "app_state.yml")),
      silent = TRUE)
  invisible(TRUE)
}
