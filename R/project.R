#' IsoFishR projects
#'
#' A project is a folder chosen by the user. It holds the original runfiles
#' and every choice made about them; reduced data and exports are derived
#' from these and can always be regenerated.
#'
#' ```
#' MyProject/
#'   project.yml   project settings (see default_settings())
#'   raw/          copies of the original runfiles, never modified
#'   samples.csv   one row per sample: status, flag, comment, edits, setting overrides
#'   regions.csv   regions per sample, in seconds of analysis time
#'   log.csv       every change: time, user, action, sample, field, old and new value
#'   cache/        reduced data per sample (safe to delete)
#'   exports/      generated tables and plots
#'   .lock         who has the project open for editing
#' ```
#'
#' `create_project()` makes a new project folder and opens it.
#' `open_project()` opens an existing one and takes the lock, so only one
#' person edits a project at a time (useful for shared Dropbox folders).
#' `close_project()` releases the lock.
#'
#' Functions that change a project write to disk immediately, add entries to
#' `log.csv` and return the updated project object.
#'
#' @param path Project folder.
#' @param settings Project settings; defaults to [default_settings()].
#' @param user Name recorded in the log and the lock file.
#' @param read_only Open without taking the lock; changes are refused.
#' @param force Take the lock even if someone else holds it (e.g. after their
#'   session crashed).
#' @param project A project from `open_project()`.
#' @return An `isofishr_project` object (`close_project()` returns `NULL`).
#' @name project
NULL

#' @rdname project
#' @export
create_project <- function(path, settings = default_settings(), user = default_user()) {
  if (file.exists(project_file(path, "project.yml"))) {
    stop("'", path, "' already contains an IsoFishR project. Use open_project().", call. = FALSE)
  }
  if (dir.exists(path) && length(list.files(path, all.files = TRUE, no.. = TRUE)) > 0) {
    stop("'", path, "' is not empty. Choose a new or empty folder for the project.", call. = FALSE)
  }
  check_settings(settings)
  for (d in c("raw", "cache", "exports")) {
    dir.create(project_file(path, d), recursive = TRUE, showWarnings = FALSE)
  }

  meta <- list(
    name = basename(normalizePath(path)),
    format = PROJECT_FORMAT,
    created = timestamp_now(),
    created_by = user,
    isofishr_version = isofishr_version()
  )
  write_yaml_file(c(list(project = meta), settings), project_file(path, "project.yml"))
  write_table_file(samples_template(), project_file(path, "samples.csv"))
  write_table_file(regions_template(), project_file(path, "regions.csv"))
  append_log(project_file(path, "log.csv"),
             log_entry(user, "create_project", new = meta$name))

  open_project(path, user = user)
}

#' @rdname project
#' @export
open_project <- function(path, user = default_user(), read_only = FALSE, force = FALSE) {
  yml_path <- project_file(path, "project.yml")
  if (!file.exists(yml_path)) {
    stop("'", path, "' is not an IsoFishR project (no project.yml).", call. = FALSE)
  }
  yml <- yaml::read_yaml(yml_path)
  if (isTRUE(yml$project$format > PROJECT_FORMAT)) {
    stop("This project was saved by a newer version of IsoFishR (",
         yml$project$isofishr_version, "). Please update IsoFishR.", call. = FALSE)
  }
  path <- normalizePath(path, winslash = "/")
  if (!read_only) acquire_lock(path, user, force)

  structure(
    list(
      path = path,
      meta = yml$project,
      settings = merge_settings(default_settings(), yml[setdiff(names(yml), "project")]),
      samples = read_table_file(project_file(path, "samples.csv"), samples_template(),
                                sample_defaults()),
      regions = read_table_file(project_file(path, "regions.csv"), regions_template()),
      user = user,
      read_only = read_only
    ),
    class = "isofishr_project"
  )
}

#' @rdname project
#' @export
close_project <- function(project) {
  if (!project$read_only && owns_lock(project)) {
    unlink(project_file(project$path, ".lock"))
  }
  invisible(NULL)
}

#' @export
print.isofishr_project <- function(x, ...) {
  status <- table(factor(x$samples$status, levels = SAMPLE_STATUS))
  cat("<IsoFishR project> ", x$meta$name, if (x$read_only) " (read-only)", "\n",
      "  Folder:  ", x$path, "\n",
      "  Samples: ", nrow(x$samples), " (",
      paste(names(status), status, sep = ": ", collapse = ", "), ")\n",
      "  Flagged: ", sum(x$samples$flag), "\n",
      "  Regions: ", nrow(x$regions), "\n", sep = "")
  invisible(x)
}

#' Read the project change log
#'
#' @param project A project from [open_project()].
#' @return A tibble with one row per recorded change.
#' @export
read_log <- function(project) {
  read_table_file(project_file(project$path, "log.csv"), log_template())
}

# ---- locking -----------------------------------------------------------------

lock_info <- function(user) {
  list(user = user, host = Sys.info()[["nodename"]], pid = Sys.getpid(), time = timestamp_now())
}

acquire_lock <- function(path, user, force = FALSE) {
  lock_path <- project_file(path, ".lock")
  me <- lock_info(user)
  if (file.exists(lock_path) && !force) {
    other <- yaml::read_yaml(lock_path)
    # The same user on the same computer may take over (e.g. after a crash).
    if (!identical(other$user, me$user) || !identical(other$host, me$host)) {
      stop("This project is open for editing by ", other$user, " on ", other$host,
           " (since ", other$time, "). Open it read-only, or use force = TRUE if ",
           "that session has ended.", call. = FALSE)
    }
  }
  write_yaml_file(me, lock_path)
}

owns_lock <- function(project) {
  lock_path <- project_file(project$path, ".lock")
  if (!file.exists(lock_path)) return(FALSE)
  lock <- yaml::read_yaml(lock_path)
  identical(lock$user, project$user) &&
    identical(lock$host, Sys.info()[["nodename"]]) &&
    identical(as.integer(lock$pid), Sys.getpid())
}

check_writable <- function(project) {
  if (project$read_only) {
    stop("The project is open read-only.", call. = FALSE)
  }
  if (!owns_lock(project)) {
    stop("This session no longer holds the project lock (someone else may have opened ",
         "it). Reopen the project to continue editing.", call. = FALSE)
  }
  invisible(project)
}

# ---- helpers -------------------------------------------------------------------

default_user <- function() {
  user <- Sys.getenv("USERNAME", Sys.getenv("USER", ""))
  if (nzchar(user)) user else "unknown"
}

isofishr_version <- function() as.character(utils::packageVersion("IsoFishR"))

timestamp_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

log_entry <- function(user, action, sample_id = "", field = "", old = "", new = "") {
  tibble::tibble(time = timestamp_now(), user = user, action = action,
                 sample_id = sample_id, field = field,
                 old = as.character(old), new = as.character(new))
}
