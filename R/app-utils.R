# Helpers for the Shiny app.

# The app runs locally, so a native folder dialog can be shown on the
# user's computer. Returns NULL if the dialog was cancelled or unavailable.
choose_folder <- function(caption = "Select folder", default = "") {
  path <- NULL
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    path <- rstudioapi::selectDirectory(caption = caption, path = default)
  } else if (.Platform$OS.type == "windows") {
    path <- utils::choose.dir(default = default, caption = caption)
  } else if (capabilities("tcltk") && requireNamespace("tcltk", quietly = TRUE)) {
    path <- tcltk::tk_choose.dir(default = default, caption = caption)
  }
  if (length(path) != 1 || is.na(path) || !nzchar(path)) NULL else path
}

# Recently opened projects, stored in the user's config folder.
recent_projects_file <- function() {
  file.path(tools::R_user_dir("IsoFishR", "config"), "recent_projects.txt")
}

recent_projects <- function() {
  f <- recent_projects_file()
  if (!file.exists(f)) return(character())
  paths <- readLines(f, warn = FALSE)
  paths[file.exists(file.path(paths, "project.yml"))]
}

remember_project <- function(path) {
  f <- recent_projects_file()
  try({
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    writeLines(utils::head(unique(c(path, recent_projects())), 10), f)
  }, silent = TRUE)
}

# Run a project change; show errors as notifications instead of crashing the
# app. Returns the result, or NULL on error.
try_notify <- function(expr, success = NULL) {
  tryCatch({
    out <- withCallingHandlers(expr, message = function(m) {
      shiny::showNotification(conditionMessage(m), type = "message")
      invokeRestart("muffleMessage")
    })
    if (!is.null(success)) shiny::showNotification(success, type = "message", duration = 3)
    out
  }, error = function(e) {
    shiny::showNotification(conditionMessage(e), type = "error", duration = 10)
    NULL
  })
}

sr_label <- function(a = 87, b = 86) {
  sprintf("<sup>%s</sup>Sr/<sup>%s</sup>Sr", a, b)
}

STATUS_LABELS <- c(new = "New", reviewed = "Reviewed", excluded = "Excluded")
