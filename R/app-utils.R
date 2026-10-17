# Helpers for the Shiny app.

# The app runs locally, so a native folder dialog can be shown on the
# user's computer. Returns NULL if the dialog was cancelled or unavailable.
#
# Dialogs opened by R (or RStudio) belong to that window, which is usually
# behind the browser, so they appear hidden. The system dialogs below are
# opened in front instead: on Windows with an always-on-top owner window,
# on macOS in the frontmost application (the browser). R's own dialogs are
# the fallback.
choose_folder <- function(caption = "Select folder", default = "") {
  path <- tryCatch(
    switch(Sys.info()[["sysname"]],
           Windows = choose_folder_windows(caption, default),
           Darwin = choose_folder_mac(caption, default),
           choose_folder_linux(caption, default)),
    error = function(e) NA_character_
  )
  # NA means the system dialog could not be shown (not that it was cancelled).
  if (length(path) == 1 && is.na(path)) path <- choose_folder_r(caption, default)
  if (length(path) != 1 || is.na(path) || !nzchar(path)) NULL else path
}

choose_folder_r <- function(caption, default) {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    rstudioapi::selectDirectory(caption = caption, path = default)
  } else if (.Platform$OS.type == "windows") {
    utils::choose.dir(default = default, caption = caption)
  } else if (capabilities("tcltk") && requireNamespace("tcltk", quietly = TRUE)) {
    tcltk::tk_choose.dir(default = default, caption = caption)
  }
}

# PowerShell script showing the Windows folder dialog in front of all windows
# and printing the chosen folder (nothing if cancelled).
folder_dialog_ps1 <- function(caption, default) {
  q <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
  c(
    "Add-Type -AssemblyName System.Windows.Forms",
    "[Console]::OutputEncoding = [System.Text.Encoding]::UTF8",
    "$owner = New-Object System.Windows.Forms.Form",
    "$owner.TopMost = $true",
    "$owner.ShowInTaskbar = $false",
    "$owner.StartPosition = 'CenterScreen'",
    "$owner.Size = New-Object System.Drawing.Size(1, 1)",
    "$owner.Show()",
    "$owner.Activate()",
    "$dialog = New-Object System.Windows.Forms.FolderBrowserDialog",
    paste0("$dialog.Description = ", q(caption)),
    "$dialog.ShowNewFolderButton = $true",
    if (nzchar(default) && dir.exists(default)) {
      paste0("$dialog.SelectedPath = ", q(normalizePath(default, winslash = "\\")))
    },
    "$result = $dialog.ShowDialog($owner)",
    "$owner.Close()",
    "if ($result -eq [System.Windows.Forms.DialogResult]::OK) { Write-Output $dialog.SelectedPath }"
  )
}

choose_folder_windows <- function(caption, default) {
  ps <- Sys.which("powershell")
  if (!nzchar(ps)) return(NA_character_)
  script <- tempfile(fileext = ".ps1")
  on.exit(unlink(script))
  # A byte order mark makes Windows PowerShell read the script as UTF-8
  # (needed for folder names with characters such as \u00f8).
  con <- file(script, open = "wb")
  writeBin(as.raw(c(0xef, 0xbb, 0xbf)), con)
  writeBin(charToRaw(enc2utf8(paste(folder_dialog_ps1(caption, default), collapse = "\r\n"))), con)
  close(con)
  out <- suppressWarnings(system2(ps, c("-NoProfile", "-NonInteractive", "-STA",
                                        "-ExecutionPolicy", "Bypass", "-File", shQuote(script)),
                                  stdout = TRUE, stderr = FALSE))
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) return(NA_character_)
  out <- trimws(out[nzchar(trimws(out))])
  if (length(out) == 0) return("")  # cancelled
  Encoding(out) <- "UTF-8"
  out[length(out)]
}

choose_folder_mac <- function(caption, default) {
  q <- function(x) gsub("\"", "\\\\\"", x)
  location <- if (nzchar(default) && dir.exists(default)) {
    paste0(" default location (POSIX file \"", q(normalizePath(default)), "\")")
  } else ""
  script <- paste0("tell application (path to frontmost application as text) to ",
                   "POSIX path of (choose folder with prompt \"", q(caption), "\"", location, ")")
  out <- suppressWarnings(system2("osascript", c("-e", shQuote(script)), stdout = TRUE, stderr = FALSE))
  if (!is.null(attr(out, "status"))) return("")  # cancelled
  sub("/$", "", out[1])
}

choose_folder_linux <- function(caption, default) {
  if (!nzchar(Sys.which("zenity"))) return(NA_character_)
  args <- c("--file-selection", "--directory", paste0("--title=", shQuote(caption)),
            if (nzchar(default)) paste0("--filename=", shQuote(file.path(default, ""))))
  out <- suppressWarnings(system2("zenity", args, stdout = TRUE, stderr = FALSE))
  if (!is.null(attr(out, "status"))) return("")  # cancelled
  out[1]
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
