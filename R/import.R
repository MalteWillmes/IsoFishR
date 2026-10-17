#' Read laser-ablation runfiles
#'
#' Reads the cycle data from one or more instrument runfiles, using the
#' column layout in the `import` section of the settings.
#'
#' @param paths Paths to runfiles.
#' @param import The `import` section of a settings list.
#' @param sample_ids Sample IDs, one per file. Defaults to the file names
#'   without extension. Pass these explicitly when `paths` are temporary
#'   upload paths.
#' @return A tibble with one row per measurement cycle and columns
#'   `sample_id`, `cycle`, `time_s` and `raw88` to `raw83` (volts).
#' @export
read_runfiles <- function(paths, import = default_settings()$import,
                          sample_ids = tools::file_path_sans_ext(basename(paths))) {
  if (length(paths) != length(sample_ids)) {
    stop("`paths` and `sample_ids` must have the same length.", call. = FALSE)
  }
  if (anyDuplicated(sample_ids)) {
    stop("Sample IDs must be unique; duplicated: ",
         paste(unique(sample_ids[duplicated(sample_ids)]), collapse = ", "), call. = FALSE)
  }
  purrr::map2(paths, sample_ids, \(path, id) read_runfile(path, import, id)) |>
    purrr::list_rbind()
}

#' Read the acquisition time of runfiles
#'
#' Reads the date and time a run was started from its header, using the
#' `timestamp_line` and `timestamp_format` of the import settings (for Nu
#' Plasma files: line 45, e.g. `"08/12/2017","04:31:02 PM"`). Quotes are
#' removed and commas or tabs between the date and time become spaces before
#' the line is parsed with [base::strptime()] `format`. Times are treated as
#' the instrument clock and returned in UTC, so daylight saving changes never
#' reorder runs.
#'
#' @param paths Paths to runfiles.
#' @param import The `import` section of a settings list.
#' @return A POSIXct vector (`NA` where there is no timestamp line or it
#'   could not be parsed).
#' @export
read_acquisition_times <- function(paths, import = default_settings()$import) {
  line_no <- as.integer(import$timestamp_line %||% 0L)
  if (is.na(line_no) || line_no < 1) return(as.POSIXct(rep(NA_real_, length(paths)), tz = "UTC"))
  text <- vapply(paths, \(p) {
    x <- readLines(p, n = line_no, warn = FALSE)
    if (length(x) < line_no) NA_character_ else x[[line_no]]
  }, character(1), USE.NAMES = FALSE)
  text <- trimws(gsub("[,\t]+", " ", gsub("\"", "", text)))
  # Parse in the C locale so "AM"/"PM" and month names work on any system.
  old <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  as.POSIXct(strptime(text, import$timestamp_format, tz = "UTC"))
}

read_runfile <- function(path, import, sample_id) {
  cols <- unlist(import$columns)
  # Lines of a different shape (e.g. header lines when the header length is
  # wrong) make readr warn; rows that don't parse as numbers are dropped
  # below, and callers check what was read.
  raw <- suppressWarnings(readr::read_delim(
    path,
    delim = import$sep,
    skip = import$skip_header,
    col_names = isTRUE(import$header),
    col_types = readr::cols(.default = readr::col_character()),
    name_repair = "minimal",
    trim_ws = TRUE,
    progress = FALSE
  ))
  if (import$skip_footer > 0) {
    raw <- utils::head(raw, -import$skip_footer)
  }
  if (max(cols) > ncol(raw)) {
    stop("Runfile '", basename(path), "' has ", ncol(raw), " columns, but the import ",
         "settings refer to column ", max(cols), ". Check the column numbers and ",
         "header length.", call. = FALSE)
  }

  raw <- raw[, cols]
  names(raw) <- names(cols)
  raw |>
    dplyr::mutate(dplyr::across(dplyr::everything(), \(x) suppressWarnings(as.numeric(x)))) |>
    dplyr::filter(!is.na(raw88), !is.na(time_s)) |>
    dplyr::mutate(sample_id = sample_id, cycle = dplyr::row_number(), .before = 1) |>
    dplyr::relocate(time_s, .after = cycle)
}
