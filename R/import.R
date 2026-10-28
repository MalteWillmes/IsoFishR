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
    # A missing or unreadable file just has no time (it is reported when
    # the run is reduced); it must not stop the project from opening.
    x <- tryCatch(read_text_lines(p, n = line_no), error = function(e) character())
    if (length(x) < line_no) NA_character_ else x[[line_no]]
  }, character(1), USE.NAMES = FALSE)
  text <- trimws(gsub("[,\t]+", " ", gsub("\"", "", text)))
  # Parse in the C locale so "AM"/"PM" and month names work on any system.
  old <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  as.POSIXct(strptime(text, import$timestamp_format, tz = "UTC"))
}

# Reads one runfile. If the header doesn't match the import settings (e.g. a
# file saved with missing or extra header lines), the start of the data is
# found automatically; the result then has the attribute "data_start" (the
# line where the data was found).
read_runfile <- function(path, import, sample_id) {
  # Where the data should start, and where it does: header lines can go
  # missing or be added when the instrument saves a file.
  expected <- import$skip_header + 1L + isTRUE(import$header)
  is_data <- data_lines(path, import)
  ok <- isTRUE(is_data[expected]) && (expected == 1L || !is_data[expected - 1L])
  raw <- if (ok) read_cycles(path, import, skip = import$skip_header)
  if (is.null(raw)) {
    start <- data_start(is_data)
    if (is.na(start)) {
      stop("No data found in '", basename(path), "' with the import settings (separator ",
           "and column numbers). Is it a complete runfile of this instrument?", call. = FALSE)
    }
    raw <- read_cycles(path, import, skip = start - 1L)
    if (is.null(raw)) {
      stop("Could not read the data in '", basename(path), "'.", call. = FALSE)
    }
    if (start != expected) attr(raw, "data_start") <- start
  }
  dplyr::mutate(raw, sample_id = sample_id, cycle = dplyr::row_number(), .before = 1) |>
    dplyr::relocate(time_s, .after = cycle)
}

# The numeric cycle data after skipping `skip` lines, or NULL if those lines
# don't contain data in the expected columns.
read_cycles <- function(path, import, skip) {
  cols <- unlist(import$columns)
  # Lines of a different shape (e.g. header lines when the header length is
  # wrong) make readr warn; rows that don't parse as numbers are dropped.
  raw <- suppressWarnings(readr::read_delim(
    path,
    delim = import$sep,
    skip = skip,
    col_names = isTRUE(import$header) && skip == import$skip_header,
    col_types = readr::cols(.default = readr::col_character()),
    name_repair = "minimal",
    trim_ws = TRUE,
    progress = FALSE
  ))
  if (import$skip_footer > 0) raw <- utils::head(raw, -import$skip_footer)
  if (max(cols) > ncol(raw)) return(NULL)

  raw <- raw[, cols]
  names(raw) <- names(cols)
  raw <- raw |>
    dplyr::mutate(dplyr::across(dplyr::everything(), \(x) suppressWarnings(as.numeric(x)))) |>
    dplyr::filter(!is.na(raw88), !is.na(time_s))
  if (nrow(raw) == 0) NULL else raw
}

# Lines of a text file as valid UTF-8. Instrument headers can contain
# Latin-1 characters (e.g. "\u00b5V", "\u00b0"), which would make text functions fail
# in a UTF-8 session; every byte is valid Latin-1, so converting from it
# never fails (the numbers and timestamps that are read are plain ASCII).
read_text_lines <- function(path, n = -1L) {
  lines <- readLines(path, n = n, warn = FALSE)
  bad <- !validUTF8(lines)
  lines[bad] <- iconv(lines[bad], from = "latin1", to = "UTF-8")
  lines
}

# For each line of a runfile: does it have numbers in all the columns the
# import settings use?
data_lines <- function(path, import) {
  cols <- unlist(import$columns)
  lines <- read_text_lines(path)
  fields <- strsplit(gsub("\"", "", lines), import$sep, fixed = TRUE)
  vapply(fields, \(f) {
    length(f) >= max(cols) && !anyNA(suppressWarnings(as.numeric(f[cols])))
  }, logical(1))
}

# First line of the first block of (at least) 5 consecutive data lines; NA
# if there is none.
data_start <- function(is_data) {
  runs <- rle(is_data)
  ends <- cumsum(runs$lengths)
  block <- which(runs$values & runs$lengths >= 5)[1]
  if (is.na(block)) NA_integer_ else as.integer(ends[block] - runs$lengths[block] + 1L)
}
