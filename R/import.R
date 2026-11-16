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
#' Reads the date and time a run was started from its header line
#' `timestamp_line` (for Nu Plasma files line 45, e.g.
#' `"22/07/2025","11:06:47 AM"`). Quotes are removed and commas or tabs
#' between the date and time become spaces.
#'
#' With `timestamp_format = "auto"` (the default) the line is read
#' flexibly: day/month/year, month/day/year or year-month-day with `/`, `.`
#' or `-`, 2- or 4-digit years, English month names (`8 Aug 2016`,
#' `Aug 8, 2016`), and 12- or 24-hour times with or without seconds. Whether
#' a numeric date is day- or month-first is decided from all the lines read
#' together (and those in `context`): a first number above 12 means
#' day-first, a second number above 12 month-first; if no date decides it,
#' day-first is assumed, as Nu instruments write. Any other
#' `timestamp_format` is used with [base::strptime()], and lines it can't
#' read are then read automatically.
#'
#' Times are treated as the instrument clock and returned in UTC, so
#' daylight saving changes never reorder runs.
#'
#' @param paths Paths to runfiles.
#' @param import The `import` section of a settings list.
#' @param context Paths of other runfiles of the same project, used only to
#'   decide the day/month order (e.g. the runs already in the project when
#'   adding a few new ones).
#' @return A POSIXct vector (`NA` where there is no timestamp line or it
#'   could not be read).
#' @export
read_acquisition_times <- function(paths, import = default_settings()$import,
                                   context = character()) {
  line_no <- as.integer(import$timestamp_line %||% 0L)
  if (length(paths) == 0 || is.na(line_no) || line_no < 1) {
    return(as.POSIXct(rep(NA_real_, length(paths)), tz = "UTC"))
  }
  text <- timestamp_text(paths, line_no)
  fmt <- import$timestamp_format %||% "auto"
  out <- as.POSIXct(rep(NA_real_, length(text)), tz = "UTC")
  if (!identical(fmt, "auto") && nzchar(fmt)) {
    # Parse in the C locale so "AM"/"PM" and month names work on any system.
    old <- Sys.getlocale("LC_TIME")
    on.exit(Sys.setlocale("LC_TIME", old), add = TRUE)
    Sys.setlocale("LC_TIME", "C")
    out <- as.POSIXct(strptime(text, fmt, tz = "UTC"))
  }
  todo <- is.na(out) & !is.na(text)
  if (any(todo)) {
    order <- date_order(c(text, if (length(context)) timestamp_text(context, line_no)))
    out[todo] <- parse_timestamps(text[todo], order)
  }
  out
}

# The timestamp line of each file, with quotes removed and separators
# between the date and time turned into spaces (NA if missing or unreadable;
# a missing file just has no time and must not stop a project opening).
timestamp_text <- function(paths, line_no) {
  # A date and a time on one line (numeric date, or a month name).
  looks_like_time <- function(x) {
    grepl("\\d{1,2}:\\d{2}", x) &
      (grepl(NUM_DATE, x) | grepl("(?i)\\b(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]*\\b", x, perl = TRUE))
  }
  text <- vapply(paths, \(p) {
    # A header with missing or extra lines moves the timestamp: if the
    # expected line has none, the nearest line that does is used.
    x <- tryCatch(read_text_lines(p, n = line_no + 20L), error = function(e) character())
    if (length(x) == 0) return(NA_character_)
    if (length(x) >= line_no && looks_like_time(x[[line_no]])) return(x[[line_no]])
    hits <- which(looks_like_time(x))
    if (length(hits) == 0) return(NA_character_)
    x[[hits[which.min(abs(hits - line_no))]]]
  }, character(1), USE.NAMES = FALSE)
  text <- trimws(gsub("[,\t;]+", " ", gsub("\"", "", text)))
  text[!nzchar(text)] <- NA_character_
  text
}

NUM_DATE <- "(\\d{1,4})[/.-](\\d{1,2})[/.-](\\d{2,4})"

# "dmy" or "mdy" for the numeric dates in `text` (see read_acquisition_times()).
date_order <- function(text) {
  m <- regmatches(text, regexec(NUM_DATE, text))
  m <- m[lengths(m) == 4]
  if (length(m) == 0) return("dmy")
  a <- as.integer(vapply(m, `[`, "", 2))
  b <- as.integer(vapply(m, `[`, "", 3))
  year_first <- nchar(vapply(m, `[`, "", 2)) == 4
  a <- a[!year_first]
  b <- b[!year_first]
  if (any(a > 12, na.rm = TRUE)) "dmy" else if (any(b > 12, na.rm = TRUE)) "mdy" else "dmy"
}

# Parse timestamp text without a fixed format (see read_acquisition_times()).
parse_timestamps <- function(text, order = "dmy") {
  months <- tolower(month.abb)
  one <- function(x) {
    if (is.na(x)) return(NA_real_)
    y <- mo <- d <- NA_integer_
    num <- regmatches(x, regexec(NUM_DATE, x))[[1]]
    if (length(num) == 4) {
      p <- as.integer(num[2:4])
      if (nchar(num[2]) == 4) {
        y <- p[1]; mo <- p[2]; d <- p[3]
      } else if (order == "mdy") {
        mo <- p[1]; d <- p[2]; y <- p[3]
      } else {
        d <- p[1]; mo <- p[2]; y <- p[3]
      }
      rest <- sub(NUM_DATE, " ", x)
    } else {
      # Month names: "8 Aug 2016", "08-Aug-2016", "Aug 8 2016", "August 8, 2016"
      word <- regmatches(x, regexpr("[A-Za-z]{3,9}", x))
      mo <- if (length(word)) match(substr(tolower(word), 1, 3), months) else NA_integer_
      if (is.na(mo)) return(NA_real_)
      no_time <- gsub("\\d{1,2}:\\d{2}(:\\d{2}(\\.\\d+)?)?", " ", x)
      nums <- as.integer(regmatches(no_time, gregexpr("\\d{1,4}", no_time))[[1]])
      if (length(nums) < 2) return(NA_real_)
      y <- nums[nchar(nums) == 4][1] %|% nums[length(nums)]
      d <- setdiff(nums, y)[1]
      rest <- no_time <- x
    }
    if (!is.na(y) && y < 100) y <- y + 2000L
    tm <- regmatches(rest, regexec("(\\d{1,2}):(\\d{2})(?::(\\d{2}(?:\\.\\d+)?))?\\s*([AaPp][Mm])?",
                                   rest, perl = TRUE))[[1]]
    h <- mi <- 0
    s <- 0
    if (length(tm) == 5) {
      h <- as.integer(tm[2])
      mi <- as.integer(tm[3])
      s <- if (nzchar(tm[4])) as.numeric(tm[4]) else 0
      ampm <- toupper(tm[5])
      if (ampm == "PM" && h < 12) h <- h + 12L
      if (ampm == "AM" && h == 12) h <- 0L
    }
    if (anyNA(c(y, mo, d)) || mo < 1 || mo > 12 || d < 1 || d > 31 || h > 23 || mi > 59) {
      return(NA_real_)
    }
    as.numeric(ISOdatetime(y, mo, d, h, mi, s, tz = "UTC"))
  }
  as.POSIXct(vapply(text, one, numeric(1), USE.NAMES = FALSE), origin = "1970-01-01", tz = "UTC")
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
