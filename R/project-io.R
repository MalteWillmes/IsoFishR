# Low-level reading and writing of the project folder files.
#
# All tables are plain CSV with fixed column types, so sample IDs such as
# "001" stay text and the files can be opened in a spreadsheet. Files are
# written to a temporary name and then renamed, so an interrupted save (or a
# Dropbox sync in the middle of a write) never leaves a half-written file.

PROJECT_FORMAT <- 1L

project_file <- function(path, ...) file.path(path, ...)

# Empty tables that define the columns and types of each project table.
samples_template <- function() {
  tibble::tibble(
    sample_id = character(),
    runfile = character(),
    source = character(),
    md5 = character(),
    added = character(),
    added_by = character(),
    status = character(),
    flag = logical(),
    comment = character(),
    trim_start_s = double(),
    trim_end_s = double(),
    reverse = logical(),
    rezero = logical(),
    cp_enabled = logical(),
    cp_method = character(),
    cp_penalty = double(),
    settings = character(),
    acquired = character(),
    run_type = character(),
    reference_material = character(),
    session = character()
  )
}

# Values for a newly added sample; also used to fill columns missing from
# files written by an older version.
sample_defaults <- function() {
  list(status = "new", flag = FALSE, comment = "", trim_start_s = 0, trim_end_s = 0,
       reverse = FALSE, rezero = TRUE, cp_enabled = FALSE, cp_method = "PELT",
       cp_penalty = 5e-5, settings = "", acquired = "", run_type = "sample",
       reference_material = "", session = "")
}

SAMPLE_STATUS <- c("new", "reviewed", "excluded")
RUN_TYPES <- c("sample", "baseline", "reference")
EDITABLE_SAMPLE_FIELDS <- c("status", "flag", "comment", "trim_start_s", "trim_end_s",
                            "reverse", "rezero", "cp_enabled", "cp_method", "cp_penalty")

reference_materials_template <- function() {
  tibble::tibble(name = character(), value = double(), uncertainty_2s = double(),
                 note = character())
}

regions_template <- function() {
  tibble::tibble(sample_id = character(), region = character(),
                 start_s = double(), end_s = double())
}

log_template <- function() {
  tibble::tibble(time = character(), user = character(), action = character(),
                 sample_id = character(), field = character(),
                 old = character(), new = character())
}

read_table_file <- function(path, template, defaults = list()) {
  if (!file.exists(path)) return(template)
  spec <- purrr::map(template, \(x) switch(
    typeof(x),
    character = readr::col_character(),
    logical = readr::col_logical(),
    double = readr::col_double(),
    integer = readr::col_integer()
  ))
  x <- readr::read_csv(path, col_types = do.call(readr::cols, spec), na = "",
                       progress = FALSE, show_col_types = FALSE)
  # Columns added in later versions get their default (or NA) values.
  for (nm in setdiff(names(template), names(x))) {
    x[[nm]] <- if (nm %in% names(defaults)) defaults[[nm]] else template[[nm]][NA_integer_]
  }
  x |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), \(v) dplyr::coalesce(v, ""))) |>
    dplyr::relocate(dplyr::all_of(names(template)))
}

write_table_file <- function(x, path) {
  tmp <- paste0(path, ".tmp")
  readr::write_csv(x, tmp, na = "")
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    stop("Could not save '", basename(path), "'. Is it open in another program?",
         call. = FALSE)
  }
  invisible(path)
}

write_yaml_file <- function(x, path) {
  tmp <- paste0(path, ".tmp")
  yaml::write_yaml(x, tmp)
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    stop("Could not save '", basename(path), "'.", call. = FALSE)
  }
  invisible(path)
}

append_log <- function(path, entries) {
  if (nrow(entries) == 0) return(invisible(path))
  entries <- dplyr::mutate(entries, dplyr::across(dplyr::everything(), as.character))
  readr::write_csv(entries, path, append = file.exists(path), na = "")
  invisible(path)
}

# Per-sample reduction setting overrides are stored in samples.csv as
# "key=value; key=value", which stays readable in a spreadsheet.
format_overrides <- function(overrides) {
  if (length(overrides) == 0) return("")
  vals <- vapply(overrides, \(v) if (is.logical(v)) toupper(as.character(v)) else format(v, digits = 15),
                 character(1))
  paste0(names(overrides), "=", vals, collapse = "; ")
}

# A sample can override every reduction setting and these sample settings.
SAMPLE_OVERRIDE_KEYS <- "run_speed_um_s"

# The project values of the settings a sample can override, as one flat list.
override_defaults <- function(settings) {
  c(settings$reduction, settings$sample[SAMPLE_OVERRIDE_KEYS])
}

# Overrides (flat list) as a partial settings list for merge_settings().
overrides_to_settings <- function(overrides) {
  is_sample <- names(overrides) %in% SAMPLE_OVERRIDE_KEYS
  list(reduction = overrides[!is_sample], sample = overrides[is_sample])
}

parse_overrides <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) return(list())
  defaults <- override_defaults(default_settings())
  pairs <- strsplit(strsplit(x, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)
  keys <- trimws(vapply(pairs, `[`, character(1), 1))
  vals <- trimws(vapply(pairs, `[`, character(1), 2))
  # No longer a setting (the spline sets its own basis size).
  old <- keys == "spline_k"
  keys <- keys[!old]
  vals <- vals[!old]
  unknown <- setdiff(keys, names(defaults))
  if (length(unknown) > 0) {
    stop("Unknown sample setting(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  purrr::map2(keys, vals, \(k, v) coerce_like(v, defaults[[k]])) |> rlang::set_names(keys)
}

coerce_like <- function(value, template) {
  switch(typeof(template),
         integer = as.integer(value),
         double = as.numeric(value),
         logical = as.logical(value),
         as.character(value))
}

# Flatten a nested settings list to "section.key" = "value" strings.
flatten_settings <- function(x, prefix = "") {
  out <- character()
  for (k in names(x)) {
    key <- if (nzchar(prefix)) paste0(prefix, ".", k) else k
    v <- x[[k]]
    out <- c(out, if (is.list(v)) flatten_settings(v, key) else
      rlang::set_names(paste(v, collapse = ", "), key))
  }
  out
}

format_regions <- function(regions) {
  if (nrow(regions) == 0) return("")
  paste0(regions$region, ": ", as.character(regions$start_s), "-",
         as.character(regions$end_s), " s", collapse = "; ")
}

# A time stamp (format `fmt`) not yet used as a name in `dir`: exports or
# moved files within the same second get a suffix (_2, _3, ...).
unique_stamp <- function(dir, fmt) {
  base <- format(Sys.time(), fmt)
  candidate <- base
  i <- 1L
  while (file.exists(file.path(dir, candidate))) {
    i <- i + 1L
    candidate <- paste0(base, "_", i)
  }
  candidate
}

# A file name in `dir` that doesn't exist yet: `name`, or `name` with a
# suffix (_2, _3, ...) before its extension.
unique_file <- function(dir, name) {
  stem <- tools::file_path_sans_ext(name)
  ext <- tools::file_ext(name)
  candidate <- name
  i <- 1L
  while (file.exists(file.path(dir, candidate))) {
    i <- i + 1L
    candidate <- paste0(stem, "_", i, if (nzchar(ext)) paste0(".", ext))
  }
  candidate
}
