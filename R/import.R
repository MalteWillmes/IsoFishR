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

read_runfile <- function(path, import, sample_id) {
  cols <- unlist(import$columns)
  raw <- readr::read_delim(
    path,
    delim = import$sep,
    skip = import$skip_header,
    col_names = isTRUE(import$header),
    col_types = readr::cols(.default = readr::col_character()),
    name_repair = "minimal",
    trim_ws = TRUE,
    progress = FALSE
  )
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
