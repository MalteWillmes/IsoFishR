#' Instrument import presets
#'
#' A preset is a named runfile layout: header and footer lines to skip, the
#' separator, and which column holds each mass and the time. IsoFishR ships
#' with `"Nu_UCDavis"`; users can save their own presets, which are stored as
#' YAML files in their IsoFishR config folder (`import_preset_dir()`) and can
#' be shared by copying the file.
#'
#' A project records the name of the preset it was set up with, and a full
#' copy of its values, so a project keeps working even where the preset is
#' not installed.
#'
#' @param name Preset name (letters, numbers, `_`, `-` and `.`).
#' @param import The `import` section of a settings list.
#' @param description Optional one-line description.
#' @param overwrite Replace an existing user preset of the same name.
#' @name import_presets
NULL

DEFAULT_PRESET <- "Nu_UCDavis"

#' @rdname import_presets
#' @return `list_import_presets()`: a tibble with `name`, `description` and
#'   `builtin`.
#' @export
list_import_presets <- function() {
  files <- c(builtin_preset_files(), user_preset_files())
  builtin <- c(rep(TRUE, length(builtin_preset_files())), rep(FALSE, length(user_preset_files())))
  names <- tools::file_path_sans_ext(basename(files))
  keep <- !duplicated(names)  # built-in presets cannot be shadowed
  descriptions <- vapply(files[keep], \(f) {
    tryCatch(yaml::read_yaml(f)$description %||% "", error = function(e) "")
  }, character(1))
  tibble::tibble(name = names[keep], description = unname(descriptions), builtin = builtin[keep])
}

#' @rdname import_presets
#' @return `get_import_preset()`: the `import` settings of the preset, with
#'   `preset` set to its name.
#' @export
get_import_preset <- function(name) {
  file <- preset_file(name)
  if (is.null(file)) {
    stop("Unknown import preset '", name, "'. Available: ",
         paste(list_import_presets()$name, collapse = ", "), call. = FALSE)
  }
  x <- yaml::read_yaml(file)
  x$description <- NULL
  import <- utils::modifyList(default_settings()$import, x)
  import$preset <- name
  check_import(import)
}

#' @rdname import_presets
#' @return `save_import_preset()`: the path of the preset file, invisibly.
#' @export
save_import_preset <- function(name, import, description = "", overwrite = FALSE) {
  if (!grepl("^[A-Za-z0-9_.-]+$", name)) {
    stop("Preset names may only contain letters, numbers, _, - and .", call. = FALSE)
  }
  if (name %in% tools::file_path_sans_ext(basename(builtin_preset_files()))) {
    stop("'", name, "' is a built-in preset and cannot be replaced. Choose another name.",
         call. = FALSE)
  }
  path <- file.path(import_preset_dir(), paste0(name, ".yml"))
  if (file.exists(path) && !overwrite) {
    stop("A preset called '", name, "' already exists. Use overwrite = TRUE to replace it.",
         call. = FALSE)
  }
  check_import(import)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  import$preset <- NULL
  yaml::write_yaml(c(list(description = description), import), path)
  invisible(path)
}

#' @rdname import_presets
#' @export
delete_import_preset <- function(name) {
  path <- file.path(import_preset_dir(), paste0(name, ".yml"))
  if (!file.exists(path)) {
    stop("No user preset called '", name, "' (built-in presets cannot be deleted).", call. = FALSE)
  }
  unlink(path)
  invisible(NULL)
}

#' @rdname import_presets
#' @return `import_preset_dir()`: the folder holding user presets. Set the
#'   option `isofishr.preset_dir` to use a different (e.g. shared) folder.
#' @export
import_preset_dir <- function() {
  getOption("isofishr.preset_dir", file.path(tools::R_user_dir("IsoFishR", "config"), "import_presets"))
}

#' Does an import section still match its named preset?
#'
#' @param import The `import` section of a settings list.
#' @return `TRUE` if `import` names a preset that exists and has the same
#'   values; `FALSE` if the values were changed or the preset is missing.
#' @keywords internal
import_matches_preset <- function(import) {
  name <- import$preset %||% ""
  if (!nzchar(name) || is.null(preset_file(name))) return(FALSE)
  preset <- tryCatch(get_import_preset(name), error = function(e) NULL)
  !is.null(preset) && identical(import_values(preset), import_values(import))
}

# Import settings without the preset name, e.g. for cache keys and comparisons.
import_values <- function(import) {
  import$preset <- NULL
  import$columns <- lapply(import$columns, as.integer)
  import$skip_header <- as.integer(import$skip_header)
  import$skip_footer <- as.integer(import$skip_footer)
  import$timestamp_line <- as.integer(import$timestamp_line %||% 0L)
  import
}

check_import <- function(import) {
  cols <- unlist(import$columns)
  required <- c("raw88", "raw87", "raw86", "raw85", "raw84", "raw83", "time_s")
  missing <- setdiff(required, names(cols))
  if (length(missing) > 0) {
    stop("Import settings are missing column numbers for: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (any(is.na(cols)) || any(cols < 1) || any(cols != round(cols))) {
    stop("Column numbers must be whole numbers of 1 or more.", call. = FALSE)
  }
  if (anyDuplicated(cols[required])) {
    stop("Each mass and the time must be read from a different column.", call. = FALSE)
  }
  for (nm in c("skip_header", "skip_footer")) {
    v <- import[[nm]]
    if (!is.numeric(v) || length(v) != 1 || is.na(v) || v < 0) {
      stop("import$", nm, " must be a number of 0 or more.", call. = FALSE)
    }
  }
  if (!is.character(import$sep) || length(import$sep) != 1 || nchar(import$sep) != 1) {
    stop("import$sep must be a single character, such as \",\" or \"\\t\".", call. = FALSE)
  }
  line <- import$timestamp_line %||% 0
  if (!is.numeric(line) || length(line) != 1 || is.na(line) || line < 0) {
    stop("import$timestamp_line must be a line number, or 0 if the files have no timestamp.",
         call. = FALSE)
  }
  if (line > 0 && (!is.character(import$timestamp_format) || !nzchar(import$timestamp_format))) {
    stop("import$timestamp_format is needed to read the timestamp line.", call. = FALSE)
  }
  invisible(import)
}

builtin_preset_files <- function() {
  sort(list.files(system.file("presets", package = "IsoFishR"), "\\.yml$", full.names = TRUE))
}

user_preset_files <- function() {
  sort(list.files(import_preset_dir(), "\\.yml$", full.names = TRUE))
}

preset_file <- function(name) {
  files <- c(builtin_preset_files(), user_preset_files())
  match <- files[tools::file_path_sans_ext(basename(files)) == name]
  if (length(match) == 0) NULL else match[1]
}
