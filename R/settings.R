#' Default project settings
#'
#' Settings are a nested list with four sections: `sample` (metadata that does
#' not affect calculations), `import` (runfile layout), `reduction` (data
#' reduction parameters) and `regions` (default region labels). Projects store
#' them as `project.yml`; individual samples only store values that differ
#' from these defaults.
#'
#' @return A nested list of settings.
#' @export
default_settings <- function() {
  list(
    sample = list(
      user = "",
      sample_type = "Otolith",
      analysis_type = "line",
      direction = "",
      spot_size_um = 40,
      run_speed_um_s = 10,
      fluence_j_cm2 = 1.85,
      laser_energy_pct = 55
    ),
    import = list(
      skip_header = 46L,
      skip_footer = 0L,
      sep = ",",
      header = FALSE,
      columns = list(raw88 = 1L, raw87 = 2L, raw86 = 3L, raw85 = 4L,
                     raw84 = 5L, raw83 = 6L, time_s = 8L)
    ),
    reduction = list(
      integration = 5L,
      blank_correction = TRUE,
      blank_time_s = 30,
      raw88_min_v = 0.5,
      raw88_max_v = 9.8,
      sr86_sr88_ref = 0.1194,
      rb85_rb87 = 2.59712,
      outlier_sd = 3,
      outlier_window = 20L,
      smoothing = "ma",
      ma_window = 10L,
      spline_k = 10L
    ),
    regions = list(
      default_labels = paste("Region", 1:8)
    )
  )
}

#' Combine settings with overrides
#'
#' @param settings A settings list, usually from [default_settings()].
#' @param overrides A (partial) nested list of values that replace those in
#'   `settings`.
#' @return The combined settings, validated with [check_settings()].
#' @export
merge_settings <- function(settings, overrides = list()) {
  check_settings(utils::modifyList(settings, overrides))
}

#' Validate settings
#'
#' @param settings A settings list.
#' @return `settings`, invisibly; errors describe the first problem found.
#' @export
check_settings <- function(settings) {
  r <- settings$reduction
  if (!r$smoothing %in% c("none", "ma", "spline")) {
    stop("reduction$smoothing must be one of 'none', 'ma' or 'spline', not '",
         r$smoothing, "'.", call. = FALSE)
  }
  for (nm in c("integration", "outlier_window", "ma_window", "spline_k")) {
    if (!is.numeric(r[[nm]]) || length(r[[nm]]) != 1 || r[[nm]] < 1) {
      stop("reduction$", nm, " must be a single number >= 1.", call. = FALSE)
    }
  }
  if (r$raw88_min_v >= r$raw88_max_v) {
    stop("reduction$raw88_min_v must be smaller than raw88_max_v.", call. = FALSE)
  }
  if (!(is.numeric(r$outlier_sd) && length(r$outlier_sd) == 1 && r$outlier_sd >= 0)) {
    stop("reduction$outlier_sd must be a single number >= 0 (0 turns outlier rejection off).",
         call. = FALSE)
  }
  invisible(settings)
}

#' Read and write project settings
#'
#' Settings files are YAML. Values missing from the file fall back to
#' [default_settings()], so older settings files keep working when new
#' settings are added.
#'
#' @param path Path to a settings YAML file.
#' @param settings A settings list.
#' @name settings_io
NULL

#' @rdname settings_io
#' @export
read_settings <- function(path) {
  merge_settings(default_settings(), yaml::read_yaml(path))
}

#' @rdname settings_io
#' @export
write_settings <- function(settings, path) {
  check_settings(settings)
  yaml::write_yaml(settings, path)
  invisible(path)
}

#' Convert a v1 project settings file
#'
#' Reads the `<project>_settings.csv` written by IsoFishR v1 and returns the
#' equivalent v2 settings list.
#'
#' @param path Path to a v1 `_settings.csv` file.
#' @return A settings list.
#' @export
import_v1_settings <- function(path) {
  v1 <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("NA", ""))
  v1 <- as.list(v1[1, , drop = TRUE])
  num <- function(x) as.numeric(x)
  int <- function(x) as.integer(x)

  smoothing <- switch(tolower(v1$smoother),
                      ma = "ma", spline = "spline", data_points = "none", "ma")
  sep <- if (is.na(v1$sep)) "\t" else v1$sep
  range_labels <- unlist(v1[paste0("range", 1:8, "_label")], use.names = FALSE)

  merge_settings(default_settings(), list(
    sample = list(
      user = if (is.na(v1$username)) "" else v1$username,
      sample_type = v1$sampletype,
      analysis_type = tolower(v1$analysistype),
      direction = if (is.na(v1$sampledirection)) "" else v1$sampledirection,
      spot_size_um = num(v1$spotsize),
      run_speed_um_s = num(v1$speed),
      fluence_j_cm2 = num(v1$fluency),
      laser_energy_pct = num(v1$energy)
    ),
    import = list(
      skip_header = int(v1$vskip),
      skip_footer = int(v1$fskip),
      sep = sep,
      header = as.logical(v1$header),
      columns = list(raw88 = int(v1$raw88), raw87 = int(v1$raw87),
                     raw86 = int(v1$raw86), raw85 = int(v1$raw85),
                     raw84 = int(v1$raw84), raw83 = int(v1$raw83),
                     time_s = int(v1$cyclesec))
    ),
    reduction = list(
      integration = int(v1$integration),
      blank_correction = as.logical(v1$blanksubcheck),
      blank_time_s = num(v1$blanktime),
      raw88_min_v = num(v1$raw88lowerthresh),
      raw88_max_v = num(v1$raw88upperthresh),
      sr86_sr88_ref = num(v1$Sr8688ratio),
      rb85_rb87 = num(v1$Rb8587ratio),
      outlier_sd = if (is.na(v1$SD_outlier)) 0 else num(v1$SD_outlier),
      outlier_window = int(v1$outlier_num),
      smoothing = smoothing,
      ma_window = int(v1$average_num),
      spline_k = int(v1$spline_k)
    ),
    regions = list(default_labels = range_labels)
  ))
}
