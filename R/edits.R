# Profile edits are stored as parameters and applied to freshly reduced data,
# so changing a reduction setting never loses trims, regions or changepoints.
# Trims and regions are stored in seconds of analysis time rather than
# micrometres, so they stay on the same part of the sample if the run speed is
# corrected later.

#' Apply trims and profile direction
#'
#' @param data Reduced data (`reduce_runs()$data`).
#' @param edits A data frame with one row per edited sample and columns
#'   `sample_id`, `trim_start_s`, `trim_end_s` (seconds removed after the
#'   run's first cycle and before its last; see below), `reverse` (flip the
#'   distance axis) and `rezero` (measure distance from the first kept
#'   point). Missing columns and samples without a row use no trim, no
#'   reversal and `rezero = TRUE`.
#'
#' Trims are counted from the run's first and last cycle (`run_start_s` and
#' `run_end_s` in reduced data), not from the first point passing the 88Sr
#' thresholds, so a trim stays on the same part of the sample when reduction
#' settings change. [project_data()] applies them during the reduction, before
#' outlier rejection and smoothing; applying them again here changes nothing.
#' @return The edited data, sorted by sample and distance.
#' @export
apply_edits <- function(data, edits = NULL) {
  defaults <- list(trim_start_s = 0, trim_end_s = 0, reverse = FALSE, rezero = TRUE)
  if (is.null(edits)) {
    edits <- tibble::tibble(sample_id = character())
  }
  for (nm in names(defaults)) {
    if (!nm %in% names(edits)) edits[[nm]] <- rep(defaults[[nm]], nrow(edits))
  }
  edits <- dplyr::select(edits, "sample_id", dplyr::all_of(names(defaults)))

  data |>
    trim_runs(edits) |>
    dplyr::left_join(dplyr::select(edits, "sample_id", "reverse", "rezero"), by = "sample_id") |>
    tidyr::replace_na(defaults[c("reverse", "rezero")]) |>
    dplyr::mutate(
      distance_um = dplyr::if_else(rezero, distance_um - min(distance_um), distance_um),
      distance_um = dplyr::if_else(reverse, max(distance_um) + min(distance_um) - distance_um,
                                   distance_um),
      .by = sample_id
    ) |>
    dplyr::arrange(factor(sample_id, levels = unique(sample_id)), distance_um) |>
    dplyr::select(-"reverse", -"rezero")
}

#' Summarise regions of a profile
#'
#' @param data Reduced (and usually edited) data.
#' @param regions A data frame with columns `sample_id`, `region` (label),
#'   `start_s` and `end_s` (analysis time). Regions may overlap.
#' @param value Column to summarise; defaults to the value selected by the
#'   smoothing setting.
#' @return One row per sample and region with the mean, standard deviation,
#'   number of points, standard error and distance range.
#' @export
summarise_regions <- function(data, regions, value = "sr87_sr86_value") {
  data |>
    dplyr::select("sample_id", "time_s", "distance_um", value = dplyr::all_of(value)) |>
    dplyr::inner_join(regions, by = dplyr::join_by(sample_id, dplyr::between(time_s, start_s, end_s))) |>
    dplyr::summarise(
      mean = mean(.data$value, na.rm = TRUE),
      sd = stats::sd(.data$value, na.rm = TRUE),
      n = sum(!is.na(.data$value)),
      se = sd / sqrt(n),
      distance_min_um = min(distance_um),
      distance_max_um = max(distance_um),
      .by = c(sample_id, region)
    )
}

#' Detect changepoints in a profile
#'
#' Runs `changepoint::cpt.mean()` on each sample and adds the segment number
#' and segment mean. Segment means are calculated within each sample (v1
#' pooled samples that shared a segment number).
#'
#' @param data Reduced (and usually edited) data, sorted by distance.
#' @param method Changepoint method: `"PELT"`, `"AMOC"`, `"SegNeigh"` or
#'   `"BinSeg"`.
#' @param penalty Manual penalty value.
#' @param value Column to analyse.
#' @return `data` with `cp_segment` and `cp_mean` added (`NA` where the value
#'   is missing).
#' @export
find_changepoints <- function(data, method = "PELT", penalty = 5e-5,
                              value = "sr87_sr86_value") {
  segment_one <- function(x) {
    ok <- !is.na(x)
    seg <- rep(NA_integer_, length(x))
    if (sum(ok) < 2) return(seg)
    cpt <- changepoint::cpt.mean(x[ok], penalty = "Manual", pen.value = penalty,
                                 method = method, test.stat = "Normal", class = TRUE)
    cpts <- changepoint::cpts(cpt)
    seg[ok] <- findInterval(seq_len(sum(ok)) - 1L, cpts) + 1L
    seg
  }
  data |>
    dplyr::mutate(cp_segment = segment_one(.data[[value]]), .by = sample_id) |>
    dplyr::mutate(
      cp_mean = dplyr::if_else(is.na(cp_segment), NA_real_, mean(.data[[value]], na.rm = TRUE)),
      .by = c(sample_id, cp_segment)
    )
}

# Trims shown to users: micrometres cut from the start and end of the
# profile (the cycles passing the thresholds). `times` has the run's
# `run_start`, `run_end`, `profile_start` and `profile_end` (s).
trims_to_um <- function(trim_start_s, trim_end_s, times, speed) {
  c(start = max(0, times$run_start + trim_start_s - times$profile_start) * speed,
    end = max(0, times$profile_end - (times$run_end - trim_end_s)) * speed)
}

# The stored trim (s after the run's first cycle, or before its last) for
# `um` micrometres cut from the start or end of the profile. 0 means no trim.
um_to_trims <- function(um, which = c("start", "end"), times, speed) {
  which <- match.arg(which)
  if (um <= 0) return(0)
  if (which == "start") {
    times$profile_start - times$run_start + um / speed
  } else {
    times$run_end - times$profile_end + um / speed
  }
}
