#' Krypton warnings per sample
#'
#' The highest measured 83Kr signal and 86Kr share of mass 86 of each
#' sample's profile (after trims), and whether either exceeds the project's
#' warning levels (`settings$qc`: `kr83_max_v`, default 1e-3 V, and
#' `kr86_max_pct`, default 1%). 86Kr is estimated from 83Kr with the natural
#' 86Kr/83Kr ratio; both come from the blank-subtracted signals.
#' The warnings only flag samples; the data are not changed.
#'
#' @param data Reduced data (from [project_data()]).
#' @param qc The `qc` section of the project settings.
#' @return A tibble with `sample_id`, `kr83_max_v`, `kr86_max_pct` and
#'   `kr_flag`.
#' @export
krypton_summary <- function(data, qc = default_settings()$qc) {
  if (!all(c("kr83_v", "kr86_pct") %in% names(data))) {
    ids <- unique(data$sample_id)
    return(tibble::tibble(sample_id = ids, kr83_max_v = NA_real_, kr86_max_pct = NA_real_,
                          kr_flag = NA))
  }
  safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
  data |>
    dplyr::summarise(kr83_max_v = safe_max(.data$kr83_v),
                     kr86_max_pct = safe_max(.data$kr86_pct),
                     .by = "sample_id") |>
    dplyr::mutate(kr_flag = dplyr::coalesce(.data$kr83_max_v > qc$kr83_max_v, FALSE) |
                    dplyr::coalesce(.data$kr86_max_pct > qc$kr86_max_pct, FALSE))
}
