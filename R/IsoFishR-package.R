#' @keywords internal
#' @importFrom mgcv s
#' @importFrom rlang .data :=
"_PACKAGE"

# Column names used inside dplyr verbs (avoids R CMD check notes)
utils::globalVariables(c(
  "sample_id", "cycle", "time_s", "distance_um",
  "raw88", "raw87", "raw86", "raw85", "raw84", "raw83",
  "blk88", "blk87", "blk86", "blk85", "blk84", "blk83",
  "sr86_sr88", "mass_bias", "rb87", "sr87", "sr87_sr86", "sr87_sr86_rb",
  "sr87_sr86_outlier", "sr87_sr86_ma", "sr87_sr86_ma_sd", "sr87_sr86_ma_se",
  "sr87_sr86_ma_n", "sr87_sr86_spline", "sr87_sr86_spline_se", "sr87_sr86_value",
  "is_outlier", "trim_start_s", "trim_end_s", "reverse", "rezero",
  "region", "start_s", "end_s", "cp_segment", "cp_mean", "sr84_sr86", "sd", "n"
))
