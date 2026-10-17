# Compare the v2 reduction with the original v1 code on the example runfiles.
#
# The v1 reduction code is taken verbatim from IsoFishR.R at the v1.1 tag
# (the `raw_data_all` and `processed_data` reactives), turned into plain
# functions and run with the same settings as v2.
#
# Run from the repository root:  Rscript dev/compare_v1.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(zoo)
  library(mgcv)
  library(broom)
  devtools::load_all(quiet = TRUE)
})

# ---- v1 code ----------------------------------------------------------------
v1_src <- system("git show v1.1:IsoFishR.R", intern = TRUE)
from <- grep("^  raw_data_all <- reactive", v1_src)
to <- grep("^####Create data subsets", v1_src) - 1
v1_code <- sub("<- reactive\\(\\{", "<- function() {", v1_src[from:to])
v1_code <- sub("^    return\\((raw_data_all|processed)\\)\\}\\)", "    return(\\1)}", v1_code)

run_v1 <- function(files, input) {
  env <- new.env()
  env$input <- input
  eval(parse(text = v1_code), envir = env)
  env$processed_data()
}

# ---- shared settings ----------------------------------------------------------
files <- list.files("inst/extdata/runfiles", pattern = "\\.run$", recursive = TRUE, full.names = TRUE)
s <- default_settings()
r <- s$reduction

input <- list(
  file = data.frame(name = basename(files), size = file.size(files), type = "",
                    datapath = files, stringsAsFactors = FALSE),
  vskip = s$import$skip_header, fskip = s$import$skip_footer, sep = s$import$sep,
  header = s$import$header,
  raw88 = 1, raw87 = 2, raw86 = 3, raw85 = 4, raw84 = 5, raw83 = 6, cyclesec = 8,
  integration = r$integration, blanksubcheck = r$blank_correction, blanktime = r$blank_time_s,
  raw88lowerthresh = r$raw88_min_v, raw88upperthresh = r$raw88_max_v,
  speed = s$sample$run_speed_um_s, Sr8688ratio = r$sr86_sr88_ref, Rb8587ratio = r$rb85_rb87,
  SD_outlier = as.character(r$outlier_sd), outlier_num = r$outlier_window,
  average_num = r$ma_window, narem = "TRUE", rollfill = "extend", spline_k = r$spline_k
)

# ---- run both -----------------------------------------------------------------
t_v1 <- system.time(v1 <- suppressWarnings(run_v1(files, input)))[["elapsed"]]
t_v2 <- system.time(v2 <- reduce_runs(read_runfiles(files), s)$data)[["elapsed"]]
s_spline <- merge_settings(s, list(reduction = list(smoothing = "spline")))
t_v2_spline <- system.time(reduce_runs(read_runfiles(files), s_spline))[["elapsed"]]

cat(sprintf("\nTime for 6 runfiles: v1 %.1f s | v2 %.2f s (moving average) | v2 %.2f s (with spline)\n\n",
            t_v1, t_v2, t_v2_spline))

# ---- compare per cycle ----------------------------------------------------------
v1_tidy <- v1 |>
  ungroup() |>
  transmute(sample_id = tools::file_path_sans_ext(Run_ID), time_s = round(CycleSecs, 1),
            v1_ratio = Sr87Sr86, v1_ma = Sr87Sr86_MA, v1_outlier = !is.na(Sr87Sr86_outlier))
v2_tidy <- v2 |>
  transmute(sample_id, time_s = round(time_s, 1),
            v2_ratio = sr87_sr86, v2_ma = sr87_sr86_ma, v2_outlier = is_outlier)

per_sample <- function(d, prefix, ratio, outlier) {
  d |>
    summarise(cycles = n(), outliers = sum({{ outlier }}),
              first_time = min(time_s), mean_ratio = mean({{ ratio }}, na.rm = TRUE),
              .by = sample_id) |>
    rename_with(\(x) paste0(prefix, x), -sample_id)
}

# Cycle-by-cycle differences, only where both versions have a cycle at the same time
matched <- inner_join(v1_tidy, v2_tidy, by = c("sample_id", "time_s")) |>
  summarise(matched = n(),
            max_diff_ratio = max(abs(v1_ratio - v2_ratio), na.rm = TRUE),
            .by = sample_id)

summary_tbl <- per_sample(v1_tidy, "v1_", v1_ratio, v1_outlier) |>
  left_join(per_sample(v2_tidy, "v2_", v2_ratio, v2_outlier), by = "sample_id") |>
  left_join(matched, by = "sample_id") |>
  mutate(mean_diff_ppm = (v2_mean_ratio - v1_mean_ratio) / v1_mean_ratio * 1e6) |>
  select(sample_id, v1_cycles, v2_cycles, v1_first_time, v2_first_time, matched,
         v1_outliers, v2_outliers, max_diff_ratio, mean_diff_ppm)

# v1 run one file at a time avoids its integration bug (blocks spanning two
# runfiles), which isolates how much of the difference that bug causes.
v1_single <- map(files, \(f) {
  inp <- input
  inp$file <- input$file[input$file$datapath == f, ]
  suppressWarnings(run_v1(f, inp))
}) |>
  list_rbind() |>
  ungroup() |>
  mutate(sample_id = tools::file_path_sans_ext(Run_ID)) |>
  summarise(v1_single_mean = mean(Sr87Sr86, na.rm = TRUE), .by = sample_id)

summary_tbl <- summary_tbl |>
  left_join(v1_single, by = "sample_id") |>
  left_join(summarise(v2_tidy, v2_mean = mean(v2_ratio, na.rm = TRUE), .by = sample_id),
            by = "sample_id") |>
  mutate(single_file_diff_ppm = (v2_mean - v1_single_mean) / v1_single_mean * 1e6) |>
  select(-v1_single_mean, -v2_mean)

print(summary_tbl, width = Inf)
