# Write a synthetic Nu-style runfile (Nu_UCDavis layout: timestamp on line
# 45, data from line 47) whose reduction gives a known 87Sr/86Sr.
#
# `ratio` is what the instrument "measures" after mass bias correction, i.e.
# the true value times any drift the test wants to simulate. `blank_v` is
# added to every mass; `blank_s` seconds at the start are on-peak zeros
# (blank only), as in real runs.
write_runfile <- function(path, acquired, ratio = 0.7092, n = 300, dt = 0.2,
                          raw88 = 4, blank_v = 0, blank_s = 0, beta = -1.6) {
  raw <- make_raw("x", n = n, sr87_sr86 = ratio, raw88 = raw88, beta = beta, dt = dt,
                  blank_s = blank_s)
  cols <- c("raw88", "raw87", "raw86", "raw85", "raw84", "raw83")
  raw[cols] <- raw[cols] + blank_v
  header <- c(rep("\"header\"", 44),
              sprintf("\"%s\",\"%s\"", format(acquired, "%m/%d/%Y", tz = "UTC"),
                      format(acquired, "%I:%M:%S %p", tz = "UTC")),
              "\"Spare text\"")
  body <- sprintf("%s,%s,%s,%s,%s,%s,%d,\"%.1f\",0",
                  format(raw$raw88, digits = 15), format(raw$raw87, digits = 15),
                  format(raw$raw86, digits = 15), format(raw$raw85, digits = 15),
                  format(raw$raw84, digits = 15), format(raw$raw83, digits = 15),
                  raw$cycle, raw$time_s)
  writeLines(c(header, body), path)
  invisible(path)
}

t0 <- as.POSIXct("2024-05-02 09:00:00", tz = "UTC")
at_min <- function(minutes) t0 + minutes * 60

# A project from synthetic runs: `runs` is a data frame with `id`, `minute`
# and `ratio` (plus optional `blank_v`, `blank_s`). Settings: no outlier
# rejection, integration 1, blank correction on.
synthetic_project <- function(runs, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  files <- file.path(dir, paste0(runs$id, ".run"))
  for (i in seq_len(nrow(runs))) {
    write_runfile(files[i], at_min(runs$minute[i]), ratio = runs$ratio[i],
                  blank_v = runs$blank_v[i] %||% 0, blank_s = runs$blank_s[i] %||% 10)
  }
  s <- default_settings()
  s$reduction$integration <- 1L
  s$reduction$outlier_sd <- 0
  s$reduction$blank_time_s <- 10
  p <- create_project(file.path(dir, "P"), settings = s, user = "tester")
  withr::defer(close_project(p), envir = env)
  add_runfiles(p, files)
}
