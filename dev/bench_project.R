# Time the main operations on a realistically sized project: two analytical
# days, 60 fish runs of 3000 cycles, an SBO standard after every fifth fish
# and a secondary standard, with instrument drift.
#
# Run from the repository root:  Rscript dev/bench_project.R

suppressPackageStartupMessages(devtools::load_all(quiet = TRUE))
source("tests/testthat/helper-data.R")
source("tests/testthat/helper-runfiles.R")

time_it <- function(label, expr) {
  t <- system.time(value <- force(expr))[["elapsed"]]
  cat(sprintf("%-55s %7.2f s\n", label, t))
  invisible(value)
}

root <- tempfile("isofishr_bench_")
dir.create(file.path(root, "runs", "day1"), recursive = TRUE)
dir.create(file.path(root, "runs", "day2"))
set.seed(1)
n_cycles <- 3000
minute <- 0
for (day in 1:2) {
  start <- (day - 1) * 24 * 60
  for (i in 1:30) {
    k <- (day - 1) * 30 + i
    if (i %% 5 == 1) {
      write_runfile(file.path(root, "runs", paste0("day", day), sprintf("SBO_%02d.run", k)),
                    at_min(start + minute), ratio = 0.70918 * (1 + 2e-5 * minute) +
                      stats::rnorm(n_cycles, sd = 1.5e-4), n = n_cycles, blank_s = 30)
      minute <- minute + 12
    }
    if (i == 15) {
      write_runfile(file.path(root, "runs", paste0("day", day), sprintf("SEC_%d.run", day)),
                    at_min(start + minute), ratio = 0.7103 * (1 + 2e-5 * minute) +
                      stats::rnorm(n_cycles, sd = 1.5e-4), n = n_cycles, blank_s = 30)
      minute <- minute + 12
    }
    profile <- rep(c(0.7075, 0.7092), each = n_cycles / 2) * (1 + 2e-5 * minute) +
      stats::rnorm(n_cycles, sd = 3e-4)
    write_runfile(file.path(root, "runs", paste0("day", day), sprintf("Fish_%02d.run", k)),
                  at_min(start + minute), ratio = profile, n = n_cycles, blank_s = 30)
    minute <- minute + 12
  }
  minute <- 0
}

cat("\nProject:", length(list.files(file.path(root, "runs"), recursive = TRUE)), "runfiles of",
    n_cycles, "cycles\n\n")

s <- default_settings()
s$reduction$blank_time_s <- 30
p <- create_project(file.path(root, "Bench"), settings = s, user = "bench")
found <- time_it("Find runfiles in folder + subfolders", find_runfiles(file.path(root, "runs")))
p <- time_it("Add all runfiles (check, copy, timestamps)",
             suppressMessages(add_runfiles(p, found$path, source_names = found$file)))
p <- time_it("SBO suggestion (mark runs, set primary)", apply_reference_suggestion(p))
p <- set_reference_material(p, "SEC", 0.7103, 0.00004)
p <- set_run_type(p, c("SEC_1", "SEC_2"), "reference", "SEC")
cat(sprintf("%-55s %7s\n", "Sessions:", paste(unique(project_runs(p)$session), collapse = ", ")))

time_it("Reduce all runs, first time (no cache)", project_data(p, calibrate = FALSE))
time_it("Reduce all runs again (from cache)", project_data(p, calibrate = FALSE))
rm(list = ls(model_cache), envir = model_cache)
time_it("Calibration model, first time", calibration_model(p))
time_it("Calibration model again (cached)", calibration_model(p))
time_it("All runs, corrected (what exports use)", project_data(p))
time_it("Workspace: one sample, corrected", project_data(p, "Fish_17"))
time_it("Workspace: build the plot for one sample", profile_plot(project_data(p, "Fish_17")))
time_it("Calibration step: reference material results", reference_material_results(p))
time_it("Calibration step: summary table", summarise_reference_materials(p))
time_it("Calibration step: plot",
        reference_material_plot(calibration_model(p), reference_material_results(p)))
p <- time_it("Change the fit (spline), model rebuilt",
             update_project_settings(p, list(calibration = list(fit = "spline"))))
time_it("  ... all runs corrected with the new fit", project_data(p))
p <- update_project_settings(p, list(calibration = list(fit = "regression")))
p <- time_it("Edit one fish (comment)", update_sample(p, "Fish_17", comment = "check"))
export_dir <- time_it("Export, default files", export_project(p))
time_it("Export, all files incl. data.csv", export_project(p, files = EXPORT_FILES))

r <- summarise_reference_materials(p)
cat("\nReference materials (corrected should be close to accepted):\n")
print(as.data.frame(r[, c("reference_material", "role", "session", "runs", "mean_uncorrected",
                          "mean_corrected", "sd_corrected", "accepted", "deviation_ppm")]),
      digits = 7, row.names = FALSE)
if (identical(commandArgs(TRUE), "--report")) {
  cat("\nexport_report.md:\n")
  writeLines(readLines(file.path(export_dir, "export_report.md"), encoding = "UTF-8"))
}
close_project(p)
unlink(root, recursive = TRUE)
