# Measure the cost of the profile plot: time to build it in R, and the size
# of the data sent to the browser for each redraw.
#
# Run from the repository root:  Rscript dev/bench_plot.R

suppressPackageStartupMessages(devtools::load_all(quiet = TRUE))

p <- create_example_project(user = "bench")
on.exit(close_project(p))
d <- project_data(p, "PC_2016_01_A2_4R")

build <- function() plotly::plotly_build(profile_plot(d))
invisible(build())  # warm up
times <- vapply(1:10, \(i) system.time(build())[["elapsed"]], numeric(1))

b <- build()
json <- htmlwidgets:::toJSON(b$x)
types <- vapply(b$x$data, \(t) t$type %||% "", "")

cat(sprintf("Points in sample:      %d\n", nrow(d)))
cat(sprintf("Build time (median):   %.0f ms\n", 1000 * stats::median(times)))
cat(sprintf("Data per redraw:       %.0f KB\n", nchar(json, type = "bytes") / 1024))
cat(sprintf("Traces:                %d (%s)\n", length(types), paste(table(types), names(table(types)), collapse = ", ")))
