REGION_COLOURS <- c("#6b8acd", "#4cb28d", "#c78a40", "#c75d9c", "#9061c9",
                    "#72853c", "#cb554f", "#83b844")
SEAWATER_SR <- 0.70918

# Display options that are switched on and off in the browser without
# rebuilding the plot, and the name of the trace each one controls. The
# smoothed line and band are named after the smoothing method.
TOGGLE_TRACES <- c(points = "Data", outliers = "Outliers", seawater = "Seawater")

#' Interactive profile plot
#'
#' The 87Sr/86Sr profile of one sample against distance, with the smoothed
#' line and its uncertainty band, rejected outliers, regions and changepoint
#' means, plus strips for total Sr, 83Kr and 85Rb/88Sr below it. Drag along
#' the x axis to select a range.
#'
#' Point traces are drawn with WebGL and values are rounded to the precision
#' shown, which keeps redraws fast for long profiles.
#'
#' @param data Reduced and edited data for one sample (from [project_data()]).
#' @param regions Regions of the sample (`region`, `start_s`, `end_s`).
#' @param show Elements to show: any of `"points"`, `"outliers"`, `"smooth"`,
#'   `"band"`, `"seawater"`, `"strips"`. All traces except the strips are
#'   always included; those not in `show` start hidden (`"legendonly"`) so
#'   they can be switched on without rebuilding the plot.
#' @param source plotly event source ID.
#' @param lines 87Sr/86Sr values to mark with labelled horizontal lines
#'   (e.g. values of known habitats).
#' @return A built plotly object (see [plotly::plotly_build()]).
#' @export
profile_plot <- function(data, regions = NULL,
                         show = c("points", "outliers", "smooth", "band", "strips"),
                         source = "profile", lines = numeric()) {
  has_spline <- "sr87_sr86_spline" %in% names(data) && any(!is.na(data$sr87_sr86_spline))
  value_is_spline <- has_spline && isTRUE(all.equal(data$sr87_sr86_value, data$sr87_sr86_spline))
  value_is_raw <- isTRUE(all.equal(data$sr87_sr86_value, data$sr87_sr86))
  labels <- smooth_labels(value_is_spline)
  vis <- function(option) if (option %in% show) TRUE else "legendonly"

  # Only the columns that are drawn, rounded to the precision shown.
  d <- tibble::tibble(
    x = round(data$distance_um, 1),
    y = round(data$sr87_sr86, 6),
    outlier = round(data$sr87_sr86_outlier, 6),
    value = round(data$sr87_sr86_value, 6),
    low = round(if (value_is_spline) data$sr87_sr86_spline - 2 * data$sr87_sr86_spline_se
                else data$sr87_sr86_ma_ci95_low, 6),
    up = round(if (value_is_spline) data$sr87_sr86_spline + 2 * data$sr87_sr86_spline_se
               else data$sr87_sr86_ma_ci95_up, 6),
    cp = if ("cp_mean" %in% names(data)) round(data$cp_mean, 6) else NA_real_,
    total_sr = signif(data$total_sr, 4),
    kr83 = signif(data$raw83, 4),
    rb85_sr88 = signif(data$rb85_sr88, 4)
  )
  hover <- "%{x:.0f} \u00b5m<br>%{y:.5f}<extra></extra>"

  main <- plotly::plot_ly(source = source)
  if (!value_is_raw) {
    main <- plotly::add_ribbons(main, data = d, x = ~x, ymin = ~low, ymax = ~up,
                                name = labels[["band"]], visible = vis("band"),
                                line = list(width = 0), fillcolor = "rgba(31,111,139,0.18)",
                                hoverinfo = "skip")
  }
  pts <- dplyr::filter(d, !is.na(y))
  main <- plotly::add_trace(main, data = pts, x = ~x, y = ~y, type = "scattergl", mode = "markers",
                            name = TOGGLE_TRACES[["points"]], visible = vis("points"),
                            marker = list(color = "rgba(60,60,60,0.55)", size = 5),
                            hovertemplate = hover)
  out <- dplyr::filter(d, !is.na(outlier))
  if (nrow(out) > 0) {
    main <- plotly::add_trace(main, data = out, x = ~x, y = ~outlier, type = "scattergl",
                              mode = "markers", name = TOGGLE_TRACES[["outliers"]],
                              visible = vis("outliers"),
                              marker = list(color = "#d62728", size = 6, symbol = "circle-open"),
                              hovertemplate = paste0("Outlier<br>", hover))
  }
  if (!value_is_raw) {
    main <- plotly::add_lines(main, data = d, x = ~x, y = ~value, name = labels[["smooth"]],
                              visible = vis("smooth"), line = list(color = "#1f6f8b", width = 2),
                              hovertemplate = hover)
  }
  if (any(!is.na(d$cp))) {
    main <- plotly::add_lines(main, data = d, x = ~x, y = ~cp, name = "Changepoint means",
                              line = list(color = "#d62728", width = 2, shape = "hv"),
                              hovertemplate = "Segment mean %{y:.5f}<extra></extra>")
  }
  # Two points are enough for a horizontal line.
  main <- plotly::add_lines(main, x = range(d$x), y = rep(SEAWATER_SR, 2),
                            name = TOGGLE_TRACES[["seawater"]], visible = vis("seawater"),
                            line = list(color = "#2ca02c", dash = "dash", width = 1),
                            hoverinfo = "skip")
  # Custom lines, labelled with their value at the right end.
  for (v in lines) {
    main <- plotly::add_lines(main, x = range(d$x), y = rep(v, 2), name = sprintf("%.5f", v),
                              showlegend = FALSE, line = list(color = "#7b3fa0", dash = "dot", width = 1.5),
                              hovertemplate = sprintf("Line %.5f<extra></extra>", v))
  }
  line_labels <- purrr::map(lines, \(v) list(
    x = 1, xref = "paper", xanchor = "right", y = v, yref = "y", yanchor = "bottom",
    text = sprintf("%.5f", v), showarrow = FALSE, font = list(color = "#7b3fa0", size = 11)
  ))
  # Shapes go on the main panel before subplot(), which drops shapes added later.
  shapes <- region_shapes(data, regions)
  main <- plotly::layout(main, yaxis = list(title = sr_label(), tickformat = ".4f"),
                         shapes = shapes$shapes, annotations = c(shapes$annotations, line_labels))
  plots <- list(main)
  heights <- 1
  if ("strips" %in% show) {
    strip <- function(y, title, colour, fmt) {
      plotly::plot_ly(d, x = ~x, source = source) |>
        plotly::add_trace(y = y, type = "scattergl", mode = "markers",
                          marker = list(color = colour, size = 3), showlegend = FALSE,
                          hovertemplate = paste0("%{y:", fmt, "}<extra>", title, "</extra>")) |>
        plotly::layout(yaxis = list(title = title, exponentformat = "e"))
    }
    plots <- c(plots, list(
      strip(~total_sr, "Total Sr (V)", "#d62728", ".2f"),
      strip(~kr83, "<sup>83</sup>Kr (V)", "#2ca02c", ".5f"),
      strip(~rb85_sr88, "<sup>85</sup>Rb/<sup>88</sup>Sr", "#ff7f0e", ".5f")
    ))
    heights <- c(0.4, 0.2, 0.2, 0.2)
  }

  p <- plotly::subplot(plots, nrows = length(plots), shareX = TRUE, titleY = TRUE,
                       heights = heights, margin = 0.015)
  p <- plotly::layout(
    p,
    xaxis = list(title = "Distance (\u00b5m)"),
    dragmode = "select",
    selectdirection = "h",
    legend = list(orientation = "h", x = 0, y = 1.06, yanchor = "bottom"),
    margin = list(l = 70, r = 10, t = 30, b = 40),
    hovermode = "closest"
  )
  p <- plotly::config(p, displaylogo = FALSE, scrollZoom = TRUE,
                      modeBarButtonsToRemove = c("lasso2d", "autoScale2d", "toggleSpikelines"))
  p <- plotly::plotly_build(plotly::event_register(p, "plotly_brushed"))
  slim_plot(p)
}

# Reduce what is sent to the browser: drop the unevaluated plot attributes
# and send repeated per-point text (plotly recycles hover templates to every
# point) once per trace.
slim_plot <- function(p) {
  p$x$attrs <- NULL
  p$x$data <- lapply(p$x$data, \(trace) {
    # WebGL traces are drawn above SVG ones, so lines and the band must be
    # WebGL too to stay in trace order (line on top of the points).
    if (identical(trace$type, "scatter")) {
      trace$type <- "scattergl"
      trace$hoveron <- NULL  # set on ribbons, not a WebGL attribute
    }
    for (field in c("hovertemplate", "hoverinfo", "text")) {
      v <- trace[[field]]
      if (length(v) > 1 && length(unique(v)) == 1) trace[[field]] <- v[[1]]
    }
    trace
  })
  p
}

smooth_labels <- function(spline) {
  if (spline) c(smooth = "Spline", band = "\u00b12 SE") else c(smooth = "Moving average", band = "95% CI")
}

# Which traces of a built profile plot each display option controls, as
# zero-based trace indices for plotly.restyle.
toggle_trace_indices <- function(plot) {
  names <- vapply(plot$x$data, \(t) t$name %||% "", character(1))
  targets <- c(TOGGLE_TRACES,
               smooth = smooth_labels(FALSE)[["smooth"]], band = smooth_labels(FALSE)[["band"]],
               smooth = smooth_labels(TRUE)[["smooth"]], band = smooth_labels(TRUE)[["band"]])
  idx <- match(targets, names)
  keep <- !is.na(idx)
  stats::setNames(idx[keep] - 1L, names(targets)[keep])
}

# Regions are stored in analysis time; draw them over the distance range of
# the points they contain.
region_shapes <- function(data, regions) {
  if (is.null(regions) || nrow(regions) == 0) return(list(shapes = list(), annotations = list()))
  ranges <- regions |>
    dplyr::mutate(colour = REGION_COLOURS[(dplyr::row_number() - 1) %% length(REGION_COLOURS) + 1]) |>
    purrr::pmap(\(region, start_s, end_s, colour, ...) {
      d <- data$distance_um[data$time_s >= start_s & data$time_s <= end_s]
      if (length(d) == 0) return(NULL)
      list(region = region, x0 = min(d), x1 = max(d), colour = colour)
    }) |>
    purrr::compact()
  list(
    shapes = purrr::map(ranges, \(r) list(
      type = "rect", xref = "x", yref = "paper", x0 = r$x0, x1 = r$x1, y0 = 0, y1 = 1,
      fillcolor = r$colour, opacity = 0.18, line = list(width = 0), layer = "below"
    )),
    annotations = purrr::map(ranges, \(r) list(
      x = (r$x0 + r$x1) / 2, y = 1, xref = "x", yref = "paper", yanchor = "bottom",
      text = r$region, showarrow = FALSE, font = list(color = r$colour, size = 12)
    ))
  )
}
