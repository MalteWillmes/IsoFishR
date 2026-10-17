REGION_COLOURS <- c("#6b8acd", "#4cb28d", "#c78a40", "#c75d9c", "#9061c9",
                    "#72853c", "#cb554f", "#83b844")
SEAWATER_SR <- 0.70918

#' Interactive profile plot
#'
#' The 87Sr/86Sr profile of one sample against distance, with the smoothed
#' line and its uncertainty band, rejected outliers, regions and changepoint
#' means, plus strips for total Sr, 83Kr and 85Rb/88Sr below it. Drag along
#' the x axis to select a range.
#'
#' @param data Reduced and edited data for one sample (from [project_data()]).
#' @param regions Regions of the sample (`region`, `start_s`, `end_s`).
#' @param show Elements to draw: any of `"points"`, `"outliers"`, `"smooth"`,
#'   `"band"`, `"seawater"`, `"strips"`.
#' @param source plotly event source ID.
#' @return A plotly object.
#' @export
profile_plot <- function(data, regions = NULL,
                         show = c("points", "outliers", "smooth", "band", "strips"),
                         source = "profile") {
  has_spline <- "sr87_sr86_spline" %in% names(data) && any(!is.na(data$sr87_sr86_spline))
  value_is_spline <- has_spline && isTRUE(all.equal(data$sr87_sr86_value, data$sr87_sr86_spline))
  value_is_raw <- isTRUE(all.equal(data$sr87_sr86_value, data$sr87_sr86))
  band <- if (value_is_spline) {
    list(low = data$sr87_sr86_spline - 2 * data$sr87_sr86_spline_se,
         up = data$sr87_sr86_spline + 2 * data$sr87_sr86_spline_se, label = "\u00b12 SE")
  } else {
    list(low = data$sr87_sr86_ma_ci95_low, up = data$sr87_sr86_ma_ci95_up, label = "95% CI")
  }
  smooth_label <- if (value_is_spline) "Spline" else "Moving average"

  main <- plotly::plot_ly(data, x = ~distance_um, source = source)
  if ("band" %in% show && !value_is_raw) {
    main <- plotly::add_ribbons(main, ymin = band$low, ymax = band$up, name = band$label,
                                line = list(width = 0), fillcolor = "rgba(31,111,139,0.18)",
                                hoverinfo = "skip")
  }
  # Always added (hidden if unticked) so the panel keeps its axes.
  main <- plotly::add_markers(main, y = ~sr87_sr86, name = "Data",
                              visible = if ("points" %in% show) TRUE else "legendonly",
                              marker = list(color = "rgba(60,60,60,0.55)", size = 5),
                              hovertemplate = "%{x:.0f} \u00b5m<br>%{y:.5f}<extra></extra>")
  if ("outliers" %in% show && any(!is.na(data$sr87_sr86_outlier))) {
    main <- plotly::add_markers(main, y = ~sr87_sr86_outlier, name = "Outliers",
                                marker = list(color = "#d62728", size = 6, symbol = "circle-open"),
                                hovertemplate = "Outlier<br>%{x:.0f} \u00b5m<br>%{y:.5f}<extra></extra>")
  }
  if ("smooth" %in% show && !value_is_raw) {
    main <- plotly::add_lines(main, y = ~sr87_sr86_value, name = smooth_label,
                              line = list(color = "#1f6f8b", width = 2),
                              hovertemplate = "%{x:.0f} \u00b5m<br>%{y:.5f}<extra></extra>")
  }
  if ("cp_mean" %in% names(data) && any(!is.na(data$cp_mean))) {
    main <- plotly::add_lines(main, y = ~cp_mean, name = "Changepoint means",
                              line = list(color = "#d62728", width = 2, shape = "hv"),
                              hovertemplate = "Segment mean %{y:.5f}<extra></extra>")
  }
  if ("seawater" %in% show) {
    main <- plotly::add_lines(main, y = SEAWATER_SR, name = "Seawater",
                              line = list(color = "#2ca02c", dash = "dash", width = 1),
                              hoverinfo = "skip")
  }
  # Shapes go on the main panel before subplot(), which drops shapes added later.
  shapes <- region_shapes(data, regions)
  main <- plotly::layout(main, yaxis = list(title = sr_label(), tickformat = ".4f"),
                         shapes = shapes$shapes, annotations = shapes$annotations)
  plots <- list(main)
  heights <- 1
  if ("strips" %in% show) {
    strip <- function(y, title, colour, fmt) {
      plotly::plot_ly(data, x = ~distance_um, source = source) |>
        plotly::add_markers(y = y, marker = list(color = colour, size = 3), showlegend = FALSE,
                            hovertemplate = paste0("%{y:", fmt, "}<extra>", title, "</extra>")) |>
        plotly::layout(yaxis = list(title = title, exponentformat = "e"))
    }
    plots <- c(plots, list(
      strip(~total_sr, "Total Sr (V)", "#d62728", ".2f"),
      strip(~raw83, "<sup>83</sup>Kr (V)", "#2ca02c", ".5f"),
      strip(~rb85_sr88, "<sup>85</sup>Rb/<sup>88</sup>Sr", "#ff7f0e", ".5f")
    ))
    heights <- c(0.58, 0.14, 0.14, 0.14)
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
  p <- plotly::config(p, displaylogo = FALSE,
                      modeBarButtonsToRemove = c("lasso2d", "autoScale2d", "toggleSpikelines"))
  plotly::event_register(p, "plotly_brushed")
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
