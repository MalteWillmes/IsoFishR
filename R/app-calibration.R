# Calibration page: reference materials, run types and sessions, the primary
# reference material correction and the secondary reference material checks.

calibration_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    border = FALSE,
    sidebar = bslib::sidebar(
      width = 340,
      title = "Calibration",
      shiny::h6("Reference materials"),
      shiny::tableOutput(ns("rm_table")),
      bslib::layout_columns(
        col_widths = c(6, 6),
        shiny::textInput(ns("rm_name"), "Name", placeholder = "e.g. MMC"),
        shiny::numericInput(ns("rm_value"), "Accepted 87Sr/86Sr", NA, step = 0.00001)
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        shiny::numericInput(ns("rm_unc"), "\u00b1 (2\u03c3)", NA, step = 0.000001),
        shiny::textInput(ns("rm_note"), "Note / source")
      ),
      shiny::div(
        class = "d-flex gap-2",
        shiny::actionButton(ns("rm_save"), "Save reference material", class = "btn-sm"),
        shiny::actionButton(ns("rm_remove"), "Remove", class = "btn-sm btn-outline-danger")
      ),
      shiny::hr(),
      shiny::selectInput(ns("primary"), "Primary reference material (corrects samples)", NULL),
      shiny::radioButtons(ns("fit"), "Correction through time", c(
        "Linear between bracketing standards" = "linear",
        "Smoothing spline" = "spline",
        "Step forward (most recent standard)" = "step",
        "Session mean" = "mean"
      )),
      shiny::radioButtons(ns("baseline"), "Baseline", c(
        "Each run's own blank (start of run)" = "per_run",
        "Baseline runs, interpolated in time" = "baseline_runs"
      )),
      shiny::numericInput(ns("session_gap"), "New session after a gap of (hours)", 6, min = 0.1, step = 0.5),
      shiny::p(class = "ifr-muted small",
               "Other reference materials are secondary: they are corrected like samples and ",
               "reported as accuracy checks.")
    ),
    bslib::navset_card_underline(
      bslib::nav_panel(
        "Runs",
        shiny::div(
          class = "d-flex flex-wrap gap-2 align-items-end mb-2",
          shiny::div(shiny::selectInput(ns("assign_type"), "Set selected runs to", NULL, width = "220px")),
          shiny::div(class = "mb-3", shiny::actionButton(ns("assign"), "Apply", class = "btn-sm btn-primary")),
          shiny::div(class = "ms-3", shiny::textInput(ns("session_name"), "Session", width = "180px",
                                                    placeholder = "e.g. Day 2 PM")),
          shiny::div(class = "mb-3", shiny::actionButton(ns("session_set"), "Move selected", class = "btn-sm")),
          shiny::div(class = "mb-3", shiny::actionButton(ns("session_auto"), "Automatic session", class = "btn-sm"))
        ),
        shiny::p(class = "ifr-muted small",
                 "Runs in acquisition order (from the runfile timestamps). Click rows to select or ",
                 "deselect them; the selection is cleared after each change. Use the filter boxes ",
                 "to narrow the list, e.g. type a name pattern under Run."),
        DT::DTOutput(ns("runs"))
      ),
      bslib::nav_panel(
        "Correction",
        shiny::uiOutput(ns("correction_note")),
        plotly::plotlyOutput(ns("correction_plot"), height = "520px")
      ),
      bslib::nav_panel(
        "Reference material results",
        shiny::h6("Summary per reference material and session"),
        DT::DTOutput(ns("rm_summary")),
        shiny::h6(class = "mt-3", "Every reference material run"),
        DT::DTOutput(ns("rm_runs"))
      )
    )
  )
}

calibration_server <- function(id, project) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    change <- function(fun, ...) {
      p <- try_notify(fun(project(), ...))
      if (!is.null(p)) project(p)
      invisible(p)
    }

    # ---- reference materials ----
    output$rm_table <- shiny::renderTable({
      rms <- shiny::req(project())$reference_materials
      if (nrow(rms) == 0) return(data.frame(Note = "None defined yet"))
      data.frame(Name = rms$name, Value = sprintf("%.6f", rms$value),
                 Unc = ifelse(is.na(rms$uncertainty_2s), "",
                              format(rms$uncertainty_2s, digits = 3))) |>
        stats::setNames(c("Name", "Value", "\u00b1 2s"))
    }, spacing = "xs", width = "100%")

    shiny::observeEvent(input$rm_save, {
      shiny::req(nzchar(trimws(input$rm_name)))
      change(set_reference_material, input$rm_name, input$rm_value,
             if (is.null(input$rm_unc)) NA_real_ else input$rm_unc, input$rm_note %||% "")
    })
    shiny::observeEvent(input$rm_remove, {
      shiny::req(nzchar(trimws(input$rm_name)))
      change(remove_reference_material, trimws(input$rm_name))
    })

    # Keep the choice lists and settings inputs in step with the project.
    shiny::observe({
      p <- shiny::req(project())
      names <- p$reference_materials$name
      cal <- p$settings$calibration
      shiny::updateSelectInput(session, "primary",
                               choices = c("None (no correction)" = "", stats::setNames(names, names)),
                               selected = cal$primary)
      shiny::updateSelectInput(session, "assign_type",
                               choices = c("Sample" = "sample", "Baseline" = "baseline",
                                           stats::setNames(paste0("rm:", names),
                                                           paste("Reference material:", names))))
      shiny::updateRadioButtons(session, "fit", selected = cal$fit)
      shiny::updateRadioButtons(session, "baseline", selected = cal$baseline)
      shiny::updateNumericInput(session, "session_gap", value = cal$session_gap_h)
    })

    save_setting <- function(key, value) {
      if (is.null(value) || (is.numeric(value) && is.na(value))) return()
      if (identical(project()$settings$calibration[[key]], value)) return()
      change(update_project_settings, list(calibration = rlang::list2(!!key := value)))
    }
    shiny::observeEvent(input$primary, save_setting("primary", input$primary), ignoreInit = TRUE)
    shiny::observeEvent(input$fit, save_setting("fit", input$fit), ignoreInit = TRUE)
    shiny::observeEvent(input$baseline, save_setting("baseline", input$baseline), ignoreInit = TRUE)
    shiny::observeEvent(input$session_gap, save_setting("session_gap_h", input$session_gap),
                        ignoreInit = TRUE)

    # ---- runs ----
    runs <- shiny::reactive(project_runs(shiny::req(project())))

    runs_table <- shiny::reactive({
      r <- runs()
      tibble::tibble(
        `#` = r$sequence,
        Acquired = ifelse(is.na(r$acquired), "", format(r$acquired, "%Y-%m-%d %H:%M", tz = "UTC")),
        Session = r$session,
        Run = r$sample_id,
        Type = dplyr::case_when(
          r$run_type == "baseline" ~ "Baseline",
          r$run_type == "reference" ~ paste0(r$reference_material, " (", r$rm_role, ")"),
          .default = "Sample"
        ),
        Status = unname(STATUS_LABELS[r$status])
      )
    })

    output$runs <- DT::renderDT({
      DT::datatable(shiny::isolate(runs_table()), rownames = FALSE, filter = "top",
                    selection = list(mode = "multiple", target = "row"),
                    options = list(pageLength = 50, dom = "tip", ordering = FALSE))
    })
    runs_proxy <- DT::dataTableProxy("runs")
    shiny::observeEvent(runs_table(), {
      DT::replaceData(runs_proxy, runs_table(), rownames = FALSE, resetPaging = FALSE,
                      clearSelection = "none")
    }, ignoreInit = TRUE)

    selected_runs <- function() {
      ids <- runs()$sample_id[input$runs_rows_selected]
      if (length(ids) == 0) shiny::showNotification("Select runs in the table first.", type = "warning")
      ids
    }

    # Clicking a row toggles it, so a selection left over from the previous
    # change would silently be included in the next one: clear it.
    clear_selection <- function() DT::selectRows(runs_proxy, NULL)

    shiny::observeEvent(input$assign, {
      ids <- selected_runs()
      shiny::req(length(ids) > 0, input$assign_type)
      type <- input$assign_type
      if (startsWith(type, "rm:")) {
        change(set_run_type, ids, "reference", sub("^rm:", "", type))
      } else {
        change(set_run_type, ids, type)
      }
      clear_selection()
    })
    shiny::observeEvent(input$session_set, {
      ids <- selected_runs()
      shiny::req(length(ids) > 0, nzchar(trimws(input$session_name)))
      change(set_session, ids, trimws(input$session_name))
      clear_selection()
    })
    shiny::observeEvent(input$session_auto, {
      ids <- selected_runs()
      shiny::req(length(ids) > 0)
      change(set_session, ids, "")
      clear_selection()
    })

    # ---- correction ----
    model <- shiny::reactive(calibration_model(shiny::req(project())))

    output$correction_note <- shiny::renderUI({
      p <- project()
      m <- model()
      msg <- if (is.null(m)) {
        "Choose a primary reference material to correct samples. Until then values are uncorrected."
      } else if (nrow(m$points) == 0) {
        paste0("No usable runs of ", m$primary, " yet: mark its runs in the Runs tab.")
      } else {
        sessions <- unique(runs()$session[runs()$run_type != "baseline"])
        missing <- setdiff(sessions, m$points$session)
        paste0(nrow(m$points), " run(s) of ", m$primary, " correct the samples. ",
               if (length(missing)) paste0("Not corrected (no ", m$primary, " in session): ",
                                           paste(missing, collapse = ", "), "."))
      }
      shiny::p(class = "ifr-muted", msg)
    })

    output$correction_plot <- plotly::renderPlotly({
      m <- model()
      shiny::validate(shiny::need(!is.null(m) && nrow(m$points) > 0,
                                  "No primary reference material runs to show yet."))
      correction_plot(m, reference_material_results(project()))
    })

    # ---- results ----
    output$rm_summary <- DT::renderDT({
      s <- summarise_reference_materials(shiny::req(project()))
      shiny::validate(shiny::need(nrow(s) > 0, "No reference material runs yet."))
      DT::datatable(round_summary(s), rownames = FALSE, options = list(dom = "t", paging = FALSE))
    })
    output$rm_runs <- DT::renderDT({
      r <- reference_material_results(shiny::req(project()))
      shiny::validate(shiny::need(nrow(r) > 0, "No reference material runs yet."))
      r$acquired <- format_time(r$acquired)
      DT::datatable(round_summary(r), rownames = FALSE, filter = "top",
                    options = list(pageLength = 25, dom = "tip", scrollX = TRUE))
    })
  })
}

#' Reference material correction plot
#'
#' Top: the correction factor (accepted / measured) of each primary
#' reference material run against acquisition time, with its 2 SE and the
#' fitted correction within each session. Bottom: the deviation of the
#' corrected secondary reference material runs from their accepted values
#' (ppm), with 2 SE including the correction uncertainty.
#'
#' @param model A model from [calibration_model()].
#' @param results Output of [reference_material_results()].
#' @return A plotly object.
#' @export
correction_plot <- function(model, results) {
  pts <- model$points
  pts$when <- as.POSIXct(pts$time, origin = "1970-01-01", tz = "UTC")
  curves <- purrr::map(unique(pts$session), \(s) {
    tt <- pts$time[pts$session == s]
    grid <- seq(min(tt) - 300, max(tt) + 300, length.out = 200)
    tibble::tibble(session = s, when = as.POSIXct(grid, origin = "1970-01-01", tz = "UTC"),
                   factor = correction_at(model, s, grid)$factor)
  }) |> purrr::list_rbind()

  top <- plotly::plot_ly() |>
    plotly::add_lines(data = curves, x = ~when, y = ~factor, split = ~session,
                      line = list(color = "#1f6f8b"), showlegend = FALSE, hoverinfo = "skip") |>
    plotly::add_markers(data = pts, x = ~when, y = ~factor, name = model$primary,
                        error_y = list(type = "data", array = 2 * pts$factor * pts$u_rel),
                        marker = list(color = "#1f6f8b", size = 7),
                        text = ~sample_id,
                        hovertemplate = "%{text}<br>%{x}<br>factor %{y:.6f}<extra></extra>") |>
    plotly::layout(yaxis = list(title = paste0("Correction factor<br>(", model$primary, ")"),
                                tickformat = ".5f"))

  sec <- dplyr::filter(results, .data$role == "secondary")
  plots <- list(top)
  if (nrow(sec) > 0) {
    sec$err_ppm <- 2 * sec$se_combined / sec$accepted * 1e6
    bottom <- plotly::plot_ly() |>
      plotly::add_markers(data = sec, x = ~acquired, y = ~deviation_ppm, color = ~reference_material,
                          error_y = list(type = "data", array = sec$err_ppm),
                          text = ~sample_id,
                          hovertemplate = "%{text}<br>%{y:.1f} ppm<extra></extra>") |>
      plotly::layout(yaxis = list(title = "Secondary RM<br>deviation (ppm)", zeroline = TRUE))
    plots <- c(plots, list(bottom))
  }
  plotly::subplot(plots, nrows = length(plots), shareX = TRUE, titleY = TRUE, margin = 0.04) |>
    plotly::layout(xaxis = list(title = "Acquisition time"), hovermode = "closest",
                   legend = list(orientation = "h", y = 1.08)) |>
    plotly::config(displaylogo = FALSE)
}
