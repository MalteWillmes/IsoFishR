# Step 4: reference materials, run types and sessions, the primary
# reference material correction and the secondary reference material checks.

FIT_CHOICES <- c(
  "Linear regression through the standards (drift)" = "regression",
  "Advanced: linear between bracketing standards" = "linear",
  "Advanced: smoothing spline" = "spline",
  "Advanced: step forward (most recent standard)" = "step",
  "Advanced: session mean" = "mean"
)

calibration_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "ifr-step",
    shiny::uiOutput(ns("suggestion")),
    bslib::layout_sidebar(
      border = FALSE,
      sidebar = bslib::sidebar(
        open = "always",  # set up while its step is hidden; would otherwise start collapsed
        width = 340,
        title = "Reference materials",
        shiny::tableOutput(ns("rm_table")),
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::textInput(ns("rm_name"), "Name", placeholder = "e.g. SBO"),
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
        shiny::selectInput(ns("fit"), "Correction through time", FIT_CHOICES),
        shiny::radioButtons(ns("baseline"), "Baseline", c(
          "Each run's own blank (start of run)" = "per_run",
          "Baseline runs, interpolated in time" = "baseline_runs"
        )),
        shiny::numericInput(ns("session_gap"), "New session after a gap of (hours)", 6,
                            min = 0.1, step = 0.5),
        shiny::p(class = "ifr-muted small",
                 "Other reference materials are secondary: corrected like samples and reported ",
                 "per session as accuracy checks.")
      ),
      bslib::navset_card_underline(
        bslib::nav_panel(
          "Assign runs",
          shiny::div(
            class = "d-flex flex-wrap gap-2 align-items-end mb-2",
            shiny::div(shiny::selectInput(ns("assign_type"), "Set selected runs to", NULL, width = "240px")),
            shiny::div(class = "mb-2", shiny::actionButton(ns("assign"), "Apply", class = "btn-sm btn-primary")),
            shiny::div(class = "ms-3", shiny::textInput(ns("session_name"), "Session", width = "180px",
                                                      placeholder = "e.g. Day 2 PM")),
            shiny::div(class = "mb-2", shiny::actionButton(ns("session_set"), "Move selected", class = "btn-sm")),
            shiny::div(class = "mb-2", shiny::actionButton(ns("session_auto"), "Automatic session", class = "btn-sm"))
          ),
          shiny::p(class = "ifr-muted small",
                   "Runs in acquisition order. Click rows to select or deselect them; the selection ",
                   "is cleared after each change. Type in the filter boxes to narrow the list, e.g. ",
                   "a name pattern under Run."),
          DT::DTOutput(ns("runs"))
        ),
        bslib::nav_panel(
          "Reference materials",
          shiny::uiOutput(ns("correction_note")),
          plotly::plotlyOutput(ns("rm_plot"), height = "560px"),
          shiny::h6(class = "mt-2", "Mean and SD of the run means, per session"),
          DT::DTOutput(ns("rm_summary"))
        ),
        bslib::nav_panel(
          "Run details",
          shiny::p(class = "ifr-muted small",
                   "Every reference material run: measured (uncorrected) and corrected mean, and ",
                   "deviation from the accepted value."),
          DT::DTOutput(ns("rm_runs"))
        )
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

    # ---- SBO suggestion ----
    output$suggestion <- shiny::renderUI({
      p <- shiny::req(project())
      ids <- suggest_reference_runs(p)
      if (length(ids) == 0) return(NULL)
      rms <- p$reference_materials
      value <- if ("SBO" %in% rms$name) rms$value[rms$name == "SBO"] else SBO_DEFAULT
      shiny::div(
        class = "alert alert-info d-flex flex-wrap align-items-end gap-3",
        shiny::div(
          class = "flex-grow-1",
          shiny::strong(sprintf("%d run(s) have SBO in their name", length(ids))),
          sprintf(" (%s). Mark them as the reference material SBO?",
                  paste(utils::head(ids, 6), collapse = ", ")),
          if (length(ids) > 6) "\u2026",
          if (!nzchar(p$settings$calibration$primary)) " It becomes the primary reference material."
        ),
        shiny::div(shiny::numericInput(ns("sbo_value"), "SBO accepted 87Sr/86Sr", value,
                                       step = 0.00001, width = "180px")),
        shiny::div(class = "mb-2", shiny::actionButton(ns("sbo_apply"), "Mark as SBO", class = "btn-primary"))
      )
    })

    shiny::observeEvent(input$sbo_apply, {
      v <- input$sbo_value
      if (is.null(v) || is.na(v) || v <= 0) {
        shiny::showNotification("Enter the accepted 87Sr/86Sr of SBO.", type = "warning")
        return()
      }
      change(\(p) {
        p <- apply_reference_suggestion(p, value = v)
        rms <- p$reference_materials
        if (!isTRUE(all.equal(rms$value[rms$name == "SBO"], v))) {
          p <- set_reference_material(p, "SBO", v, rms$uncertainty_2s[rms$name == "SBO"],
                                      rms$note[rms$name == "SBO"])
        }
        p
      })
    })

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
      shiny::updateSelectInput(session, "fit", selected = cal$fit)
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

    # ---- assign runs ----
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

    # ---- correction and results ----
    model <- shiny::reactive(calibration_model(shiny::req(project())))
    results <- shiny::reactive(reference_material_results(shiny::req(project())))

    output$correction_note <- shiny::renderUI({
      m <- model()
      msg <- if (is.null(m)) {
        "Choose a primary reference material to correct samples. Until then all values are uncorrected."
      } else if (nrow(m$points) == 0) {
        paste0("No usable runs of ", m$primary, " yet: mark its runs under Assign runs.")
      } else {
        sessions <- unique(runs()$session[runs()$run_type != "baseline"])
        missing <- setdiff(sessions, m$points$session)
        paste0(nrow(m$points), " run(s) of ", m$primary, " correct the samples (",
               names(FIT_CHOICES)[FIT_CHOICES == m$fit], "). ",
               if (length(missing)) paste0("Not corrected (no ", m$primary, " in session): ",
                                           paste(missing, collapse = ", "), "."))
      }
      shiny::p(class = "ifr-muted", msg)
    })

    output$rm_plot <- plotly::renderPlotly({
      m <- model()
      r <- results()
      shiny::validate(shiny::need(nrow(r) > 0,
                                  "Mark reference material runs (Assign runs, or the SBO suggestion above)."))
      reference_material_plot(m, r)
    })

    output$rm_summary <- DT::renderDT({
      s <- summarise_reference_materials(shiny::req(project()))
      shiny::validate(shiny::need(nrow(s) > 0, "No reference material runs yet."))
      s <- dplyr::select(s, "reference_material", "role", "session", "runs", "mean_uncorrected",
                         "sd_uncorrected", "mean_corrected", "sd_corrected", "accepted",
                         "deviation_ppm")
      DT::datatable(round_summary(s), rownames = FALSE,
                    colnames = c("Reference material", "Role", "Session", "Runs", "Mean (uncorrected)",
                                 "SD (uncorrected)", "Mean (corrected)", "SD (corrected)",
                                 "Accepted", "Deviation (ppm)"),
                    options = list(dom = "t", paging = FALSE))
    })
    output$rm_runs <- DT::renderDT({
      r <- results()
      shiny::validate(shiny::need(nrow(r) > 0, "No reference material runs yet."))
      r$acquired <- format_time(r$acquired)
      DT::datatable(round_summary(r), rownames = FALSE, filter = "top",
                    options = list(pageLength = 25, dom = "tip", scrollX = TRUE))
    })
  })
}

#' Reference material plot
#'
#' One panel per reference material, against acquisition time. For the
#' primary reference material: each run's measured (uncorrected) mean with
#' 2 SE, the fitted correction line (by default the linear regression) and
#' the corrected run means. For secondary reference materials: uncorrected
#' and corrected run means, with the corrected session averages as
#' horizontal lines. The accepted values are dashed lines.
#'
#' @param model A model from [calibration_model()] (`NULL` if no primary
#'   reference material is set).
#' @param results Output of [reference_material_results()].
#' @return A plotly object.
#' @export
reference_material_plot <- function(model, results) {
  hover <- "%{text}<br>%{x}<br>%{y:.6f}<extra></extra>"
  accepted_line <- function(p, d, value) {
    plotly::add_lines(p, x = range(d$acquired), y = rep(value, 2), name = "Accepted",
                      line = list(color = "#555", dash = "dash", width = 1), showlegend = FALSE,
                      hoverinfo = "skip")
  }
  panels <- purrr::map(unique(results$reference_material), \(rm_name) {
    d <- dplyr::filter(results, .data$reference_material == rm_name)
    p <- plotly::plot_ly() |>
      accepted_line(d, d$accepted[1]) |>
      plotly::add_markers(data = d, x = ~acquired, y = ~measured, name = "Uncorrected",
                          legendgroup = "uncorrected", showlegend = rm_name == results$reference_material[1],
                          error_y = list(type = "data", array = 2 * d$se, color = "#999"),
                          marker = list(color = "#999", size = 7, symbol = "circle-open"),
                          text = ~sample_id, hovertemplate = hover)
    if (d$role[1] == "primary" && !is.null(model) && nrow(model$points) > 0) {
      fitted <- purrr::map(unique(model$points$session), \(s) {
        tt <- model$points$time[model$points$session == s]
        grid <- seq(min(tt) - 300, max(tt) + 300, length.out = 100)
        tibble::tibble(session = s, acquired = as.POSIXct(grid, origin = "1970-01-01", tz = "UTC"),
                       measured = model$accepted / correction_at(model, s, grid)$factor)
      }) |> purrr::list_rbind()
      p <- plotly::add_lines(p, data = fitted, x = ~acquired, y = ~measured, split = ~session,
                             name = "Fit (uncorrected)", legendgroup = "fit", showlegend = FALSE,
                             line = list(color = "#999", width = 2), hoverinfo = "skip")
    }
    if (d$role[1] == "secondary") {
      avg <- d |>
        dplyr::summarise(start = min(.data$acquired), end = max(.data$acquired),
                         mean = mean(.data$mean), .by = "session")
      for (i in seq_len(nrow(avg))) {
        p <- plotly::add_lines(p, x = c(avg$start[i], avg$end[i]), y = rep(avg$mean[i], 2),
                               name = "Session average (corrected)", legendgroup = "avg",
                               showlegend = FALSE, line = list(color = "#1f6f8b", width = 2),
                               hovertemplate = paste0(avg$session[i], " average %{y:.6f}<extra></extra>"))
      }
    }
    p |>
      plotly::add_markers(data = d, x = ~acquired, y = ~mean, name = "Corrected",
                          legendgroup = "corrected", showlegend = rm_name == results$reference_material[1],
                          error_y = list(type = "data", array = 2 * d$se_combined, color = "#1f6f8b"),
                          marker = list(color = "#1f6f8b", size = 7),
                          text = ~sample_id, hovertemplate = hover) |>
      plotly::layout(yaxis = list(title = paste0(rm_name, "<br>(", d$role[1], ")"),
                                  tickformat = ".5f"))
  })
  plotly::subplot(panels, nrows = length(panels), shareX = TRUE, titleY = TRUE, margin = 0.04) |>
    plotly::layout(xaxis = list(title = "Acquisition time"), hovermode = "closest",
                   legend = list(orientation = "h", y = 1.06)) |>
    plotly::config(displaylogo = FALSE)
}
