# The workspace: sample list on the left, profile plot in the middle and the
# Reduction / Edit / Notes panels on the right. Every change is saved through
# the project functions as soon as it is made.

workspace_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    fillable = TRUE,
    border = FALSE,
    sidebar = bslib::sidebar(
      open = "always",  # set up while its step is hidden; would otherwise start collapsed
      width = 330,
      title = "Samples",
      shiny::div(class = "ifr-sample-table", DT::DTOutput(ns("samples"))),
      shiny::div(
        class = "d-flex gap-2",
        shiny::actionButton(ns("prev"), "\u2190 Previous", class = "btn-sm flex-fill"),
        shiny::actionButton(ns("next_sample"), "Next \u2192", class = "btn-sm flex-fill")
      )
    ),
    bslib::layout_columns(
      col_widths = c(8, 4),
      fill = TRUE,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "d-flex align-items-center gap-3 flex-wrap",
          shiny::textOutput(ns("sample_title"), inline = TRUE),
          shiny::uiOutput(ns("calibration_badge"), inline = TRUE),
          shiny::div(
            class = "ms-auto",
            shiny::checkboxGroupInput(
              ns("show"), NULL, inline = TRUE,
              choices = c("Points" = "points", "Outliers" = "outliers", "Smoothed" = "smooth",
                          "Uncertainty" = "band", "Seawater" = "seawater", "Sr, Kr and Rb panels" = "strips"),
              selected = c("points", "outliers", "smooth", "band", "strips")
            )
          )
        ),
        bslib::card_body(
          padding = 4,
          shiny::uiOutput(ns("reduction_warnings")),
          plotly::plotlyOutput(ns("plot"), height = "100%")
        ),
        bslib::card_footer(shiny::textOutput(ns("selection_text")))
      ),
      bslib::navset_card_tab(
        id = ns("panel"),
        bslib::nav_panel("Edit", shiny::div(class = "ifr-panel", edit_panel_ui(ns))),
        bslib::nav_panel("Reduction", shiny::div(class = "ifr-panel", reduction_panel_ui(ns))),
        bslib::nav_panel("Notes", shiny::div(class = "ifr-panel", notes_panel_ui(ns)))
      )
    )
  )
}

edit_panel_ui <- function(ns) {
  shiny::tagList(
    shiny::h6("Trim"),
    shiny::p(class = "ifr-muted", "Drag across the plot to select a range."),
    shiny::actionButton(ns("trim_to_selection"), "Keep only selected range", class = "btn-sm"),
    shiny::actionButton(ns("trim_clear"), "Remove trims", class = "btn-sm"),
    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::numericInput(ns("trim_start_um"), "Trim start (\u00b5m)", 0, min = 0, updateOn = "blur"),
      shiny::numericInput(ns("trim_end_um"), "Trim end (\u00b5m)", 0, min = 0, updateOn = "blur")
    ),
    shiny::checkboxInput(ns("rezero"), "Distance starts at 0 after trimming", TRUE),
    shiny::checkboxInput(ns("reverse"), "Reverse profile direction", FALSE),
    shiny::hr(),
    shiny::h6("Regions"),
    shiny::div(
      class = "d-flex gap-2 align-items-end",
      shiny::div(class = "flex-grow-1", shiny::textInput(ns("region_name"), "Name", width = "100%")),
      shiny::div(class = "mb-2", shiny::actionButton(ns("region_add"), "Add from selection", class = "btn-sm"))
    ),
    shiny::tableOutput(ns("regions")),
    shiny::div(
      class = "d-flex gap-2 align-items-end",
      shiny::div(class = "flex-grow-1", shiny::selectInput(ns("region_remove_name"), "Remove region", NULL, width = "100%")),
      shiny::div(class = "mb-2", shiny::actionButton(ns("region_remove"), "Remove", class = "btn-sm"))
    ),
    shiny::hr(),
    shiny::h6("Changepoints"),
    shiny::checkboxInput(ns("cp_enabled"), "Detect changepoints", FALSE),
    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::selectInput(ns("cp_method"), "Method", c("PELT", "AMOC", "SegNeigh", "BinSeg")),
      shiny::numericInput(ns("cp_penalty"), "Penalty", 5e-5, min = 0, step = 1e-5, updateOn = "blur")
    ),
    shiny::textOutput(ns("cp_summary"))
  )
}

reduction_panel_ui <- function(ns) {
  shiny::tagList(
    shiny::p(class = "ifr-muted",
             "Settings used for this sample. Changes apply to this sample only; the other ",
             "settings and the project-wide defaults are under Settings."),
    shiny::uiOutput(ns("override_summary")),
    shiny::uiOutput(ns("reduction_inputs")),
    shiny::div(
      class = "d-flex gap-2",
      shiny::actionButton(ns("reduction_apply"), "Apply to this sample", class = "btn-primary btn-sm"),
      shiny::actionButton(ns("reduction_reset"), "Use project settings", class = "btn-sm")
    )
  )
}

notes_panel_ui <- function(ns) {
  shiny::tagList(
    shiny::radioButtons(ns("status"), "Status", stats::setNames(names(STATUS_LABELS), STATUS_LABELS),
                        inline = TRUE),
    shiny::checkboxInput(ns("flag"), "Flag for review", FALSE),
    shiny::textAreaInput(ns("comment"), "Comment", rows = 4, width = "100%", updateOn = "blur"),
    shiny::actionButton(ns("reviewed_next"), "Mark reviewed and go to next", class = "btn-primary btn-sm"),
    shiny::hr(),
    shiny::h6("Sample details"),
    shiny::tableOutput(ns("sample_info")),
    shiny::h6("History"),
    shiny::tableOutput(ns("sample_log")),
    shiny::hr(),
    shiny::actionButton(ns("remove"), "Remove sample from project\u2026",
                        class = "btn-outline-danger btn-sm")
  )
}

workspace_server <- function(id, project) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    current <- shiny::reactiveVal(NULL)
    selection <- shiny::reactiveVal(NULL)  # selected distance range

    # Apply a project change and keep the new project object.
    change <- function(fun, ...) {
      p <- try_notify(fun(project(), ...))
      if (!is.null(p)) project(p)
      invisible(p)
    }

    sample_row <- shiny::reactive({
      p <- project()
      id <- current()
      shiny::req(p, id, id %in% p$samples$sample_id)
      p$samples[p$samples$sample_id == id, ]
    })

    # ---- sample table ----
    table_data <- shiny::reactive({
      s <- shiny::req(project())$samples
      tibble::tibble(
        Sample = s$sample_id,
        Type = dplyr::case_when(s$run_type == "baseline" ~ "Baseline",
                                s$run_type == "reference" ~ s$reference_material,
                                .default = ""),
        Status = unname(STATUS_LABELS[s$status]),
        Flag = ifelse(s$flag, "\u2691", ""),
        Comment = substr(s$comment, 1, 40)
      )
    })

    # Rendered once per project; later changes go through replaceData() so the
    # selection and scroll position are kept.
    output$samples <- DT::renderDT({
      data <- shiny::isolate(table_data())
      selected <- match(shiny::isolate(current()) %||% NA_character_, data$Sample)
      if (is.na(selected)) selected <- if (nrow(data) > 0) 1L else NULL
      DT::datatable(
        data,
        selection = list(mode = "single", target = "row", selected = selected),
        rownames = FALSE,
        options = list(dom = "ft", paging = FALSE, scrollY = "45vh", scrollCollapse = TRUE,
                       language = list(search = "", searchPlaceholder = "Search samples"),
                       columnDefs = list(list(width = "20px", targets = 2)))
      )
    })
    table_proxy <- DT::dataTableProxy("samples")

    shiny::observeEvent(table_data(), {
      DT::replaceData(table_proxy, table_data(), rownames = FALSE, resetPaging = FALSE,
                      clearSelection = "none")
      # Rows may have been added or removed, so re-select by sample ID.
      i <- match(shiny::isolate(current()) %||% NA_character_, table_data()$Sample)
      DT::selectRows(table_proxy, if (is.na(i)) NULL else i)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$samples_rows_selected, {
      ids <- project()$samples$sample_id
      current(ids[input$samples_rows_selected])
    })

    select_sample <- function(id) {
      ids <- project()$samples$sample_id
      i <- match(id, ids)
      if (is.na(i)) return()
      current(id)
      DT::selectRows(table_proxy, i)
    }

    step_sample <- function(by) {
      ids <- project()$samples$sample_id
      i <- match(current(), ids)
      if (length(ids) == 0) return()
      j <- if (is.na(i)) 1 else min(max(i + by, 1), length(ids))
      select_sample(ids[j])
    }
    shiny::observeEvent(input$prev, step_sample(-1))
    shiny::observeEvent(input$next_sample, step_sample(1))

    # ---- data for the selected sample ----
    # The project object changes with every edit (a comment, a flag), but the
    # plot only depends on a few of its values. These keys change only when
    # those values do, so unrelated edits don't recompute or redraw the plot.
    # (A reactiveVal ignores being set to an identical value.)
    data_key <- shiny::reactiveVal(NULL)
    shiny::observe({
      p <- project()
      row <- sample_row()
      s <- sample_settings(p, row$sample_id)
      data_key(list(
        sample_id = row$sample_id, md5 = row$md5,
        import = import_values(s$import), reduction = s$reduction,
        run_speed = s$sample$run_speed_um_s,
        edits = as.list(row[c("trim_start_s", "trim_end_s", "reverse", "rezero",
                              "cp_enabled", "cp_method", "cp_penalty")]),
        calibration = calibration_inputs(p)
      ))
    })
    regions_key <- shiny::reactiveVal(NULL)
    shiny::observe({
      p <- project()
      id <- current()
      shiny::req(p, id)
      regions_key(dplyr::filter(p$regions, .data$sample_id == !!id))
    })

    sample_data <- shiny::reactive({
      key <- shiny::req(data_key())
      p <- shiny::isolate(project())
      shiny::withProgress(message = "Reducing", value = 0.5, {
        data <- project_data(p, key$sample_id)
      })
      if (key$edits$cp_enabled && nrow(data) > 2) {
        data <- try_notify(find_changepoints(data, key$edits$cp_method, key$edits$cp_penalty)) %||% data
      }
      data
    })

    sample_regions <- shiny::reactive(shiny::req(regions_key()))

    run_speed <- shiny::reactive({
      shiny::req(project(), current())
      sample_settings(project(), current())$sample$run_speed_um_s
    })

    output$sample_title <- shiny::renderText({
      row <- sample_row()
      paste0(row$sample_id, if (row$status == "excluded") " (excluded)")
    })

    # Whether the plotted 87Sr/86Sr is corrected to the primary reference
    # material (Calibration step) or raw.
    output$calibration_badge <- shiny::renderUI({
      d <- sample_data()
      cal <- shiny::isolate(project())$settings$calibration
      if (!"correction_status" %in% names(d) || nrow(d) == 0) {
        return(shiny::span(class = "badge text-bg-secondary", "Uncorrected (no calibration)"))
      }
      status <- d$correction_status[1]
      if (startsWith(status, "corrected")) {
        shiny::span(class = "badge text-bg-success",
                    paste0("Corrected to ", cal$primary, " (", cal$fit, ")"))
      } else {
        shiny::span(class = "badge text-bg-warning", paste("Uncorrected:", status))
      }
    })

    # Selection events can only be read once the plot exists (plotly warns
    # otherwise), so note when it has been drawn.
    plot_ready <- shiny::reactiveVal(FALSE)
    # Only the signal strips change the plot layout; the other display
    # options are switched in the browser (see below) without a rebuild.
    show_strips <- shiny::reactiveVal(TRUE)
    shiny::observe(show_strips("strips" %in% input$show))
    toggle_idx <- shiny::reactiveVal(integer())
    plot_renders <- shiny::reactiveVal(0L)  # counted for tests

    output$plot <- plotly::renderPlotly({
      show <- union(setdiff(shiny::isolate(input$show), "strips"), if (show_strips()) "strips")
      d <- sample_data()
      shiny::validate(shiny::need(
        nrow(d) > 0,
        if (shiny::isolate(sample_row())$run_type == "baseline") {
          "This is a baseline run: it is used for blank correction and has no 87Sr/86Sr profile."
        } else {
          "No data points pass the 88Sr thresholds; check the reduction settings."
        }
      ))
      p <- profile_plot(d, sample_regions(), show = show, source = ns("profile"))
      toggle_idx(toggle_trace_indices(p))
      plot_renders(shiny::isolate(plot_renders()) + 1L)
      plot_ready(TRUE)
      p
    })

    shiny::observeEvent(input$show, {
      idx <- toggle_idx()
      if (length(idx) == 0) return()
      visible <- lapply(names(idx), \(option) if (option %in% input$show) TRUE else "legendonly")
      plotly::plotlyProxy("plot", session) |>
        plotly::plotlyProxyInvoke("restyle", list(visible = visible), unname(idx))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    output$reduction_warnings <- shiny::renderUI({
      w <- attr(sample_data(), "warnings")
      if (length(w) == 0) return(NULL)
      shiny::div(class = "alert alert-warning py-1 px-2 mb-1 small", paste(unique(w), collapse = " "))
    })

    # ---- plot selection ----
    shiny::observeEvent(current(), selection(NULL))
    brushed <- shiny::reactive({
      shiny::req(plot_ready())
      plotly::event_data("plotly_brushed", source = ns("profile"))
    })
    shiny::observeEvent(brushed(), {
      b <- brushed()
      selection(if (is.null(b$x)) NULL else sort(b$x))
    }, ignoreNULL = FALSE)

    output$selection_text <- shiny::renderText({
      s <- selection()
      if (is.null(s)) return("No range selected. Drag across the plot to select one.")
      d <- sample_data()
      pts <- d[d$distance_um >= s[1] & d$distance_um <= s[2], ]
      sprintf("Selected %.0f\u2013%.0f \u00b5m: %d points, mean %.5f \u00b1 %.5f (2 SE)",
              s[1], s[2], sum(!is.na(pts$sr87_sr86)), mean(pts$sr87_sr86, na.rm = TRUE),
              2 * stats::sd(pts$sr87_sr86, na.rm = TRUE) / sqrt(sum(!is.na(pts$sr87_sr86))))
    })

    # Analysis-time range of the points inside the selected distance range.
    selected_time <- function() {
      s <- selection()
      if (is.null(s)) {
        shiny::showNotification("Select a range on the plot first.", type = "warning")
        return(NULL)
      }
      d <- sample_data()
      t <- d$time_s[d$distance_um >= s[1] & d$distance_um <= s[2]]
      if (length(t) == 0) {
        shiny::showNotification("The selected range contains no data points.", type = "warning")
        return(NULL)
      }
      range(t)
    }

    # ---- sync panel inputs when the sample (or its saved values) changes ----
    shiny::observeEvent(sample_row(), {
      row <- sample_row()
      speed <- run_speed()
      shiny::updateNumericInput(session, "trim_start_um", value = round(row$trim_start_s * speed, 1))
      shiny::updateNumericInput(session, "trim_end_um", value = round(row$trim_end_s * speed, 1))
      shiny::updateCheckboxInput(session, "rezero", value = row$rezero)
      shiny::updateCheckboxInput(session, "reverse", value = row$reverse)
      shiny::updateCheckboxInput(session, "cp_enabled", value = row$cp_enabled)
      shiny::updateSelectInput(session, "cp_method", selected = row$cp_method)
      shiny::updateNumericInput(session, "cp_penalty", value = row$cp_penalty)
      shiny::updateRadioButtons(session, "status", selected = row$status)
      shiny::updateCheckboxInput(session, "flag", value = row$flag)
      shiny::updateTextAreaInput(session, "comment", value = row$comment)
    })

    # Save a field only if it differs from the stored value, so inputs being
    # refreshed after a sample switch never write anything.
    save_field <- function(field, value) {
      row <- shiny::isolate(sample_row())
      old <- row[[field]]
      if (is.null(value) || length(value) != 1 || is.na(value)) return()
      same <- if (is.numeric(old)) isTRUE(abs(old - value) < 1e-9) else identical(old, value)
      if (!same) {
        args <- rlang::list2(!!field := value)
        change(\(p) rlang::exec(update_sample, p, row$sample_id, !!!args))
      }
    }

    shiny::observeEvent(input$trim_start_um, save_field("trim_start_s", input$trim_start_um / run_speed()),
                        ignoreInit = TRUE)
    shiny::observeEvent(input$trim_end_um, save_field("trim_end_s", input$trim_end_um / run_speed()),
                        ignoreInit = TRUE)
    shiny::observeEvent(input$rezero, save_field("rezero", input$rezero), ignoreInit = TRUE)
    shiny::observeEvent(input$reverse, save_field("reverse", input$reverse), ignoreInit = TRUE)
    shiny::observeEvent(input$cp_enabled, save_field("cp_enabled", input$cp_enabled), ignoreInit = TRUE)
    shiny::observeEvent(input$cp_method, save_field("cp_method", input$cp_method), ignoreInit = TRUE)
    shiny::observeEvent(input$cp_penalty, save_field("cp_penalty", input$cp_penalty), ignoreInit = TRUE)
    shiny::observeEvent(input$status, save_field("status", input$status), ignoreInit = TRUE)
    shiny::observeEvent(input$flag, save_field("flag", input$flag), ignoreInit = TRUE)
    shiny::observeEvent(input$comment, save_field("comment", input$comment), ignoreInit = TRUE)

    # ---- trims ----
    shiny::observeEvent(input$trim_to_selection, {
      t <- selected_time()
      if (is.null(t)) return()
      full <- project_data(project(), current(), edited = FALSE)
      change(update_sample, current(),
             trim_start_s = max(t[1] - min(full$time_s), 0),
             trim_end_s = max(max(full$time_s) - t[2], 0))
      selection(NULL)
    })
    shiny::observeEvent(input$trim_clear, {
      change(update_sample, current(), trim_start_s = 0, trim_end_s = 0)
    })

    # ---- regions ----
    output$regions <- shiny::renderTable({
      r <- sample_regions()
      if (nrow(r) == 0) return(data.frame(Note = "No regions yet"))
      summarise_regions(sample_data(), r, value = "sr87_sr86") |>
        dplyr::right_join(dplyr::select(r, "region"), by = "region") |>
        dplyr::transmute(
          Region = .data$region,
          From = as.integer(round(.data$distance_min_um)),
          To = as.integer(round(.data$distance_max_um)),
          Mean = sprintf("%.5f", .data$mean),
          SE2 = sprintf("%.5f", 2 * .data$se),
          n = .data$n
        ) |>
        stats::setNames(c("Region", "From (\u00b5m)", "To (\u00b5m)", "Mean", "2 SE", "n"))
    }, width = "100%", spacing = "xs")

    shiny::observe({
      p <- shiny::req(project())  # NULL right after the project is closed
      r <- sample_regions()
      shiny::updateSelectInput(session, "region_remove_name", choices = r$region)
      labels <- p$settings$regions$default_labels
      next_label <- setdiff(labels, r$region)[1]
      if (is.na(next_label)) next_label <- paste("Region", nrow(r) + 1)
      shiny::updateTextInput(session, "region_name", value = next_label)
    })

    shiny::observeEvent(input$region_add, {
      t <- selected_time()
      shiny::req(t, nzchar(input$region_name))
      r <- sample_regions()
      new <- dplyr::bind_rows(
        dplyr::select(dplyr::filter(r, .data$region != input$region_name), "region", "start_s", "end_s"),
        tibble::tibble(region = input$region_name, start_s = t[1], end_s = t[2])
      )
      change(set_regions, current(), new)
      selection(NULL)
    })

    shiny::observeEvent(input$region_remove, {
      shiny::req(input$region_remove_name)
      r <- sample_regions()
      change(set_regions, current(), dplyr::filter(r, .data$region != input$region_remove_name))
    })

    output$cp_summary <- shiny::renderText({
      row <- sample_row()
      if (!row$cp_enabled) return("")
      n <- length(unique(stats::na.omit(sample_data()$cp_segment)))
      sprintf("%d segment(s) found.", n)
    })

    # ---- reduction settings for this sample ----
    output$reduction_inputs <- shiny::renderUI({
      shiny::req(current())
      settings_inputs(ns, shiny::isolate(sample_settings(project(), current())),
                      c("reduction", "sample"), SAMPLE_PANEL_KEYS)
    })
    shiny::observeEvent(sample_row(), {
      update_settings_inputs(session, sample_settings(project(), current()),
                             c("reduction", "sample"), SAMPLE_PANEL_KEYS)
    })

    output$override_summary <- shiny::renderUI({
      overrides <- parse_overrides(sample_row()$settings)
      if (length(overrides) == 0) {
        return(shiny::p(class = "ifr-muted", "Using project settings."))
      }
      shiny::div(class = "alert alert-info py-1 px-2 small",
                 "Changed for this sample: ", format_overrides(overrides))
    })

    shiny::observeEvent(input$reduction_apply, {
      p <- project()
      id <- current()
      wanted <- settings_from_inputs(input, p$settings, c("reduction", "sample"), SAMPLE_PANEL_KEYS) |>
        override_defaults()
      wanted <- wanted[SAMPLE_PANEL_KEYS]
      project_values <- override_defaults(p$settings)
      current_overrides <- parse_overrides(sample_row()$settings)
      changes <- list()
      for (key in names(wanted)) {
        differs <- !identical(wanted[[key]], project_values[[key]])
        if (differs) {
          changes[key] <- list(wanted[[key]])
        } else if (key %in% names(current_overrides)) {
          changes[key] <- list(NULL)
        }
      }
      if (length(changes) == 0) return()
      shiny::withProgress(message = "Reducing", {
        change(\(p) rlang::exec(set_sample_settings, p, id, !!!changes))
      })
    })

    shiny::observeEvent(input$reduction_reset, {
      overrides <- parse_overrides(sample_row()$settings)
      if (length(overrides) == 0) return()
      resets <- rlang::set_names(rep(list(NULL), length(overrides)), names(overrides))
      change(\(p) rlang::exec(set_sample_settings, p, current(), !!!resets))
      update_settings_inputs(session, sample_settings(project(), current()),
                             c("reduction", "sample"), SAMPLE_PANEL_KEYS)
    })

    # ---- notes ----
    shiny::observeEvent(input$reviewed_next, {
      change(update_sample, current(), status = "reviewed")
      step_sample(1)
    })

    shiny::observeEvent(input$remove, {
      shiny::showModal(shiny::modalDialog(
        title = paste("Remove", current(), "?"),
        shiny::p("The sample, its regions and its edits are removed from the project. ",
                 "The runfile is kept in the project's raw/removed folder, and the removal ",
                 "is recorded in the session log."),
        shiny::p("To keep the sample but leave it out of exports, set its status to ",
                 shiny::strong("Excluded"), " instead."),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("remove_confirm"), "Remove sample", class = "btn-danger")
        )
      ))
    })

    shiny::observeEvent(input$remove_confirm, {
      shiny::removeModal()
      id <- current()
      ids <- project()$samples$sample_id
      i <- match(id, ids)
      neighbour <- c(ids[-i])[min(i, length(ids) - 1)]
      if (!is.null(change(remove_samples, id))) {
        shiny::showNotification(paste("Removed", id), type = "message")
        current(if (length(neighbour) == 1 && !is.na(neighbour)) neighbour else NULL)
      }
    })

    output$sample_info <- shiny::renderTable({
      row <- sample_row()
      runs <- project_runs(project())
      run <- runs[runs$sample_id == row$sample_id, ]
      type <- switch(row$run_type, baseline = "Baseline",
                     reference = paste0(row$reference_material, " (", run$rm_role, ")"), "Sample")
      data.frame(
        Field = c("Type", "Acquired", "Session", "Original file", "Added", "Added by",
                  "Stored as", "MD5"),
        Value = c(type, row$acquired, run$session, row$source, row$added, row$added_by,
                  row$runfile, substr(row$md5, 1, 12))
      )
    }, colnames = FALSE, width = "100%", spacing = "xs")

    output$sample_log <- shiny::renderTable({
      shiny::req(project(), current())
      log <- read_log(project()) |> dplyr::filter(.data$sample_id == !!current())
      if (nrow(log) == 0) return(NULL)
      log |>
        utils::tail(15) |>
        dplyr::transmute(When = sub("T", " ", substr(.data$time, 1, 16)), Who = .data$user,
                         What = dplyr::if_else(.data$field == "", .data$action, .data$field),
                         Old = .data$old, New = .data$new)
    }, width = "100%", spacing = "xs")
  })
}
