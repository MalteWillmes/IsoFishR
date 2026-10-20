# Project settings and session log pages.

project_settings_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "container-fluid py-2",
    shiny::div(
      class = "d-flex align-items-center gap-3 mb-2",
      shiny::actionButton(ns("save"), "Save project settings", class = "btn-primary flex-shrink-0"),
      shiny::p(class = "ifr-muted mb-0",
               "Project defaults. Samples with their own settings (Workspace > Reduction) ",
               "keep those overrides. Changing reduction settings reprocesses samples the next ",
               "time they are viewed.")
    ),
    shiny::uiOutput(ns("form"))
  )
}

preset_choices <- function(current = "") {
  names <- list_import_presets()$name
  choices <- stats::setNames(names, names)
  if (!nzchar(current) || !current %in% names) choices <- c("Custom (no preset)" = "", choices)
  choices
}

project_settings_server <- function(id, project) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # The preset shown when the form was built, so rebuilding the form does
    # not overwrite a project's changed values with the preset's.
    shown_preset <- shiny::reactiveVal(NULL)

    # Built once per project; the inputs then hold the current values.
    output$form <- shiny::renderUI({
      p <- project()
      shiny::req(p)
      s <- shiny::isolate(p$settings)
      shown_preset(s$import$preset %||% "")
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        bslib::card(bslib::card_header("Data reduction"), settings_inputs(ns, s, "reduction")),
        bslib::card(
          bslib::card_header("Samples and laser"),
          settings_inputs(ns, s, "sample"),
          shiny::textAreaInput(ns("region_labels"), "Default region names (one per line)",
                               paste(s$regions$default_labels, collapse = "\n"), rows = 4)
        ),
        bslib::card(
          bslib::card_header("Runfile import"),
          shiny::p(class = "ifr-muted",
                   "Must match the instrument's runfile layout before runfiles are added. ",
                   "Choose a preset, or set the layout and save it as a preset for reuse."),
          shiny::selectInput(ns("import_preset"), "Instrument preset",
                             preset_choices(s$import$preset %||% ""),
                             selected = s$import$preset %||% ""),
          shiny::uiOutput(ns("preset_status")),
          settings_inputs(ns, s, "import"),
          column_inputs(ns, s$import$columns),
          shiny::hr(),
          shiny::h6("Check the layout with a runfile"),
          shiny::fileInput(ns("check_file"), NULL, buttonLabel = "Choose runfile\u2026",
                           placeholder = "Nothing is added to the project"),
          shiny::uiOutput(ns("check_result")),
          shiny::hr(),
          shiny::h6("Save this layout as a preset"),
          shiny::textInput(ns("preset_name"), "Preset name", placeholder = "e.g. Neptune_Lab2"),
          shiny::textInput(ns("preset_description"), "Description (optional)"),
          shiny::div(
            class = "d-flex gap-2",
            shiny::actionButton(ns("preset_save"), "Save preset", class = "btn-sm"),
            shiny::actionButton(ns("preset_delete"), "Delete selected preset\u2026",
                                class = "btn-sm btn-outline-danger")
          ),
          shiny::p(class = "ifr-muted small mt-2",
                   "Presets are saved for your user account and are available in all projects. ",
                   "Share one by copying its file from ", shiny::code(import_preset_dir()), ".")
        )
      )
    }) |> shiny::bindEvent(project()$path)

    # The import layout currently in the form (not yet saved).
    form_import <- shiny::reactive({
      p <- shiny::req(project())
      import <- settings_from_inputs(input, p$settings, "import")$import
      import$preset <- input$import_preset %||% ""
      import
    })

    # Choosing a preset loads its values into the form.
    shiny::observeEvent(input$import_preset, {
      name <- input$import_preset
      if (identical(name, shown_preset())) return()
      shown_preset(name)
      if (!nzchar(name)) return()
      preset <- try_notify(get_import_preset(name))
      if (is.null(preset)) return()
      update_settings_inputs(session, list(import = preset), "import")
      update_column_inputs(session, preset$columns)
    }, ignoreInit = TRUE)

    output$preset_status <- shiny::renderUI({
      import <- form_import()
      name <- import$preset
      msg <- if (!nzchar(name)) {
        "Custom layout, not saved as a preset."
      } else if (import_matches_preset(import)) {
        paste0("Matches the ", name, " preset.")
      } else {
        paste0("Changed from the ", name, " preset. Save it under a new name to reuse it.")
      }
      shiny::p(class = "ifr-muted small", msg)
    })

    shiny::observeEvent(input$preset_save, {
      name <- trimws(input$preset_name)
      shiny::req(nzchar(name))
      saved <- try_notify(
        save_import_preset(name, form_import(), description = input$preset_description),
        success = paste0("Saved preset ", name, ".")
      )
      if (is.null(saved)) return()
      shown_preset(name)
      shiny::updateSelectInput(session, "import_preset", choices = preset_choices(name),
                               selected = name)
      shiny::updateTextInput(session, "preset_name", value = "")
      shiny::updateTextInput(session, "preset_description", value = "")
    })

    shiny::observeEvent(input$preset_delete, {
      name <- input$import_preset
      presets <- list_import_presets()
      if (!nzchar(name) || isTRUE(presets$builtin[presets$name == name])) {
        shiny::showNotification("Only presets you saved yourself can be deleted.", type = "warning")
        return()
      }
      shiny::showModal(shiny::modalDialog(
        title = paste0("Delete preset ", name, "?"),
        "Projects that use it keep their own copy of the layout.",
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("preset_delete_confirm"), "Delete preset", class = "btn-danger")
        )
      ))
    })

    shiny::observeEvent(input$preset_delete_confirm, {
      shiny::removeModal()
      name <- input$import_preset
      try_notify(delete_import_preset(name), success = paste("Deleted preset", name))
      shown_preset("")
      shiny::updateSelectInput(session, "import_preset", choices = preset_choices(""), selected = "")
    })

    output$check_result <- shiny::renderUI({
      f <- shiny::req(input$check_file)
      check_runfile_ui(f$datapath, f$name, form_import())
    })

    shiny::observeEvent(input$save, {
      p <- project()
      new <- settings_from_inputs(input, p$settings, c("reduction", "sample", "import"))
      new$import$preset <- input$import_preset %||% ""
      labels <- trimws(strsplit(input$region_labels %||% "", "\n")[[1]])
      new$regions$default_labels <- labels[nzchar(labels)]
      if (nrow(p$samples) > 0 && !identical(import_values(new$import), import_values(p$settings$import))) {
        shiny::showNotification(
          "Import settings changed: samples already in the project will be re-read with the new layout.",
          type = "warning", duration = 8)
      }
      updated <- try_notify(update_project_settings(p, new), success = "Project settings saved.")
      if (!is.null(updated)) project(updated)
    })
  })
}

update_column_inputs <- function(session, columns) {
  for (key in names(COLUMN_KEYS)) {
    shiny::updateNumericInput(session, paste0("set_import_col_", key), value = columns[[key]])
  }
}

# Read a runfile with an import layout and summarise what came out, so a
# wrong layout is obvious before any files are added.
check_runfile_ui <- function(path, name, import) {
  raw <- tryCatch(read_runfile(path, import, sample_id = "check"), error = function(e) e)
  if (inherits(raw, "error")) {
    return(shiny::div(class = "alert alert-danger py-1 px-2 small", conditionMessage(raw)))
  }
  if (nrow(raw) == 0) {
    return(shiny::div(class = "alert alert-danger py-1 px-2 small",
                      "No data rows were read. Check the header length and separator."))
  }
  start <- attr(raw, "data_start")
  problems <- c(
    if (!is.null(start)) sprintf(paste0(
      "The data starts on line %d, but the header setting expects line %d. IsoFishR found it ",
      "automatically; if all your files look like this, set 'Header lines to skip' to %d."),
      start, import$skip_header + 1L, start - 1L),
    if (any(diff(raw$time_s) <= 0)) "The time column does not always increase; check its column number.",
    if (max(raw$raw88) < 0.05) "The 88Sr signal never exceeds 0.05 V; check the 88Sr column.",
    if (stats::median(raw$raw86 / raw$raw88, na.rm = TRUE) > 0.5)
      "86Sr is larger than half of 88Sr; the mass columns may be swapped."
  )
  head_rows <- utils::head(raw[, c("time_s", "raw88", "raw87", "raw86", "raw85", "raw84", "raw83")], 5)
  shiny::tagList(
    shiny::div(
      class = paste("alert py-1 px-2 small", if (length(problems)) "alert-warning" else "alert-success"),
      sprintf("%s: read %d cycles, %.1f to %.1f s, 88Sr up to %.2f V.",
              name, nrow(raw), min(raw$time_s), max(raw$time_s), max(raw$raw88)),
      if (length(problems)) shiny::tags$ul(class = "mb-0", lapply(problems, shiny::tags$li))
    ),
    simple_table(head_rows, digits = 5)
  )
}

# A small static HTML table (for use inside renderUI).
simple_table <- function(df, digits = 3) {
  cells <- lapply(df, \(x) if (is.numeric(x)) formatC(x, digits = digits, format = "g") else x)
  shiny::tags$table(
    class = "table table-sm small mb-0",
    shiny::tags$thead(shiny::tags$tr(lapply(names(df), shiny::tags$th))),
    shiny::tags$tbody(lapply(seq_len(nrow(df)), \(i) {
      shiny::tags$tr(lapply(cells, \(col) shiny::tags$td(col[i])))
    }))
  )
}

log_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "container-fluid py-2", DT::DTOutput(ns("log")))
}

log_server <- function(id, project) {
  shiny::moduleServer(id, function(input, output, session) {
    output$log <- DT::renderDT({
      p <- project()
      shiny::req(p)
      log <- read_log(p) |>
        dplyr::mutate(time = sub("T", " ", substr(.data$time, 1, 19))) |>
        dplyr::arrange(dplyr::desc(dplyr::row_number()))
      DT::datatable(log, rownames = FALSE, filter = "top",
                    options = list(pageLength = 50, dom = "tip"))
    })
  })
}
