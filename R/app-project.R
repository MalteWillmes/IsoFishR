# Project settings and change log pages.

project_settings_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "container-fluid py-3",
    shiny::p(class = "ifr-muted",
             "Project defaults. Samples with their own settings (Workspace \u2192 Reduction) ",
             "keep those overrides. Changing reduction settings reprocesses samples the next ",
             "time they are viewed."),
    shiny::uiOutput(ns("form")),
    shiny::actionButton(ns("save"), "Save project settings", class = "btn-primary")
  )
}

project_settings_server <- function(id, project) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Built once per project; the inputs then hold the current values.
    output$form <- shiny::renderUI({
      p <- project()
      shiny::req(p)
      s <- shiny::isolate(p$settings)
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
                   "The defaults suit Nu Plasma files."),
          settings_inputs(ns, s, "import"),
          column_inputs(ns, s$import$columns)
        )
      )
    }) |> shiny::bindEvent(project()$path)

    shiny::observeEvent(input$save, {
      p <- project()
      new <- settings_from_inputs(input, p$settings, c("reduction", "sample", "import"))
      labels <- trimws(strsplit(input$region_labels %||% "", "\n")[[1]])
      new$regions$default_labels <- labels[nzchar(labels)]
      if (nrow(p$samples) > 0 && !identical(new$import, p$settings$import)) {
        shiny::showNotification(
          "Import settings changed: samples already in the project will be re-read with the new layout.",
          type = "warning", duration = 8)
      }
      updated <- try_notify(update_project_settings(p, new), success = "Project settings saved.")
      if (!is.null(updated)) project(updated)
    })
  })
}

log_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "container-fluid py-3", DT::DTOutput(ns("log")))
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
