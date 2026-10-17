# Export page: preview the summary tables and write them to the project's
# exports/ folder.

export_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    border = FALSE,
    sidebar = bslib::sidebar(
      width = 300,
      title = "Export summaries",
      shiny::p(class = "ifr-muted",
               "Writes the tables to a new dated folder in the project's exports folder. ",
               "Earlier exports are kept."),
      shiny::checkboxInput(ns("include_excluded"), "Include excluded samples", FALSE),
      shiny::checkboxInput(ns("include_data"), "Also export every data point (data.csv)", FALSE),
      shiny::actionButton(ns("export"), "Export", class = "btn-primary"),
      shiny::uiOutput(ns("last_export"))
    ),
    bslib::navset_card_underline(
      bslib::nav_panel("Regions", DT::DTOutput(ns("regions"))),
      bslib::nav_panel("Samples", DT::DTOutput(ns("samples"))),
      bslib::nav_panel("Changepoints", DT::DTOutput(ns("changepoints")))
    )
  )
}

export_server <- function(id, project) {
  shiny::moduleServer(id, function(input, output, session) {
    last_dir <- shiny::reactiveVal(NULL)

    preview_data <- shiny::reactive({
      p <- shiny::req(project())
      ids <- export_samples(p, input$include_excluded)$sample_id
      shiny::req(length(ids) > 0)
      shiny::withProgress(message = "Reducing samples", project_data(p, ids))
    })

    preview_table <- function(x) {
      num <- names(x)[vapply(x, is.double, logical(1))]
      ratio_cols <- intersect(num, c("mean", "sd", "se", "mean_smoothed"))
      DT::datatable(x, rownames = FALSE, filter = "top",
                    options = list(pageLength = 25, dom = "tip", scrollX = TRUE)) |>
        DT::formatRound(ratio_cols, digits = 6) |>
        DT::formatRound(setdiff(num, ratio_cols), digits = 1)
    }

    output$regions <- DT::renderDT({
      x <- summarise_project_regions(project(), preview_data(), input$include_excluded)
      shiny::validate(shiny::need(nrow(x) > 0, "No regions defined yet."))
      preview_table(x)
    })
    output$samples <- DT::renderDT({
      preview_table(summarise_project_samples(project(), preview_data(), input$include_excluded))
    })
    output$changepoints <- DT::renderDT({
      x <- summarise_project_changepoints(project(), preview_data(), input$include_excluded)
      shiny::validate(shiny::need(nrow(x) > 0, "No samples have changepoint detection enabled."))
      preview_table(x)
    })

    shiny::observeEvent(input$export, {
      dir <- shiny::withProgress(message = "Exporting", try_notify(
        export_project(project(), input$include_excluded, input$include_data),
        success = "Summaries exported."
      ))
      if (!is.null(dir)) last_dir(dir)
    })

    output$last_export <- shiny::renderUI({
      dir <- last_dir()
      shiny::req(dir)
      shiny::div(
        class = "mt-3",
        shiny::p(class = "ifr-muted small", "Exported to:", shiny::br(), shiny::code(dir)),
        shiny::actionButton(session$ns("open_folder"), "Open folder", class = "btn-sm")
      )
    })

    shiny::observeEvent(input$open_folder, open_folder(last_dir()))
  })
}

# Open a folder in the system file browser (the app runs locally).
open_folder <- function(path) {
  if (.Platform$OS.type == "windows") {
    shell.exec(normalizePath(path))
  } else if (Sys.info()[["sysname"]] == "Darwin") {
    system2("open", shQuote(path), wait = FALSE)
  } else {
    system2("xdg-open", shQuote(path), wait = FALSE)
  }
}
