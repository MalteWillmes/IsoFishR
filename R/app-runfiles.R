# Step 3: add runfiles (single files or whole folders) and manage the runs
# of the project.

runfiles_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "ifr-step",
    bslib::layout_columns(
      col_widths = c(4, 8),
      bslib::card(
        bslib::card_header("Add runfiles"),
        shiny::radioButtons(
          ns("add_mode"), "If a run with the same name is already in the project",
          c("Keep it (skip the new file)" = "add", "Replace it with the new file" = "replace"),
          selected = "add"
        ),
        shiny::p(class = "ifr-muted small",
                 "Replaced runs keep their edits, regions and notes; the old file is kept in ",
                 "the project's raw/removed folder."),
        shiny::hr(),
        shiny::h6("Individual files"),
        shiny::fileInput(ns("add_files"), NULL, multiple = TRUE, width = "100%",
                         buttonLabel = "Choose files\u2026", placeholder = "No files chosen"),
        shiny::hr(),
        shiny::h6("A whole folder (including subfolders)"),
        shiny::actionButton(ns("choose_folder"), "Choose folder\u2026"),
        shiny::uiOutput(ns("folder_summary")),
        shiny::uiOutput(ns("extension_note"))
      ),
      bslib::card(
        bslib::card_header(
          class = "d-flex align-items-center",
          shiny::span("Runs in this project", class = "me-auto"),
          shiny::actionButton(ns("remove"), "Remove selected\u2026",
                              class = "btn-sm btn-outline-danger")
        ),
        shiny::p(class = "ifr-muted small",
                 "In acquisition order. Click rows to select them. Mark reference materials ",
                 "and baselines in step 4, Calibration."),
        DT::DTOutput(ns("runs"))
      )
    )
  )
}

runfiles_server <- function(id, project) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    found <- shiny::reactiveVal(NULL)  # runfiles found in the chosen folder

    change <- function(fun, ...) {
      p <- try_notify(fun(project(), ...))
      if (!is.null(p)) project(p)
      invisible(p)
    }

    add <- function(paths, names) {
      n_before <- nrow(project()$samples)
      shiny::withProgress(message = sprintf("Adding %d runfile(s)", length(paths)), {
        change(add_runfiles, paths, source_names = names, mode = input$add_mode %||% "add")
      })
      sbo <- suggest_reference_runs(project())
      if (nrow(project()$samples) > n_before && length(sbo) > 0) {
        shiny::showNotification(
          sprintf("%d run(s) look like SBO reference material runs; see step 4, Calibration.",
                  length(sbo)), type = "message", duration = 8)
      }
    }

    shiny::observeEvent(input$add_files, {
      f <- input$add_files
      add(f$datapath, f$name)
    })

    output$extension_note <- shiny::renderUI({
      ext <- shiny::req(project())$settings$import$extension %||% ".run"
      shiny::p(class = "ifr-muted small mt-2",
               "Picks up all ", shiny::code(ext), " files (set under step 2, Settings).")
    })

    shiny::observeEvent(input$choose_folder, {
      dir <- choose_folder("Select a folder with runfiles")
      if (is.null(dir)) return()
      found(c(list(dir = dir), list(files = find_runfiles(dir, project()$settings$import))))
    })

    output$folder_summary <- shiny::renderUI({
      f <- found()
      if (is.null(f)) return(NULL)
      files <- f$files
      known <- files$sample_id %in% project()$samples$sample_id
      ok <- !files$duplicate
      shiny::div(
        class = "mt-2",
        shiny::p(class = "small", shiny::code(f$dir)),
        shiny::p(sprintf("Found %d runfile(s) in %d folder(s).", nrow(files),
                         length(unique(files$folder)))),
        if (any(files$duplicate)) shiny::div(
          class = "alert alert-warning py-1 px-2 small",
          "Skipped, because files with the same name are in different subfolders: ",
          paste(unique(files$file[files$duplicate]), collapse = ", ")
        ),
        if (any(known & ok)) shiny::p(class = "ifr-muted small", sprintf(
          "%d already in the project (%s).", sum(known & ok),
          if (identical(input$add_mode, "replace")) "will be replaced if changed" else "will be skipped"
        )),
        if (sum(ok) > 0) shiny::actionButton(ns("add_folder"), sprintf("Add %d runfile(s)", sum(ok)),
                                             class = "btn-primary btn-sm")
      )
    })

    shiny::observeEvent(input$add_folder, {
      files <- dplyr::filter(found()$files, !.data$duplicate)
      add(files$path, files$file)
      found(NULL)
    })

    # ---- runs table ----
    runs_table <- shiny::reactive({
      p <- shiny::req(project())
      r <- project_runs(p)
      src <- p$samples$source[match(r$sample_id, p$samples$sample_id)]
      tibble::tibble(
        `#` = r$sequence,
        Run = r$sample_id,
        Acquired = ifelse(is.na(r$acquired), "", format(r$acquired, "%Y-%m-%d %H:%M", tz = "UTC")),
        Session = r$session,
        Type = dplyr::case_when(r$run_type == "baseline" ~ "Baseline",
                                r$run_type == "reference" ~ r$reference_material,
                                .default = "Sample"),
        File = src
      )
    })

    output$runs <- DT::renderDT({
      DT::datatable(shiny::isolate(runs_table()), rownames = FALSE, filter = "top",
                    selection = list(mode = "multiple", target = "row"),
                    options = list(pageLength = 50, dom = "tip", ordering = FALSE))
    })
    proxy <- DT::dataTableProxy("runs")
    shiny::observeEvent(runs_table(), {
      DT::replaceData(proxy, runs_table(), rownames = FALSE, resetPaging = FALSE,
                      clearSelection = "all")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$remove, {
      ids <- runs_table()$Run[input$runs_rows_selected]
      if (length(ids) == 0) {
        shiny::showNotification("Select runs in the table first.", type = "warning")
        return()
      }
      shiny::showModal(shiny::modalDialog(
        title = sprintf("Remove %d run(s)?", length(ids)),
        shiny::p(paste(utils::head(ids, 20), collapse = ", "), if (length(ids) > 20) "\u2026"),
        shiny::p("Their regions and edits are removed from the project. The runfiles are kept in ",
                 "the project's raw/removed folder, and the removal is recorded in the change log."),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("remove_confirm"), "Remove", class = "btn-danger")
        )
      ))
    })
    shiny::observeEvent(input$remove_confirm, {
      shiny::removeModal()
      ids <- runs_table()$Run[input$runs_rows_selected]
      shiny::req(length(ids) > 0)
      change(remove_samples, ids)
    })
  })
}
