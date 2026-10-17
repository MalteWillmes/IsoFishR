#' Run the IsoFishR app
#'
#' Opens the IsoFishR workspace in a browser. The app works on project
#' folders (see [create_project()]); every change is saved immediately, so
#' the app can be closed at any time and the project reopened later.
#'
#' @param project Optional path of a project to open straight away.
#' @param user Name recorded in the project log.
#' @param ... Passed to [shiny::shinyApp()] `options`, e.g. `port` or
#'   `launch.browser`.
#' @return A Shiny app object; printing it (or calling at the console) runs it.
#' @export
run_app <- function(project = NULL, user = default_user(), ...) {
  shiny::shinyApp(
    ui = app_ui,
    server = app_server(project, user),
    options = utils::modifyList(list(launch.browser = TRUE), list(...))
  )
}

app_ui <- function(request) {
  bslib::page_fillable(
    title = "IsoFishR",
    theme = bslib::bs_theme(version = 5, preset = "shiny", primary = "#1f6f8b"),
    padding = 0,
    gap = 0,
    shiny::tags$head(shiny::tags$style(shiny::HTML(app_css)),
                     shiny::tags$script(shiny::HTML(app_js))),
    shiny::uiOutput("main", fill = TRUE)
  )
}

# After Quit: replace the page with a short message (instead of Shiny's
# greyed-out "disconnected" screen) and try to close the tab. Browsers only
# let a page close tabs it opened itself, so the message stays if not.
app_js <- "
  // Ask before the tab is closed or reloaded by accident: closing it stops
  // the app (work is saved, but the app has to be started again). Quit
  // switches this off.
  window.ifrConfirmLeave = true;
  window.addEventListener('beforeunload', function(e) {
    if (!window.ifrConfirmLeave) return;
    e.preventDefault();
    e.returnValue = '';
  });

  Shiny.addCustomMessageHandler('ifr-quit', function(message) {
    window.ifrConfirmLeave = false;
    document.body.innerHTML =
      '<style>#shiny-disconnected-overlay{display:none!important}</style>' +
      '<div style=\"max-width:640px;margin:4rem auto;padding:0 1rem;font-family:sans-serif\">' +
      '<h2>IsoFishR has stopped</h2>' +
      '<p>Everything was saved. You can close this tab.</p>' +
      '<p style=\"color:#666\">To start again, run <code>IsoFishR::run_app()</code> in R.</p></div>';
    window.close();
  });
"

app_css <- "
  .ifr-project { color:var(--bs-secondary-color); font-weight:400; margin-left:.25rem; }
  .ifr-start { max-width:980px; margin:2rem auto; padding:0 1rem; }
  .ifr-recent a { display:block; padding:.15rem 0; word-break:break-all; }
  .ifr-muted { color:var(--bs-secondary-color); font-size:.875rem; }
  .ifr-sample-table table.dataTable tbody tr { cursor:pointer; }
  .ifr-panel .form-group { margin-bottom:.5rem; }
  .ifr-panel .shiny-input-container:not(.shiny-input-container-inline) { width:100%; }
"

app_server <- function(initial_project = NULL, user = default_user()) {
  # Shared by all browser sessions of this app.
  sessions <- new.env()
  sessions$n <- 0L

  function(input, output, session) {
    sessions$n <- sessions$n + 1L
    project <- shiny::reactiveVal(NULL)
    # Changes only when a project is opened or closed, so the layout is not
    # rebuilt on every edit.
    open_path <- shiny::reactiveVal(NULL)
    pending_open <- shiny::reactiveVal(NULL)
    # Page shown when a project opens: a new project starts on its settings.
    first_tab <- shiny::reactiveVal("Workspace")

    do_open <- function(path, read_only = FALSE, force = FALSE, tab = "Workspace") {
      first_tab(tab)
      user_name <- shiny::isolate(input$user_name) %||% user
      p <- tryCatch(
        open_project(path, user = user_name, read_only = read_only, force = force),
        error = function(e) e
      )
      if (inherits(p, "error")) {
        if (grepl("open for editing", conditionMessage(p))) {
          pending_open(path)
          shiny::showModal(shiny::modalDialog(
            title = "Project is in use",
            conditionMessage(p),
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton("open_read_only", "Open read-only"),
              shiny::actionButton("open_force", "Take over editing", class = "btn-warning")
            )
          ))
        } else {
          shiny::showNotification(conditionMessage(p), type = "error", duration = 10)
        }
        return(invisible(NULL))
      }
      old <- shiny::isolate(project())
      if (!is.null(old)) close_project(old)
      # Temporary (example) projects don't belong in the recent list.
      if (!startsWith(p$path, normalizePath(tempdir(), winslash = "/"))) remember_project(p$path)
      project(p)
      open_path(p$path)
    }

    shiny::observeEvent(input$open_read_only, {
      shiny::removeModal()
      do_open(pending_open(), read_only = TRUE)
    })
    shiny::observeEvent(input$open_force, {
      shiny::removeModal()
      do_open(pending_open(), force = TRUE)
    })

    if (!is.null(initial_project)) {
      shiny::observe(do_open(initial_project)) |> shiny::bindEvent(TRUE, once = TRUE)
    }

    output$main <- shiny::renderUI({
      if (is.null(open_path())) start_screen_ui(user, recent_projects())
      else workspace_page_ui(selected = shiny::isolate(first_tab()))
    })

    # ---- start screen ----
    shiny::observeEvent(input$browse_open, {
      path <- choose_folder("Select an IsoFishR project folder", input$open_path %||% "")
      if (!is.null(path)) shiny::updateTextInput(session, "open_path", value = path)
    })
    shiny::observeEvent(input$browse_parent, {
      path <- choose_folder("Select where to create the project", input$create_parent %||% "")
      if (!is.null(path)) shiny::updateTextInput(session, "create_parent", value = path)
    })
    shiny::observeEvent(input$open, {
      shiny::req(nzchar(input$open_path))
      do_open(input$open_path)
    })
    shiny::observeEvent(input$open_recent, do_open(input$open_recent))
    shiny::observeEvent(input$create, {
      shiny::req(nzchar(input$create_parent), nzchar(input$create_name))
      path <- file.path(input$create_parent, input$create_name)
      p <- try_notify(create_project(path, user = input$user_name %||% user,
                                     import_preset = input$create_preset %||% DEFAULT_PRESET))
      if (!is.null(p)) {
        close_project(p)
        do_open(p$path, tab = "Project settings")
      }
    })

    # ---- workspace ----
    output$project_title <- shiny::renderUI({
      p <- project()
      shiny::req(p)
      shiny::tagList(
        shiny::span(class = "ifr-project", p$meta$name),
        if (p$read_only) shiny::span(class = "badge text-bg-warning", "Read-only")
      )
    })
    shiny::observeEvent(input$close_project, {
      p <- project()
      if (!is.null(p)) close_project(p)
      project(NULL)
      open_path(NULL)
    })

    workspace_server("ws", project)
    calibration_server("calibration", project)
    export_server("export", project)
    project_settings_server("settings", project)
    log_server("log", project)

    # Quit: release the lock, tell the page, then stop the app (which frees
    # the R console). The short delay lets the message reach the browser.
    shiny::observeEvent(input$quit, {
      p <- project()
      if (!is.null(p)) close_project(p)
      project(NULL)
      session$sendCustomMessage("ifr-quit", list())
      later::later(function() shiny::stopApp(), delay = 0.5)
    })

    # Release the lock when the tab closes. Stop the app (freeing the R
    # console) once no browser tab is left, after a grace period so that
    # reloading the page does not stop it.
    session$onSessionEnded(function() {
      p <- shiny::isolate(project())
      if (!is.null(p)) close_project(p)
      sessions$n <- sessions$n - 1L
      later::later(function() if (sessions$n == 0L) shiny::stopApp(), delay = 5)
    })
  }
}

start_screen_ui <- function(user, recent) {
  shiny::div(
    class = "ifr-start",
    shiny::div(
      class = "d-flex align-items-center",
      shiny::h2("IsoFishR", class = "me-auto"),
      shiny::actionButton("quit", "Quit IsoFishR", class = "btn-sm btn-outline-secondary")
    ),
    shiny::p(class = "ifr-muted",
             "Data reduction and analysis of laser-ablation 87Sr/86Sr profiles. ",
             "Projects are folders on your computer; all changes are saved as you work."),
    shiny::textInput("user_name", "Your name (recorded in the project log)", user),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Open a project"),
        shiny::div(
          class = "d-flex gap-2 align-items-end",
          shiny::div(class = "flex-grow-1",
                     shiny::textInput("open_path", "Project folder", width = "100%")),
          shiny::div(class = "mb-3", shiny::actionButton("browse_open", "Browse\u2026"))
        ),
        shiny::actionButton("open", "Open", class = "btn-primary"),
        if (length(recent) > 0) shiny::div(
          class = "ifr-recent mt-3",
          shiny::strong("Recent projects"),
          purrr::map(recent, \(path) shiny::tags$a(
            href = "#", path,
            onclick = sprintf("Shiny.setInputValue('open_recent', %s, {priority: 'event'}); return false;",
                              jsonlite_string(path))
          ))
        )
      ),
      bslib::card(
        bslib::card_header("Create a project"),
        shiny::div(
          class = "d-flex gap-2 align-items-end",
          shiny::div(class = "flex-grow-1",
                     shiny::textInput("create_parent", "Create in folder", width = "100%")),
          shiny::div(class = "mb-3", shiny::actionButton("browse_parent", "Browse\u2026"))
        ),
        shiny::textInput("create_name", "Project name", placeholder = "e.g. Salmon_2024"),
        shiny::selectInput("create_preset", "Instrument preset (runfile layout)",
                           list_import_presets()$name, selected = DEFAULT_PRESET),
        shiny::p(class = "ifr-muted",
                 "Presets and reduction settings can be changed under Project settings ",
                 "before adding runfiles."),
        shiny::actionButton("create", "Create", class = "btn-primary")
      )
    )
  )
}

workspace_page_ui <- function(selected = "Workspace") {
  bslib::navset_bar(
    id = "page",
    selected = selected,
    title = shiny::span("IsoFishR ", shiny::uiOutput("project_title", inline = TRUE)),
    fillable = "Workspace",
    padding = 0,
    bslib::nav_panel("Workspace", workspace_ui("ws")),
    bslib::nav_panel("Project settings", project_settings_ui("settings")),
    bslib::nav_panel("Calibration", calibration_ui("calibration")),
    bslib::nav_panel("Export", export_ui("export")),
    bslib::nav_panel("Change log", log_ui("log")),
    bslib::nav_spacer(),
    bslib::nav_item(shiny::actionButton("close_project", "Close project", class = "btn-sm")),
    bslib::nav_item(shiny::actionButton("quit", "Quit", class = "btn-sm btn-outline-secondary"))
  )
}

# Quote a string for use in inline JavaScript.
jsonlite_string <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("'", "\\\\'", x)
  paste0("'", x, "'")
}
