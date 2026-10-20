#' Run the IsoFishR app
#'
#' Opens IsoFishR in a browser. The app walks through six steps: Project
#' (create or open), Settings, Runfiles, Calibration, Workspace and Export.
#' Every change is saved immediately, and a project reopens on the
#' step it was last worked on.
#'
#' @param project Optional path of a project to open straight away.
#' @param user Name recorded in the project log.
#' @param ... Passed to [shiny::shinyApp()] `options`, e.g. `port` or
#'   `launch.browser`.
#' @return A Shiny app object; printing it (or calling at the console) runs it.
#' @export
run_app <- function(project = NULL, user = default_user(), ...) {
  # Identifies this run of the app, so delayed stops scheduled by an earlier
  # run in the same R session (Quit, closed tab) don't stop this one.
  run_id <- basename(tempfile("run"))
  shiny::shinyApp(
    ui = app_ui,
    server = app_server(project, user, run_id = run_id),
    onStart = function() {
      # Uploads of many runfiles at once exceed Shiny's default 5 MB limit.
      options(shiny.maxRequestSize = 4 * 1024^3, isofishr.run_id = run_id)
    },
    options = utils::modifyList(list(launch.browser = TRUE), list(...))
  )
}

# The steps of the app, in order: id = navigation value, label = tab title.
APP_STEPS <- c(project = "Project", settings = "Settings", runfiles = "Runfiles",
               calibration = "Calibration", workspace = "Workspace", export = "Export")

app_ui <- function(request) {
  # Images (the logo) in inst/www.
  shiny::addResourcePath("isofishr", system.file("www", package = "IsoFishR"))
  bslib::page_navbar(
    id = "step",
    title = shiny::span("IsoFishR", shiny::uiOutput("project_title", inline = TRUE)),
    window_title = "IsoFishR",
    theme = app_theme(),
    fillable = "workspace",
    header = shiny::tags$head(shiny::tags$style(shiny::HTML(app_css)),
                              shiny::tags$script(shiny::HTML(app_js))),
    bslib::nav_panel(APP_STEPS[["project"]], value = "project", project_step_ui()),
    bslib::nav_panel(APP_STEPS[["settings"]], value = "settings", project_settings_ui("settings")),
    bslib::nav_panel(APP_STEPS[["runfiles"]], value = "runfiles", runfiles_ui("runfiles")),
    bslib::nav_panel(APP_STEPS[["calibration"]], value = "calibration", calibration_ui("calibration")),
    bslib::nav_panel(APP_STEPS[["workspace"]], value = "workspace", workspace_ui("ws")),
    bslib::nav_panel(APP_STEPS[["export"]], value = "export", export_ui("export")),
    bslib::nav_spacer(),
    bslib::nav_item(shiny::actionLink("show_log", "Session log")),
    bslib::nav_item(shiny::actionButton("quit", "Quit IsoFishR", class = "btn-sm btn-outline-secondary"))
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

# The default Bootstrap 5 theme, which bslib ships precompiled: a customised
# theme is compiled with Sass when the app starts, which takes seconds. The
# colour and the compact spacing are set with CSS variables instead (app_css).
app_theme <- function() {
  bslib::bs_theme(version = 5)
}

app_css <- "
  :root {
    --bs-primary:#1f6f8b; --bs-primary-rgb:31,111,139;
    --bs-link-color:#1f6f8b; --bs-link-color-rgb:31,111,139;
    --bs-link-hover-color:#174f63; --bs-link-hover-color-rgb:23,79,99;
    --bs-body-font-size:.9rem; --bslib-spacer:.6rem; --bslib-sidebar-padding:.75rem;
  }
  .btn { --bs-btn-padding-y:.25rem; --bs-btn-font-size:.9rem; }
  .btn-sm { --bs-btn-padding-y:.2rem; --bs-btn-font-size:.8rem; }
  .btn-primary {
    --bs-btn-bg:#1f6f8b; --bs-btn-border-color:#1f6f8b;
    --bs-btn-hover-bg:#195b72; --bs-btn-hover-border-color:#195b72;
    --bs-btn-active-bg:#174f63; --bs-btn-active-border-color:#174f63;
    --bs-btn-disabled-bg:#1f6f8b; --bs-btn-disabled-border-color:#1f6f8b;
  }
  .form-control, .form-select, .selectize-input, .selectize-dropdown { font-size:.9rem; }
  .form-control, .form-select { padding-top:.25rem; padding-bottom:.25rem; }
  .form-control:focus, .form-select:focus { border-color:#8fb7c5; box-shadow:0 0 0 .2rem rgba(31,111,139,.25); }
  .form-check-input:checked { background-color:#1f6f8b; border-color:#1f6f8b; }
  .form-label, .control-label { margin-bottom:.15rem; }
  .card { --bs-card-spacer-y:.5rem; --bs-card-spacer-x:.75rem; --bs-card-cap-padding-y:.35rem; }
  .navbar { --bs-navbar-padding-y:.25rem; }
  .nav { --bs-nav-link-padding-y:.35rem; }
  .modal { --bs-modal-padding:.75rem; }
  h1, h2, h3, h4, h5, h6 { margin-bottom:.35rem; }
  p { margin-bottom:.5rem; }
  hr { margin:.5rem 0; }
  .progress-bar { background-color:#1f6f8b; }

  .ifr-project { color:var(--bs-secondary-color); font-weight:400; margin-left:.5rem; }
  .ifr-start { max-width:980px; margin:.75rem auto; padding:0 .75rem; }
  .ifr-logo { display:block; height:64px; width:auto; max-width:100%; margin:.25rem 0 .75rem; }
  .ifr-recent a { display:block; padding:.1rem 0; word-break:break-all; }
  .ifr-muted { color:var(--bs-secondary-color); font-size:.85rem; }
  .ifr-sample-table table.dataTable tbody tr { cursor:pointer; }
  .form-group, .shiny-input-container { margin-bottom:.4rem; }
  .card-body.bslib-gap-spacing > .shiny-input-container { margin-bottom:0; }
  .shiny-input-container > label, .form-group > label { margin-bottom:.15rem; }
  .ifr-panel .shiny-input-container:not(.shiny-input-container-inline) { width:100%; }
  .ifr-lines .shiny-input-container { margin-bottom:0; }
  .ifr-lines .form-control { padding-top:.1rem; padding-bottom:.1rem; }
  /* Settings page: label and input on one line, to fit more on the screen */
  .ifr-settings .card-body.bslib-gap-spacing { gap:.3rem; }
  .ifr-settings .shiny-input-container:not(.shiny-input-container-inline):not(:has(textarea)):not(:has(.checkbox)):not(:has(input[type=file])) {
    display:grid; grid-template-columns:minmax(0,1fr) minmax(120px,45%); align-items:center;
    column-gap:.5rem; width:100%; margin-bottom:0;
  }
  .ifr-settings .shiny-input-container > label { margin-bottom:0; line-height:1.2; }
  .ifr-settings .shiny-input-container .checkbox { margin:0; }
  .ifr-settings .shiny-input-container:has(textarea), .ifr-settings .shiny-input-container:has(input[type=file]) { width:100%; }
  .ifr-settings .card-body > p, .ifr-settings h6 { margin-bottom:.2rem; }
  .ifr-settings hr { margin:.35rem 0; }
  .ifr-step { max-width:1600px; margin:0 auto; padding:.5rem .75rem; }
  .shiny-input-container .progress { margin-bottom:.25rem; }
  table.dataTable thead th, table.dataTable thead td { padding:4px 8px !important; }
  table.dataTable tbody td { padding:2px 8px !important; }
  table.dataTable thead input { padding:1px 4px; font-size:.8rem; }
  .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_paginate { padding-top:.25rem; font-size:.85rem; }
"

app_server <- function(initial_project = NULL, user = default_user(), run_id = NULL) {
  # Shared by all browser sessions of this app.
  sessions <- new.env()
  sessions$n <- 0L
  # Is this run of the app (see run_app()) still the one running?
  this_run <- function() is.null(run_id) || identical(getOption("isofishr.run_id"), run_id)

  function(input, output, session) {
    sessions$n <- sessions$n + 1L
    project <- shiny::reactiveVal(NULL)
    pending_open <- shiny::reactiveVal(NULL)
    # The step a project was opened on (also used by the tests).
    opened_step <- shiny::reactiveVal(NULL)

    # Steps 2-6 are only shown while a project is open.
    show_steps <- function(show) {
      for (step in setdiff(names(APP_STEPS), "project")) {
        if (show) bslib::nav_show("step", step) else bslib::nav_hide("step", step)
      }
    }
    show_steps(FALSE)

    do_open <- function(path, read_only = FALSE, force = FALSE, step = NULL) {
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
      if (!startsWith(p$path, normalizePath(tempdir(), winslash = "/"))) remember_project(p$path)
      project(p)
      # Back to where the project was left; a new project starts on Settings.
      step <- step %||% read_last_step(p) %||% "settings"
      if (!step %in% names(APP_STEPS) || step == "project") step <- "settings"
      opened_step(step)
      show_steps(TRUE)
      bslib::nav_select("step", step)
    }

    close_current <- function() {
      p <- shiny::isolate(project())
      if (!is.null(p)) close_project(p)
      project(NULL)
      show_steps(FALSE)
      bslib::nav_select("step", "project")
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

    # Remember the step the user is on (not the Project step itself).
    shiny::observeEvent(input$step, {
      p <- project()
      if (!is.null(p) && input$step != "project") write_last_step(p, input$step)
    }, ignoreInit = TRUE)

    output$project_title <- shiny::renderUI({
      p <- project()
      if (is.null(p)) return(NULL)
      shiny::tagList(
        shiny::span(class = "ifr-project", p$meta$name),
        if (p$read_only) shiny::span(class = "badge text-bg-warning ms-1", "Read-only")
      )
    })

    # ---- step 1: project ----
    output$recent <- shiny::renderUI({
      project()  # refresh after opening a project
      recent <- recent_projects()
      if (length(recent) == 0) return(NULL)
      shiny::div(
        class = "ifr-recent mt-2",
        shiny::strong("Recent projects"),
        purrr::map(recent, \(path) shiny::tags$a(
          href = "#", path,
          onclick = sprintf("Shiny.setInputValue('open_recent', %s, {priority: 'event'}); return false;",
                            jsonlite_string(path))
        ))
      )
    })

    output$current_project <- shiny::renderUI({
      p <- project()
      if (is.null(p)) return(NULL)
      runs <- p$samples
      bslib::card(
        class = "border-primary",
        bslib::card_header(shiny::strong("Open project: ", p$meta$name)),
        shiny::p(shiny::code(p$path)),
        shiny::p(sprintf("%d run(s): %d sample(s), %d reference material run(s), %d baseline run(s).",
                         nrow(runs), sum(runs$run_type == "sample"),
                         sum(runs$run_type == "reference"), sum(runs$run_type == "baseline"))),
        shiny::div(
          class = "d-flex gap-2",
          shiny::actionButton("continue", paste("Continue to", APP_STEPS[[read_last_step(p) %||% "settings"]]),
                              class = "btn-primary"),
          shiny::actionButton("close_project", "Close project")
        )
      )
    })

    shiny::observeEvent(input$continue, {
      p <- shiny::req(project())
      bslib::nav_select("step", read_last_step(p) %||% "settings")
    })
    shiny::observeEvent(input$close_project, close_current())

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
        do_open(p$path, step = "settings")
      }
    })

    # ---- steps 2-6 ----
    project_settings_server("settings", project)
    runfiles_server("runfiles", project)
    calibration_server("calibration", project)
    workspace_server("ws", project)
    export_server("export", project)
    log_server("log", project)

    shiny::observeEvent(input$show_log, {
      shiny::showModal(shiny::modalDialog(
        title = "Session log", size = "xl", easyClose = TRUE,
        if (is.null(project())) "Open a project to see its session log." else log_ui("log"),
        footer = shiny::modalButton("Close")
      ))
    })

    # Quit: release the lock, tell the page, then stop the app (which frees
    # the R console). The short delay lets the message reach the browser.
    shiny::observeEvent(input$quit, {
      p <- project()
      if (!is.null(p)) close_project(p)
      project(NULL)
      session$sendCustomMessage("ifr-quit", list())
      later::later(function() if (this_run()) shiny::stopApp(), delay = 0.5)
    })

    # Release the lock when the tab closes. Stop the app (freeing the R
    # console) once no browser tab is left, after a grace period so that
    # reloading the page does not stop it.
    session$onSessionEnded(function() {
      p <- shiny::isolate(project())
      if (!is.null(p)) close_project(p)
      sessions$n <- sessions$n - 1L
      later::later(function() if (sessions$n == 0L && this_run()) shiny::stopApp(), delay = 5)
    })
  }
}

project_step_ui <- function() {
  shiny::div(
    class = "ifr-start",
    shiny::img(src = "isofishr/logo.png", alt = "IsoFishR", class = "ifr-logo"),
    shiny::p(class = "ifr-muted",
             "Data reduction and analysis of 87Sr/86Sr isotope ratios obtained via ",
             "laser-ablation mass spectrometry. The aim of this application is to provide a ",
             "rapid, reliable, and reproducible data reduction and analysis tool that can be ",
             "applied across disciplines. A paper outlining the application is available in ",
             shiny::a("PLOS ONE",
                      href = "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0204519",
                      target = "_blank", rel = "noopener", .noWS = "after"),
             "."),
    shiny::uiOutput("current_project"),
    shiny::textInput("user_name", "Your name (recorded in the project log)", default_user()),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Open a project"),
        shiny::div(
          class = "d-flex gap-2 align-items-end",
          shiny::div(class = "flex-grow-1",
                     shiny::textInput("open_path", "Project folder", width = "100%")),
          shiny::div(class = "mb-2", shiny::actionButton("browse_open", "Browse\u2026"))
        ),
        shiny::actionButton("open", "Open", class = "btn-primary"),
        shiny::uiOutput("recent")
      ),
      bslib::card(
        bslib::card_header("Create a project"),
        shiny::div(
          class = "d-flex gap-2 align-items-end",
          shiny::div(class = "flex-grow-1",
                     shiny::textInput("create_parent", "Create in folder", width = "100%")),
          shiny::div(class = "mb-2", shiny::actionButton("browse_parent", "Browse\u2026"))
        ),
        shiny::textInput("create_name", "Project name", placeholder = "e.g. Salmon_2024"),
        shiny::selectInput("create_preset", "Instrument preset (runfile layout)",
                           list_import_presets()$name, selected = DEFAULT_PRESET),
        shiny::actionButton("create", "Create", class = "btn-primary")
      )
    )
  )
}

# Quote a string for use in inline JavaScript.
jsonlite_string <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("'", "\\\\'", x)
  paste0("'", x, "'")
}
