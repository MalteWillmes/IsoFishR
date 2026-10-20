app_project <- function(env = parent.frame()) {
  path <- file.path(withr::local_tempdir(.local_envir = env), "AppProject")
  p <- create_project(path, user = "tester")
  withr::defer(close_project(p), envir = env)
  add_runfiles(p, salmon_files(2))
}

salmon_files <- function(n) example_runfiles()[grepl("PC_2016", example_runfiles())][seq_len(n)]

test_that("profile_plot builds with regions, changepoints and without strips", {
  d <- reduce_runs(read_runfiles(salmon_files(1)))$data
  regions <- tibble::tibble(region = c("core", "edge"), start_s = c(200, 1000), end_s = c(260, 1100))
  b <- plotly::plotly_build(profile_plot(d, regions))
  expect_length(b$x$layout$shapes, 2)
  expect_equal(vapply(b$x$layout$annotations, `[[`, "", "text"), c("core", "edge"))

  cp <- find_changepoints(d)
  b2 <- plotly::plotly_build(profile_plot(cp, show = c("points", "smooth")))
  expect_true("Changepoint means" %in% vapply(b2$x$data, \(t) t$name %||% "", ""))
  expect_null(b2$x$layout$yaxis2)
})

test_that("workspace edits are saved to the project", {
  p <- app_project()
  rv <- shiny::reactiveVal(p)
  shiny::testServer(workspace_server, args = list(project = rv), {
    session$setInputs(samples_rows_selected = 1)
    expect_equal(current(), "PC_2016_01_A2_4R")
    expect_gt(nrow(sample_data()), 100)

    session$setInputs(status = "reviewed", flag = TRUE, comment = "nice profile")
    s <- rv()$samples[1, ]
    expect_equal(s$status, "reviewed")
    expect_true(s$flag)
    expect_equal(s$comment, "nice profile")

    # 100 µm trim at the default 10 µm/s run speed = 10 s
    session$setInputs(trim_start_um = 100)
    expect_equal(rv()$samples$trim_start_s[1], 10)
    expect_equal(min(sample_data()$distance_um), 0)

    # Selecting the second sample must not copy values from the first
    session$setInputs(next_sample = 1)
    expect_equal(current(), "PC_2016_01_A3_6R")
    expect_equal(rv()$samples$status[2], "new")
    expect_equal(rv()$samples$comment[2], "")

    # Per-sample reduction settings: apply, then reset
    session$setInputs(set_reduction_ma_window = 25, reduction_apply = 1)
    expect_equal(rv()$samples$settings[2], "ma_window=25")
    expect_equal(rv()$samples$settings[1], "")
    # The run speed can be changed per sample too
    session$setInputs(set_sample_run_speed_um_s = 5, reduction_apply = 2)
    expect_equal(rv()$samples$settings[2], "ma_window=25; run_speed_um_s=5")
    expect_equal(sample_settings(rv(), "PC_2016_01_A3_6R")$sample$run_speed_um_s, 5)
    d <- project_data(rv(), "PC_2016_01_A3_6R")
    expect_equal(diff(range(d$distance_um)) / diff(range(d$time_s)), 5)
    session$setInputs(reduction_reset = 1)
    expect_equal(rv()$samples$settings[2], "")

    session$setInputs(reviewed_next = 1)
    expect_equal(rv()$samples$status[2], "reviewed")
  })

  log <- read_log(p)
  expect_true(all(c("status", "flag", "comment", "trim_start_s", "ma_window") %in% log$field))
  # Switching samples refreshes inputs but must not log anything for sample 2
  # beyond the explicit changes made above.
  s2 <- dplyr::filter(log, sample_id == "PC_2016_01_A3_6R", action == "update_sample")
  expect_equal(s2$field, "status")
})

test_that("project settings page saves changed defaults", {
  p <- app_project()
  rv <- shiny::reactiveVal(p)
  shiny::testServer(project_settings_server, args = list(project = rv), {
    session$setInputs(set_reduction_ma_window = 15, set_sample_run_speed_um_s = 5,
                      region_labels = "Core\nEdge", save = 1)
    expect_equal(rv()$settings$reduction$ma_window, 15L)
    expect_equal(rv()$settings$sample$run_speed_um_s, 5)
    expect_equal(rv()$settings$regions$default_labels, c("Core", "Edge"))
  })
})

test_that("export page writes an export and previews the summaries", {
  p <- app_project()
  p <- set_regions(p, p$samples$sample_id[1], data.frame(region = "core", start_s = 200, end_s = 260))
  rv <- shiny::reactiveVal(p)
  shiny::testServer(export_server, args = list(project = rv), {
    expect_equal(nrow(preview_data()) > 0, TRUE)
    session$setInputs(export = 1)
    expect_true(dir.exists(last_dir()))
    expect_true(file.exists(file.path(last_dir(), "regions.csv")))
  })
})

test_that("removing a sample in the app selects a neighbouring sample", {
  p <- app_project()
  rv <- shiny::reactiveVal(p)
  shiny::testServer(workspace_server, args = list(project = rv), {
    session$setInputs(samples_rows_selected = 1)
    session$setInputs(remove = 1, remove_confirm = 1)
    expect_equal(rv()$samples$sample_id, "PC_2016_01_A3_6R")
    expect_equal(current(), "PC_2016_01_A3_6R")
    session$setInputs(remove_confirm = 2)
    expect_equal(nrow(rv()$samples), 0)
    expect_null(current())
  })
})

test_that("smoothed line uses every point even when outliers were rejected", {
  # Regression: a filtered `data =` on the outlier trace leaked into later traces.
  d <- reduce_runs(read_runfiles(salmon_files(2)[2]))$data
  expect_gt(sum(d$is_outlier), 0)
  b <- plotly::plotly_build(profile_plot(d))
  traces <- rlang::set_names(b$x$data, vapply(b$x$data, \(t) t$name %||% "", ""))
  n_values <- length
  expect_equal(n_values(traces[["Moving average"]]$y), nrow(d))
  expect_equal(n_values(traces[["Data"]]$y), sum(!is.na(d$sr87_sr86)))
  expect_equal(n_values(traces[["Outliers"]]$y), sum(d$is_outlier))
})

test_that("settings page loads presets, saves new ones and checks a runfile", {
  withr::local_options(isofishr.preset_dir = withr::local_tempdir())
  p <- app_project()
  rv <- shiny::reactiveVal(p)
  shiny::testServer(project_settings_server, args = list(project = rv), {
    # The form as rendered for a Nu_UCDavis project
    s <- p$settings
    inputs <- list(import_preset = "Nu_UCDavis",
                   set_import_skip_header = 46, set_import_skip_footer = 0,
                   set_import_sep = ",", set_import_header = FALSE)
    for (k in names(s$import$columns)) inputs[[paste0("set_import_col_", k)]] <- s$import$columns[[k]]
    do.call(session$setInputs, inputs)
    shown_preset("Nu_UCDavis")
    expect_true(import_matches_preset(form_import()))

    # Change the layout and save it as a preset
    session$setInputs(set_import_skip_header = 40, preset_name = "Lab_B",
                      preset_description = "test", preset_save = 1)
    expect_true("Lab_B" %in% list_import_presets()$name)
    expect_equal(get_import_preset("Lab_B")$skip_header, 40L)

    # A runfile that doesn't fit the 40-line header shows a warning or error
    session$setInputs(check_file = data.frame(name = "a.run", datapath = example_runfiles()[1]))
    expect_match(paste(as.character(output$check_result$html)), "alert-(warning|danger)")

    # Back to the right layout: the check succeeds
    session$setInputs(set_import_skip_header = 46)
    expect_match(paste(as.character(output$check_result$html)), "read 6895 cycles")

    # Saving the project records the preset name
    session$setInputs(import_preset = "Nu_UCDavis", save = 1)
    expect_equal(rv()$settings$import$preset, "Nu_UCDavis")
  })
})

test_that("the plot is only rebuilt when what it shows changes", {
  p <- app_project()
  rv <- shiny::reactiveVal(p)
  shiny::testServer(workspace_server, args = list(project = rv), {
    session$setInputs(samples_rows_selected = 1,
                      show = c("points", "outliers", "smooth", "band", "strips"))
    output$plot
    n <- plot_renders()
    expect_gte(n, 1)

    # Notes and display toggles: no rebuild
    session$setInputs(comment = "looks fine", flag = TRUE, status = "reviewed")
    output$plot
    session$setInputs(show = c("points", "smooth", "strips"))
    output$plot
    expect_equal(plot_renders(), n)
    expect_true(all(c("points", "smooth", "band", "seawater") %in% names(toggle_idx())))

    # Things that change the plot: rebuild
    session$setInputs(trim_start_um = 50)
    output$plot
    expect_equal(plot_renders(), n + 1)
    session$setInputs(show = c("points", "smooth"))
    output$plot
    expect_equal(plot_renders(), n + 2)
    session$setInputs(next_sample = 1)
    output$plot
    expect_equal(plot_renders(), n + 3)
  })
})

test_that("plot data is slimmed for the browser", {
  d <- reduce_runs(read_runfiles(salmon_files(1)))$data
  p <- profile_plot(d)
  expect_null(p$x$attrs)
  expect_true(all(vapply(p$x$data, \(t) length(t$hovertemplate) <= 1, logical(1))))
  pts <- p$x$data[[which(vapply(p$x$data, \(t) identical(t$name, "Data"), logical(1)))]]
  expect_equal(pts$type, "scattergl")
  json <- htmlwidgets:::toJSON(p$x)
  expect_lt(nchar(json, type = "bytes"), 250 * 1024)  # 540 KB before slimming
  # Arrays must reach plotly as numbers (pre-serialised JSON arrived as one string)
  expect_true(all(vapply(p$x$data, \(t) is.numeric(t$y), logical(1))))
})

test_that("calibration page assigns run types and sets up the correction", {
  runs <- data.frame(id = c("RM1", "fish1", "RM2"), minute = c(0, 20, 40),
                     ratio = c(0.70918 * 1.0002, 0.7105, 0.70918 * 1.0004))
  p <- synthetic_project(runs)
  rv <- shiny::reactiveVal(p)
  shiny::testServer(calibration_server, args = list(project = rv), {
    session$setInputs(rm_name = "MMC", rm_value = 0.70918, rm_unc = 0.00002, rm_note = "", rm_save = 1)
    expect_equal(rv()$reference_materials$name, "MMC")

    # Rows 1 and 3 in acquisition order are the standards
    session$setInputs(runs_rows_selected = c(1, 3), assign_type = "rm:MMC", assign = 1)
    expect_equal(rv()$samples$run_type, c("reference", "sample", "reference"))

    session$setInputs(primary = "MMC")
    expect_equal(rv()$settings$calibration$primary, "MMC")
    session$setInputs(fit = "step")
    expect_equal(rv()$settings$calibration$fit, "step")
    expect_equal(nrow(model()$points), 2)

    session$setInputs(runs_rows_selected = 2, session_name = "Afternoon", session_set = 1)
    expect_equal(rv()$samples$session[2], "Afternoon")
    session$setInputs(session_auto = 1)
    expect_equal(rv()$samples$session[2], "")
  })
  d <- project_data(shiny::isolate(rv()))
  expect_equal(unique(d$correction_status[d$sample_id == "fish1"]), "corrected")
})

test_that("the workspace plot follows changes to the calibration", {
  runs <- data.frame(id = c("RM1", "fish1"), minute = c(0, 20), ratio = c(0.70918 * 1.001, 0.71))
  p <- synthetic_project(runs)
  p <- set_reference_material(p, "MMC", 0.70918)
  p <- set_run_type(p, "RM1", "reference", "MMC")
  rv <- shiny::reactiveVal(p)
  shiny::testServer(workspace_server, args = list(project = rv), {
    session$setInputs(samples_rows_selected = 2, show = c("points", "smooth", "strips"))
    before <- mean(sample_data()$sr87_sr86, na.rm = TRUE)
    expect_equal(before, 0.71, tolerance = 1e-9)
    rv(update_project_settings(rv(), list(calibration = list(primary = "MMC"))))
    session$flushReact()
    expect_equal(mean(sample_data()$sr87_sr86, na.rm = TRUE), 0.71 / 1.001, tolerance = 1e-7)
  })
})

test_that("Quit releases the project lock and closes the project", {
  p <- app_project()
  path <- p$path
  close_project(p)
  shiny::testServer(app_server(initial_project = path, user = "tester"), {
    session$flushReact()
    expect_false(is.null(project()))
    expect_true(file.exists(file.path(path, ".lock")))
    session$setInputs(quit = 1)
    expect_null(project())
    expect_false(file.exists(file.path(path, ".lock")))
  })
})

test_that("creating a project from the start screen opens it", {
  # Regression: the new, empty project crashed the session (grey screen).
  parent <- withr::local_tempdir()
  shiny::testServer(app_server(user = "tester"), {
    session$setInputs(user_name = "tester", create_parent = parent, create_name = "New",
                      create_preset = "Nu_UCDavis", create = 1)
    expect_equal(project()$meta$name, "New")
    expect_equal(nrow(project()$samples), 0)
    expect_equal(opened_step(), "settings")  # new projects start on Settings
    close_project(project())
  })
  shiny::testServer(calibration_server,
                    args = list(project = shiny::reactiveVal(open_project(file.path(parent, "New"),
                                                                          user = "tester"))), {
    session$flushReact()
    expect_equal(nrow(runs_table()), 0)
    close_project(project())
  })
})

test_that("the Runfiles step skips or replaces same-name runs", {
  p <- app_project()
  rv <- shiny::reactiveVal(p)
  dir <- withr::local_tempdir()
  changed <- file.path(dir, "upload0.run")
  file.copy(salmon_files(1), changed)
  cat("\n", file = changed, append = TRUE)
  upload <- data.frame(name = "PC_2016_01_A2_4R.run", datapath = changed)
  shiny::testServer(runfiles_server, args = list(project = rv), {
    old_md5 <- rv()$samples$md5[1]
    session$setInputs(add_mode = "add", add_files = upload)
    expect_equal(rv()$samples$md5[1], old_md5)
    session$setInputs(add_mode = "replace", add_files = upload)
    expect_equal(rv()$samples$md5[1], unname(tools::md5sum(changed)))
    expect_equal(rv()$samples$source[1], "PC_2016_01_A2_4R.run")
    expect_equal(nrow(rv()$samples), 2)
  })
})

test_that("a project reopens on the step it was left on", {
  p <- app_project()
  path <- p$path
  write_last_step(p, "calibration")
  close_project(p)
  shiny::testServer(app_server(initial_project = path, user = "tester"), {
    session$flushReact()
    expect_equal(opened_step(), "calibration")
    session$setInputs(step = "export")
    expect_equal(read_last_step(project()), "export")
    session$setInputs(close_project = 1)
    expect_null(project())
  })
})

test_that("the Runfiles step adds a whole folder with subfolders", {
  path <- file.path(withr::local_tempdir(), "P")
  p <- create_project(path, user = "tester")
  rv <- shiny::reactiveVal(p)
  src <- withr::local_tempdir()
  dir.create(file.path(src, "day1"))
  dir.create(file.path(src, "day2"))
  file.copy(salmon_files(2)[1], file.path(src, "day1"))
  file.copy(salmon_files(2)[2], file.path(src, "day2"))
  shiny::testServer(runfiles_server, args = list(project = rv), {
    found(list(dir = src, files = find_runfiles(src, rv()$settings$import)))
    session$setInputs(add_mode = "add", add_folder = 1)
    expect_equal(nrow(rv()$samples), 2)
    expect_null(found())
    expect_equal(nrow(runs_table()), 2)
    session$setInputs(runs_rows_selected = 1, remove = 1, remove_confirm = 1)
    expect_equal(nrow(rv()$samples), 1)
  })
  close_project(shiny::isolate(rv()))
})

test_that("the Calibration step applies the SBO suggestion with the chosen value", {
  runs <- data.frame(id = c("SBO_1", "fish", "SBO_2"), minute = c(0, 10, 20),
                     ratio = c(0.70918, 0.7100, 0.70920))
  p <- synthetic_project(runs)
  rv <- shiny::reactiveVal(p)
  shiny::testServer(calibration_server, args = list(project = rv), {
    session$setInputs(sbo_value = 0.709175, sbo_apply = 1)
    expect_equal(rv()$reference_materials$value[rv()$reference_materials$name == "SBO"], 0.709175)
    expect_equal(rv()$settings$calibration$primary, "SBO")
    expect_equal(sum(rv()$samples$run_type == "reference"), 2)
    expect_equal(nrow(results()), 2)
  })
  b <- plotly::plotly_build(reference_material_plot(calibration_model(shiny::isolate(rv())),
                                                    reference_material_results(shiny::isolate(rv()))))
  names <- vapply(b$x$data, \(t) t$name %||% "", "")
  expect_true(all(c("Uncorrected", "Corrected", "Correction: Session mean") %in% names))

  # In run order the correction line covers every run of the session,
  # including the sample between the standards
  p <- shiny::isolate(rv())
  lines <- correction_lines(calibration_model(p), project_runs(p), "order")
  expect_equal(lines$x, 1:3)
  expect_equal(lines$y, rep(mean(c(0.70918, 0.70920)), 3), tolerance = 1e-5)  # session mean
  b <- plotly::plotly_build(reference_material_plot(calibration_model(p), reference_material_results(p),
                                                    x = "time", runs = project_runs(p)))
  expect_true("Correction: Session mean" %in% vapply(b$x$data, \(t) t$name %||% "", ""))
})
