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
