local_preset_dir <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_options(isofishr.preset_dir = dir, .local_envir = env)
  dir
}

neptune_layout <- function() {
  import <- default_settings()$import
  import$skip_header <- 12L
  import$sep <- "\t"
  import$columns <- list(raw88 = 6L, raw87 = 5L, raw86 = 4L, raw85 = 3L, raw84 = 2L,
                         raw83 = 1L, time_s = 7L)
  import
}

test_that("Nu_UCDavis is built in and is the default layout", {
  local_preset_dir()
  presets <- list_import_presets()
  expect_equal(presets$name, "Nu_UCDavis")
  expect_true(presets$builtin)
  expect_identical(get_import_preset("Nu_UCDavis"), default_settings()$import)
  expect_equal(default_settings()$import$preset, "Nu_UCDavis")
  expect_true(import_matches_preset(default_settings()$import))
})

test_that("user presets can be saved, listed, loaded and deleted", {
  dir <- local_preset_dir()
  save_import_preset("Neptune_Lab2", neptune_layout(), description = "Neptune, tab separated")
  expect_true(file.exists(file.path(dir, "Neptune_Lab2.yml")))

  presets <- list_import_presets()
  expect_equal(presets$name, c("Nu_UCDavis", "Neptune_Lab2"))
  expect_equal(presets$builtin, c(TRUE, FALSE))
  expect_equal(presets$description[2], "Neptune, tab separated")

  loaded <- get_import_preset("Neptune_Lab2")
  expect_equal(loaded$preset, "Neptune_Lab2")
  expect_equal(import_values(loaded), import_values(neptune_layout()))

  expect_error(save_import_preset("Neptune_Lab2", neptune_layout()), "already exists")
  expect_no_error(save_import_preset("Neptune_Lab2", neptune_layout(), overwrite = TRUE))

  delete_import_preset("Neptune_Lab2")
  expect_equal(list_import_presets()$name, "Nu_UCDavis")
  expect_error(get_import_preset("Neptune_Lab2"), "Unknown import preset")
})

test_that("built-in presets are protected and names are checked", {
  local_preset_dir()
  expect_error(save_import_preset("Nu_UCDavis", neptune_layout()), "built-in")
  expect_error(delete_import_preset("Nu_UCDavis"), "built-in")
  expect_error(save_import_preset("my preset", neptune_layout()), "letters, numbers")
})

test_that("invalid layouts are rejected", {
  local_preset_dir()
  bad <- neptune_layout()
  bad$columns$raw87 <- bad$columns$raw88
  expect_error(save_import_preset("Bad", bad), "different column")
  bad <- neptune_layout()
  bad$columns$time_s <- 0L
  expect_error(save_import_preset("Bad", bad), "1 or more")
  bad <- neptune_layout()
  bad$skip_header <- -1
  expect_error(merge_settings(default_settings(), list(import = bad)), "skip_header")
})

test_that("changed values no longer match their preset", {
  local_preset_dir()
  import <- default_settings()$import
  import$skip_header <- 40L
  expect_false(import_matches_preset(import))
  import$preset <- ""
  expect_false(import_matches_preset(import))
})

test_that("projects can be created from a preset and keep a copy of it", {
  local_preset_dir()
  save_import_preset("Neptune_Lab2", neptune_layout())
  path <- file.path(withr::local_tempdir(), "P")
  p <- create_project(path, user = "tester", import_preset = "Neptune_Lab2")
  close_project(p)
  expect_equal(p$settings$import$preset, "Neptune_Lab2")
  expect_equal(p$settings$import$skip_header, 12L)

  # The project still opens with its layout after the preset is deleted
  delete_import_preset("Neptune_Lab2")
  p2 <- open_project(path, user = "tester")
  on.exit(close_project(p2))
  expect_equal(import_values(p2$settings$import), import_values(neptune_layout()))
  expect_false(import_matches_preset(p2$settings$import))
})

test_that("renaming the preset alone does not invalidate the cache", {
  local_preset_dir()
  save_import_preset("Copy", default_settings()$import)
  path <- file.path(withr::local_tempdir(), "P")
  p <- create_project(path, user = "tester")
  on.exit(close_project(p))
  p <- add_runfiles(p, example_runfiles()[1])
  project_data(p)
  cached <- list.files(file.path(p$path, "cache"), full.names = TRUE)
  p <- update_project_settings(p, list(import = list(preset = "Copy")))
  project_data(p)
  expect_equal(list.files(file.path(p$path, "cache"), full.names = TRUE), cached)
})
