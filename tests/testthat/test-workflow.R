# One test that walks through a project's life the way a user would:
# create, add runfiles, review and edit, close, reopen later, add more
# runfiles, change settings, remove a sample and export.

test_that("a project can be built up over several sessions and exported", {
  files <- example_runfiles()
  root <- withr::local_tempdir()
  path <- file.path(root, "Otoliths")

  # Session 1: create, add two runfiles, edit one
  p <- create_project(path, user = "alice")
  p <- add_runfiles(p, files[1:2])
  id1 <- p$samples$sample_id[1]
  p <- update_sample(p, id1, trim_start_s = profile_trim(p, id1, 15), status = "reviewed",
                     comment = "clear natal region")
  p <- set_regions(p, id1, data.frame(region = c("natal", "marine"), start_s = c(250, 900),
                                       end_s = c(350, 1100)))
  first_regions <- summarise_project_regions(p)
  close_project(p)

  # Session 2 (a colleague): reopen, add the rest, adjust one sample's settings
  p <- open_project(path, user = "bob")
  expect_equal(summarise_project_regions(p), first_regions)
  p <- add_runfiles(p, files[3:6])
  expect_equal(nrow(p$samples), 6)
  id2 <- p$samples$sample_id[2]
  p <- set_sample_settings(p, id2, blank_time_s = 25, ma_window = 15)
  p <- update_sample(p, id2, cp_enabled = TRUE)
  p <- update_sample(p, p$samples$sample_id[6], status = "excluded")
  p <- remove_samples(p, p$samples$sample_id[5])

  # Changing a project default re-reduces samples on the next read
  before <- summarise_project_samples(p)
  p <- update_project_settings(p, list(reduction = list(outlier_sd = 2)))
  after <- summarise_project_samples(p)
  expect_true(all(after$n_outliers >= before$n_outliers))
  expect_gt(sum(after$n_outliers), sum(before$n_outliers))

  dir <- export_project(p)
  close_project(p)

  # The export reflects every choice made in both sessions
  samples <- readr::read_csv(file.path(dir, "samples.csv"), show_col_types = FALSE)
  expect_equal(nrow(samples), 4)  # 6 added, 1 removed, 1 excluded
  expect_equal(samples$comment[samples$sample_id == id1], "clear natal region")
  expect_equal(samples$trim_start_um[samples$sample_id == id1], 150)
  expect_equal(samples$setting_overrides[samples$sample_id == id2], "blank_time_s=25; ma_window=15")
  expect_gt(samples$n_cp_segments[samples$sample_id == id2], 0)

  regions <- readr::read_csv(file.path(dir, "regions.csv"), show_col_types = FALSE)
  expect_equal(regions$region, c("natal", "marine"))

  settings <- readr::read_csv(file.path(dir, "settings.csv"), show_col_types = FALSE)
  expect_equal(unique(settings$reduction.outlier_sd), 2)
  expect_equal(settings$reduction.blank_time_s[settings$sample_id == id2], 25)

  log <- read_log(open_project(path, user = "carol", read_only = TRUE))
  expect_setequal(unique(log$user), c("alice", "bob"))
  expect_true(all(c("create_project", "add_sample", "update_sample", "set_regions",
                    "sample_setting", "project_setting", "remove_sample", "export") %in% log$action))
  expect_false(file.exists(file.path(path, ".lock")))
})
