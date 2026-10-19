new_project <- function(env = parent.frame()) {
  path <- withr::local_tempdir(.local_envir = env)
  project_dir <- file.path(path, "MyProject")
  p <- create_project(project_dir, user = "tester")
  withr::defer(close_project(p), envir = env)
  p
}

salmon <- function(n = 2) example_runfiles()[grepl("PC_2016", example_runfiles())][seq_len(n)]

test_that("create_project builds the folder layout and opens the project", {
  p <- new_project()
  expect_s3_class(p, "isofishr_project")
  expect_true(all(file.exists(file.path(p$path, c(
    "project.yml", "samples.csv", "regions.csv", "log.csv", ".lock", "raw", "cache", "exports"
  )))))
  expect_equal(p$meta$name, "MyProject")
  expect_equal(p$settings, default_settings())
  expect_equal(nrow(p$samples), 0)
  expect_error(create_project(p$path), "already contains")
  expect_output(print(p), "Samples: 0")
})

test_that("create_project refuses a non-empty folder", {
  path <- withr::local_tempdir()
  writeLines("x", file.path(path, "notes.txt"))
  expect_error(create_project(path), "not empty")
})

test_that("runfiles are copied, registered and logged", {
  p <- add_runfiles(new_project(), salmon(2))
  expect_equal(p$samples$sample_id, c("PC_2016_01_A2_4R", "PC_2016_01_A3_6R"))
  expect_equal(p$samples$status, c("new", "new"))
  expect_true(all(file.exists(file.path(p$path, p$samples$runfile))))
  expect_equal(unname(tools::md5sum(file.path(p$path, p$samples$runfile))), p$samples$md5)

  log <- read_log(p)
  expect_equal(log$action, c("create_project", "add_sample", "add_sample"))
  expect_equal(log$user, rep("tester", 3))
})

test_that("adding the same file twice or a clashing name is skipped", {
  p <- add_runfiles(new_project(), salmon(1))
  expect_message(p2 <- add_runfiles(p, salmon(1)), "already in the project")
  expect_equal(nrow(p2$samples), 1)
  expect_message(p3 <- add_runfiles(p, salmon(2)[2], sample_ids = "PC_2016_01_A2_4R"), "choose Replace")
  expect_equal(nrow(p3$samples), 1)
  expect_message(p4 <- add_runfiles(p, salmon(2)[2], sample_ids = "a/b"), "not allowed")
  expect_equal(attr(p4, "import_report")$result, "failed")
  expect_equal(nrow(p4$samples), 1)
})

test_that("files without readable data are reported and not copied", {
  p <- new_project()
  bad <- withr::local_tempfile(fileext = ".run")
  writeLines(c(rep("header", 46), "1,2,3"), bad)
  expect_message(p2 <- add_runfiles(p, bad), "Could not read 1 file")
  expect_equal(attr(p2, "import_report")$result, "failed")
  expect_match(attr(p2, "import_report")$detail, "No data found")
  expect_length(list.files(file.path(p$path, "raw")), 0)
})

test_that("one bad file doesn't stop the batch", {
  p <- new_project()
  dir <- withr::local_tempdir()
  bad <- file.path(dir, "broken.run")
  writeLines(c("garbage", "more garbage"), bad)
  p <- suppressMessages(add_runfiles(p, c(salmon(2)[1], bad, salmon(2)[2])))
  report <- attr(p, "import_report")
  expect_equal(report$result, c("added", "failed", "added"))
  expect_equal(nrow(p$samples), 2)
})

test_that("runfiles with a damaged header are read from where the data starts", {
  # As after a bad save on the instrument: some header lines missing, or extra
  # lines added. The data must come out the same as from the intact file.
  lines <- readLines(salmon(1))
  dir <- withr::local_tempdir()
  short <- file.path(dir, "short_header.run")
  long <- file.path(dir, "long_header.run")
  writeLines(lines[-(10:13)], short)
  writeLines(c(lines[1:20], "extra line", "\"another\"", lines[21:length(lines)]), long)

  import <- default_settings()$import
  intact <- read_runfiles(salmon(1), import, "x")
  for (f in c(short, long)) {
    d <- read_runfile(f, import, "x")
    expect_false(is.null(attr(d, "data_start")))
    attr(d, "data_start") <- NULL
    expect_equal(d, intact)
  }

  p <- new_project()
  p <- suppressMessages(add_runfiles(p, c(short, long)))
  report <- attr(p, "import_report")
  expect_equal(report$result, c("added", "added"))
  expect_match(report$detail[1], "data found from line 43 \\(expected line 47\\)")
  expect_match(report$detail[2], "data found from line 49")
  # The damaged timestamp line is reported, the data is reduced normally
  expect_match(report$detail[1], "no readable acquisition time")
  expect_gt(nrow(project_data(p, "short_header")), 1000)
})

test_that("sample edits are saved, logged and survive reopening", {
  p <- add_runfiles(new_project(), salmon(1))
  id <- p$samples$sample_id
  p <- update_sample(p, id, status = "reviewed", flag = TRUE, comment = "check edge",
                     trim_start_s = 5, reverse = TRUE)
  p <- update_sample(p, id, flag = TRUE)  # unchanged: not logged again

  log <- dplyr::filter(read_log(p), action == "update_sample")
  expect_equal(log$field, c("status", "flag", "comment", "trim_start_s", "reverse"))
  expect_equal(log$old[1:2], c("new", "FALSE"))
  expect_equal(log$new[1:2], c("reviewed", "TRUE"))

  close_project(p)
  p2 <- open_project(p$path, user = "tester")
  on.exit(close_project(p2))
  expect_equal(p2$samples, p$samples)
  expect_error(update_sample(p2, id, status = "done"), "status must be")
  expect_error(update_sample(p2, id, md5 = "x"), "Cannot change")
  expect_error(update_sample(p2, "nope", flag = TRUE), "Unknown sample")
})

test_that("numeric-looking sample IDs stay text", {
  p <- add_runfiles(new_project(), salmon(1), sample_ids = "007")
  close_project(p)
  p2 <- open_project(p$path, user = "tester")
  on.exit(close_project(p2))
  expect_identical(p2$samples$sample_id, "007")
})

test_that("per-sample settings override the project and can be reset", {
  p <- add_runfiles(new_project(), salmon(2))
  ids <- p$samples$sample_id
  p <- set_sample_settings(p, ids[1], ma_window = 25, outlier_sd = 0)
  expect_equal(p$samples$settings[1], "ma_window=25; outlier_sd=0")
  expect_identical(sample_settings(p, ids[1])$reduction$ma_window, 25L)
  expect_identical(sample_settings(p, ids[2])$reduction$ma_window, 10L)

  p <- set_sample_settings(p, ids[1], ma_window = NULL)
  expect_equal(p$samples$settings[1], "outlier_sd=0")
  expect_error(set_sample_settings(p, ids[1], smoothing = "loess"), "smoothing")
  expect_error(set_sample_settings(p, ids[1], speed = 3), "Unknown")

  log <- dplyr::filter(read_log(p), action == "sample_setting")
  expect_equal(log$field, c("ma_window", "outlier_sd", "ma_window"))
  expect_equal(log$new[3], "(project: 10)")
})

test_that("regions are replaced per sample and logged", {
  p <- add_runfiles(new_project(), salmon(2))
  ids <- p$samples$sample_id
  p <- set_regions(p, ids[1], data.frame(region = c("core", "edge"), start_s = c(40, 200),
                                         end_s = c(80, 260)))
  p <- set_regions(p, ids[2], data.frame(region = "core", start_s = 50, end_s = 90))
  p <- set_regions(p, ids[1], data.frame(region = "core", start_s = 45, end_s = 80))
  expect_equal(p$regions$sample_id, c(ids[2], ids[1]))
  expect_equal(p$regions$start_s, c(50, 45))
  expect_error(set_regions(p, ids[1], data.frame(region = "x", start_s = 5, end_s = 1)), "start")

  log <- dplyr::filter(read_log(p), action == "set_regions")
  expect_equal(nrow(log), 3)
  expect_equal(log$old[3], "core: 40-80 s; edge: 200-260 s")
})

test_that("project settings changes are saved and logged per setting", {
  p <- new_project()
  p <- update_project_settings(p, list(reduction = list(ma_window = 15L),
                                       sample = list(run_speed_um_s = 5)))
  log <- dplyr::filter(read_log(p), action == "project_setting")
  expect_setequal(log$field, c("reduction.ma_window", "sample.run_speed_um_s"))
  close_project(p)
  p2 <- open_project(p$path, user = "tester")
  on.exit(close_project(p2))
  expect_equal(p2$settings$reduction$ma_window, 15L)
  expect_equal(p2$meta$name, "MyProject")
})

test_that("the lock stops a second user, allows read-only access and can be forced", {
  p <- new_project()
  expect_error(open_project(p$path, user = "someone_else"), "open for editing by tester")

  ro <- open_project(p$path, user = "someone_else", read_only = TRUE)
  expect_error(update_project_settings(ro, list(reduction = list(ma_window = 3L))), "read-only")
  close_project(ro)
  expect_true(file.exists(file.path(p$path, ".lock")))

  forced <- open_project(p$path, user = "someone_else", force = TRUE)
  expect_error(update_project_settings(p, list(reduction = list(ma_window = 3L))),
               "no longer holds the project lock")
  close_project(forced)
  expect_false(file.exists(file.path(p$path, ".lock")))
})

test_that("a taken-over session cannot release the new owner's lock", {
  # Two app tabs run in the same R process, so the process ID is not enough
  # to tell them apart.
  p <- new_project()
  second <- open_project(p$path, user = "tester")
  close_project(p)
  expect_true(file.exists(file.path(p$path, ".lock")))
  expect_no_error(update_project_settings(second, list(reduction = list(ma_window = 3L))))
  close_project(second)
  expect_false(file.exists(file.path(p$path, ".lock")))
})

test_that("project_data reduces with per-sample settings, applies edits and caches", {
  p <- add_runfiles(new_project(), salmon(2))
  ids <- p$samples$sample_id
  d <- project_data(p)
  expect_equal(unique(d$sample_id), ids)
  expect_identical(
    dplyr::filter(d, sample_id == ids[1]),
    dplyr::filter(apply_edits(reduce_runs(read_runfiles(salmon(1)))$data), sample_id == ids[1])
  )
  cache_files <- function() list.files(file.path(p$path, "cache"))
  expect_length(cache_files(), 2)
  before <- cache_files()

  # Changing one sample's settings only re-reduces that sample
  p <- set_sample_settings(p, ids[2], ma_window = 30)
  d2 <- project_data(p)
  after <- cache_files()
  expect_length(cache_files(), 2)
  expect_equal(length(intersect(before, after)), 1)  # only sample 2 was re-reduced
  expect_identical(dplyr::filter(d2, sample_id == ids[1]), dplyr::filter(d, sample_id == ids[1]))
  expect_false(identical(dplyr::filter(d2, sample_id == ids[2])$sr87_sr86_ma,
                         dplyr::filter(d, sample_id == ids[2])$sr87_sr86_ma))

  # Edits are applied on top of the cached reduction
  p <- update_sample(p, ids[1], trim_start_s = 20)
  d3 <- project_data(p, ids[1])
  expect_equal(min(d3$time_s), min(dplyr::filter(d, sample_id == ids[1])$time_s) + 20,
               tolerance = 0.5)
  expect_equal(min(d3$distance_um), 0)

  # Excluded samples are left out by default
  p <- update_sample(p, ids[2], status = "excluded")
  expect_equal(unique(project_data(p)$sample_id), ids[1])
})

test_that("project_regions summarises the stored regions", {
  p <- add_runfiles(new_project(), salmon(1))
  id <- p$samples$sample_id
  p <- set_regions(p, id, data.frame(region = c("core", "edge"), start_s = c(200, 1000),
                                     end_s = c(260, 1100)))
  r <- project_regions(p)
  expect_equal(r$region, c("core", "edge"))
  expect_true(all(r$mean > 0.703 & r$mean < 0.712))
  expect_true(all(r$n > 10))
})

test_that("removing a sample drops its rows, regions and cache but keeps the runfile", {
  p <- add_runfiles(new_project(), salmon(2))
  ids <- p$samples$sample_id
  p <- set_regions(p, ids[1], data.frame(region = "core", start_s = 200, end_s = 260))
  p <- set_regions(p, ids[2], data.frame(region = "core", start_s = 200, end_s = 260))
  project_data(p)
  runfile <- p$samples$runfile[1]

  p <- remove_samples(p, ids[1])
  expect_equal(p$samples$sample_id, ids[2])
  expect_equal(p$regions$sample_id, ids[2])
  expect_false(file.exists(file.path(p$path, runfile)))
  kept <- list.files(file.path(p$path, "raw", "removed"))
  expect_length(kept, 1)
  expect_match(kept, basename(runfile), fixed = TRUE)
  expect_length(list.files(file.path(p$path, "cache")), 1)

  log <- dplyr::filter(read_log(p), action == "remove_sample")
  expect_equal(log$sample_id, ids[1])
  expect_equal(log$new, file.path("raw", "removed", kept))

  # Survives reopening, and the same file can be added again
  close_project(p)
  p2 <- open_project(p$path, user = "tester")
  on.exit(close_project(p2))
  expect_equal(p2$samples$sample_id, ids[2])
  p2 <- add_runfiles(p2, salmon(1))
  expect_equal(p2$samples$sample_id, c(ids[2], ids[1]))
  expect_error(remove_samples(p2, "nope"), "Unknown sample")
})

test_that("runfiles can be added in batches, skipping or replacing same-name runs", {
  p <- add_runfiles(new_project(), salmon(2))
  ids <- p$samples$sample_id
  p <- update_sample(p, ids[1], comment = "keep me", trim_start_s = 5)
  p <- set_regions(p, ids[1], data.frame(region = "core", start_s = 200, end_s = 260))

  # A second batch: one new file and a re-exported version of an existing run
  dir <- withr::local_tempdir()
  changed <- file.path(dir, paste0(ids[1], ".run"))
  file.copy(salmon(1), changed)
  cat("\n", file = changed, append = TRUE)  # different content, same name
  batch <- c(changed, salmon(3)[3])

  expect_message(p2 <- add_runfiles(p, batch), "Added 1 run")
  expect_equal(nrow(p2$samples), 3)
  expect_equal(p2$samples$md5[1], p$samples$md5[1])  # kept

  expect_message(p3 <- add_runfiles(p2, batch, mode = "replace"), "Replaced 1 run")
  expect_equal(nrow(p3$samples), 3)
  row <- p3$samples[p3$samples$sample_id == ids[1], ]
  expect_equal(row$md5, unname(tools::md5sum(changed)))
  expect_equal(row$comment, "keep me")              # edits are kept
  expect_equal(row$trim_start_s, 5)
  expect_equal(nrow(p3$regions), 1)
  expect_length(list.files(file.path(p3$path, "raw", "removed")), 1)
  expect_true("replace_runfile" %in% read_log(p3)$action)
  expect_gt(nrow(project_data(p3, ids[1])), 100)    # reduced from the new file

  # Replacing with the identical file again changes nothing
  expect_message(p4 <- add_runfiles(p3, changed, mode = "replace"), "identical file")
  expect_equal(p4$samples, p3$samples)
  expect_message(p5 <- add_runfiles(p3, c(salmon(1), salmon(1)), sample_ids = c("x", "x")),
                 "same name")
  expect_equal(attr(p5, "import_report")$result[2], "skipped")
})
