#' Create an example project
#'
#' Creates a project from the example runfiles that come with IsoFishR: five
#' Chinook salmon otolith transects from Putah Creek, California, and one
#' white seabass otolith, measured on a Nu Plasma MC-ICP-MS at UC Davis. It
#' is a quick way to try the app or the package functions without data of
#' your own.
#'
#' The project uses the default settings and the `"Nu_UCDavis"` import
#' preset, with a run speed of 5 um/s as in the example project of
#' IsoFishR v1.
#'
#' @param path Folder for the new project. By default a new folder in the
#'   R session's temporary directory, which is deleted when R closes.
#' @param user Name recorded in the project log.
#' @return The open project (see [open_project()]).
#' @examples
#' p <- create_example_project()
#' p
#' head(summarise_project_samples(p))
#' close_project(p)
#' @export
create_example_project <- function(path = file.path(tempfile("IsoFishR_"), "Example_project"),
                                   user = default_user()) {
  files <- example_runfile_paths()
  if (length(files) == 0) stop("The example runfiles are missing from the installation.", call. = FALSE)
  settings <- default_settings()
  settings$sample$run_speed_um_s <- 5
  p <- create_project(path, settings = settings, user = user)
  add_runfiles(p, files)
}

example_runfile_paths <- function() {
  list.files(system.file("extdata", "runfiles", package = "IsoFishR"),
             pattern = "\\.run$", recursive = TRUE, full.names = TRUE)
}
