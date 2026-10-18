#' Export record as Markdown
#'
#' Renders [export_report()] as a Markdown document (`export_report.md` in
#' every export): readable in any text editor, rendered on GitHub and in
#' RStudio, and easy to copy into a lab notebook or a methods section.
#'
#' @param report Output of [export_report()].
#' @return A character vector of Markdown lines.
#' @export
report_markdown <- function(report) {
  cal <- report$calibration
  rms <- c(if (!is.null(cal$primary)) list(c(cal$primary, role = "primary")),
           purrr::map(cal$secondary, \(r) c(r, role = "secondary")))

  rm_table <- if (length(rms) > 0) {
    md_table(tibble::tibble(
      `Reference material` = purrr::map_chr(rms, "name"),
      Role = purrr::map_chr(rms, "role"),
      Accepted = purrr::map_chr(rms, \(r) fmt_ratio(r$accepted)),
      `Accepted 2s` = purrr::map_chr(rms, \(r) fmt_small(r$accepted_2s)),
      Runs = purrr::map_chr(rms, \(r) as.character(r$runs)),
      `Mean (uncorrected)` = purrr::map_chr(rms, \(r) fmt_ratio(r$mean_uncorrected)),
      `SD (uncorrected)` = purrr::map_chr(rms, \(r) fmt_small(r$sd_uncorrected)),
      `Mean (corrected)` = purrr::map_chr(rms, \(r) fmt_ratio(r$mean_corrected)),
      `SD (corrected)` = purrr::map_chr(rms, \(r) fmt_small(r$sd_corrected)),
      `Deviation (ppm)` = purrr::map_chr(rms, \(r) fmt_num(r$deviation_ppm, 1)),
      `Runs within uncertainty` = purrr::map_chr(rms, \(r) fmt_count(r$runs_within_uncertainty))
    ))
  } else {
    "No reference materials are defined."
  }

  per_session <- purrr::map(rms, \(r) purrr::map(r$per_session %||% list(), \(s) tibble::tibble(
    `Reference material` = r$name, Session = s$session, Runs = as.character(s$runs),
    `Mean (uncorrected)` = fmt_ratio(s$mean_uncorrected),
    `SD (uncorrected)` = fmt_small(s$sd_uncorrected),
    `Mean (corrected)` = fmt_ratio(s$mean_corrected),
    `SD (corrected)` = fmt_small(s$sd_corrected),
    `Deviation (ppm)` = fmt_num(s$deviation_ppm, 1)
  )) |> purrr::list_rbind()) |> purrr::list_rbind()

  sessions <- purrr::list_rbind(purrr::map(report$sessions, tibble::as_tibble))
  overrides <- report$sample_setting_overrides

  c(
    paste0("# IsoFishR export: ", report$project),
    "",
    paste0("- **Exported:** ", report$exported, " by ", report$exported_by),
    paste0("- **IsoFishR version:** ", report$isofishr_version),
    paste0("- **Samples exported:** ", report$samples,
           if (isTRUE(report$include_excluded)) " (including excluded samples)" else
             " (excluded samples left out)"),
    paste0("- **Files:** ", paste0("`", report$files, "`", collapse = ", ")),
    "",
    "## Runs and sessions",
    "",
    md_table(tibble::tibble(
      Runs = report$runs$total, Samples = report$runs$samples,
      `Reference material runs` = report$runs$reference_material_runs,
      `Baseline runs` = report$runs$baseline_runs, Excluded = report$runs$excluded
    )),
    "",
    if (nrow(sessions) > 0) {
      md_table(dplyr::rename(sessions, Session = "session", Runs = "runs",
                             `First run` = "first_run", `Last run` = "last_run"))
    } else "No runs yet.",
    "",
    "## Calibration",
    "",
    paste0("- **Correction:** ", cal$correction),
    paste0("- **Method:** ", cal$method_description, " (`", cal$method, "`)"),
    paste0("- **Primary reference material:** ", cal$primary$name %||% "none"),
    paste0("- **Secondary reference materials:** ",
           if (length(cal$secondary)) paste(purrr::map_chr(cal$secondary, "name"), collapse = ", ")
           else "none"),
    paste0("- **Baseline:** ", cal$baseline_description),
    paste0("- **New session after a gap of:** ", cal$session_gap_h, " h"),
    "",
    "Means and SDs are of the reference material run means (one value per run).",
    "",
    "### Reference materials",
    "",
    rm_table,
    "",
    if (nrow(per_session) > 0) c("### Per session", "", md_table(per_session), ""),
    "## Settings",
    "",
    settings_markdown(report$settings),
    "## Samples with their own settings",
    "",
    if (length(overrides) > 0) {
      md_table(tibble::tibble(Sample = names(overrides), Settings = unlist(overrides)))
    } else "None: all samples use the project settings.",
    ""
  )
}

# Settings as one table per section, labelled as in the app.
settings_markdown <- function(settings) {
  spec <- settings_spec()
  sections <- c(reduction = "Data reduction", sample = "Samples and laser",
                import = "Runfile import", regions = "Regions")
  purrr::imap(sections, \(title, section) {
    values <- settings[[section]]
    keys <- names(values)
    labels <- spec$label[match(paste(section, keys), paste(spec$section, spec$key))]
    extra <- c(preset = "Instrument preset", columns = "Column numbers",
               default_labels = "Default region names")
    labels <- dplyr::coalesce(labels, unname(extra[keys]), keys)
    shown <- vapply(values, \(v) {
      if (is.list(v)) paste(names(v), unlist(v), sep = " = ", collapse = ", ")
      else if (length(v) == 0 || (length(v) == 1 && is.na(v))) ""
      else paste(v, collapse = ", ")
    }, character(1))
    c(paste("###", title), "", md_table(tibble::tibble(Setting = labels, Value = shown)), "")
  }) |> unlist(use.names = FALSE)
}

md_table <- function(df) {
  df <- dplyr::mutate(df, dplyr::across(dplyr::everything(), \(x) {
    x <- ifelse(is.na(x), "", as.character(x))
    gsub("|", "\\|", x, fixed = TRUE)
  }))
  c(paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
    do.call(paste, c(df, sep = " | ")) |> (\(rows) paste0("| ", rows, " |"))())
}

# Number formats: 87Sr/86Sr to 6 decimals, small spreads with 3 significant
# digits, missing values as "-".
fmt_ratio <- function(x) if (is.null(x) || is.na(x)) "-" else formatC(x, format = "f", digits = 6)
fmt_small <- function(x) if (is.null(x) || is.na(x)) "-" else formatC(x, format = "g", digits = 3)
fmt_num <- function(x, digits) if (is.null(x) || is.na(x)) "-" else formatC(x, format = "f", digits = digits)
fmt_count <- function(x) if (is.null(x) || is.na(x)) "-" else as.character(x)
