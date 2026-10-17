# Settings forms for the app. One table describes every editable setting, so
# the project settings page and the per-sample Reduction panel are generated
# from the same definitions.

settings_spec <- function() {
  tibble::tribble(
    ~section,    ~key,               ~label,                              ~type,     ~choices,
    "reduction", "integration",      "Integration (cycles per point)",    "integer", NULL,
    "reduction", "blank_correction", "Blank correction",                  "logical", NULL,
    "reduction", "blank_time_s",     "Blank time (s)",                    "numeric", NULL,
    "reduction", "raw88_min_v",      "Min 88Sr (V)",                      "numeric", NULL,
    "reduction", "raw88_max_v",      "Max 88Sr (V)",                      "numeric", NULL,
    "reduction", "sr86_sr88_ref",    "Reference 86Sr/88Sr",               "numeric", NULL,
    "reduction", "rb85_rb87",        "85Rb/87Rb",                         "numeric", NULL,
    "reduction", "outlier_sd",       "Outlier rejection (SD, 0 = off)",   "numeric", NULL,
    "reduction", "outlier_window",   "Outlier window (points)",           "integer", NULL,
    "reduction", "smoothing",        "Smoothing",                         "choice",
      c("Moving average" = "ma", "Spline" = "spline", "None" = "none"),
    "reduction", "ma_window",        "Moving average window (points)",    "integer", NULL,
    "reduction", "spline_k",         "Spline k",                          "integer", NULL,
    "sample",    "run_speed_um_s",   "Run speed (\u00b5m/s)",             "numeric", NULL,
    "sample",    "analysis_type",    "Analysis type",                     "choice",
      c("Line" = "line", "Spot" = "spot"),
    "sample",    "sample_type",      "Sample type",                       "text",    NULL,
    "sample",    "direction",        "Analysis direction",                "text",    NULL,
    "sample",    "spot_size_um",     "Spot size (\u00b5m)",               "numeric", NULL,
    "sample",    "fluence_j_cm2",    "Fluence (J/cm\u00b2)",              "numeric", NULL,
    "sample",    "laser_energy_pct", "Laser energy (%)",                  "numeric", NULL,
    "sample",    "user",             "User",                              "text",    NULL,
    "import",    "skip_header",      "Header lines to skip",              "integer", NULL,
    "import",    "skip_footer",      "Footer lines to skip",              "integer", NULL,
    "import",    "sep",              "Separator",                         "choice",
      c("Comma" = ",", "Tab" = "\t", "Semicolon" = ";"),
    "import",    "header",           "Data has column headings",          "logical", NULL,
    "import",    "timestamp_line",   "Line with the acquisition date/time (0 = none)", "integer", NULL,
    "import",    "timestamp_format", "Date/time format (R strptime)",     "text",    NULL
  )
}

COLUMN_KEYS <- c(raw88 = "88Sr column", raw87 = "87 column", raw86 = "86Sr column",
                 raw85 = "85Rb column", raw84 = "84Sr column", raw83 = "83Kr column",
                 time_s = "Time (s) column")

setting_input_id <- function(section, key) paste0("set_", section, "_", key)

# One input per setting; `values` is the settings section to show.
setting_input <- function(ns, section, key, label, type, choices, value) {
  id <- ns(setting_input_id(section, key))
  switch(type,
    integer = shiny::numericInput(id, label, value, min = 0, step = 1),
    numeric = shiny::numericInput(id, label, value),
    logical = shiny::checkboxInput(id, label, isTRUE(value)),
    choice = shiny::selectInput(id, label, choices, selected = value),
    text = shiny::textInput(id, label, value)
  )
}

settings_inputs <- function(ns, settings, sections) {
  spec <- dplyr::filter(settings_spec(), .data$section %in% sections)
  purrr::pmap(spec, \(section, key, label, type, choices) {
    setting_input(ns, section, key, label, type, choices, settings[[section]][[key]])
  })
}

column_inputs <- function(ns, columns) {
  purrr::imap(COLUMN_KEYS, \(label, key) {
    shiny::numericInput(ns(paste0("set_import_col_", key)), label, columns[[key]], min = 1, step = 1)
  })
}

# Read the inputs of the given sections back into a nested settings list.
settings_from_inputs <- function(input, settings, sections) {
  spec <- dplyr::filter(settings_spec(), .data$section %in% sections)
  for (i in seq_len(nrow(spec))) {
    s <- spec$section[i]
    k <- spec$key[i]
    v <- input[[setting_input_id(s, k)]]
    if (is.null(v) || (length(v) == 1 && is.na(v))) next
    settings[[s]][[k]] <- coerce_like(v, settings[[s]][[k]])
  }
  if ("import" %in% sections) {
    for (key in names(COLUMN_KEYS)) {
      v <- input[[paste0("set_import_col_", key)]]
      if (!is.null(v) && !is.na(v)) settings$import$columns[[key]] <- as.integer(v)
    }
  }
  settings
}

update_settings_inputs <- function(session, settings, sections) {
  spec <- dplyr::filter(settings_spec(), .data$section %in% sections)
  for (i in seq_len(nrow(spec))) {
    s <- spec$section[i]
    k <- spec$key[i]
    id <- setting_input_id(s, k)
    v <- settings[[s]][[k]]
    switch(spec$type[i],
      integer = ,
      numeric = shiny::updateNumericInput(session, id, value = v),
      logical = shiny::updateCheckboxInput(session, id, value = isTRUE(v)),
      choice = shiny::updateSelectInput(session, id, selected = v),
      text = shiny::updateTextInput(session, id, value = v)
    )
  }
}
