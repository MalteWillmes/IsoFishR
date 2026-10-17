# IsoFishR v2 design

The original app (v1) is preserved at tag `v1.1` / branch `v1`. v2 is developed on `v2-dev`.

## Goals
1. Tidyverse code throughout.
2. One workspace for data reduction and analysis: move back and forth freely, return to a project later and add samples.
3. A file that records every choice the user makes.
4. Fast reduction.
5. Fix the v1 calculation bugs.
6. Project folders chosen by the user, stored outside the app.

## Core idea
v1 saved a snapshot after each stage (raw → reduced → analysed CSVs), and analysis edits were written permanently into the data, so going back meant losing work. In v2 only two things are stored:

- the **raw runfiles**, which are never modified, and
- the **choices for each sample**: setting overrides, trim, reverse, regions, changepoints, comments and flags.

Everything else (reduced data, region statistics, plots, exports) is recalculated from those two and cached.

## Project folder
```
MyProject/
  project.yml   project defaults and IsoFishR version (read_settings / write_settings)
  raw/          original runfiles, copied in and never changed
  samples.csv   one row per sample: ID, runfile, date added, status, flag, comment, setting overrides
  regions.csv   sample_id, region, start_s, end_s
  log.csv       append-only record of every change: time, user, sample, setting, old, new
  cache/        reduced data per sample, keyed by a hash of the raw file and settings; safe to delete
  exports/      generated tables and plots, with the settings used for each sample
  .lock         stops a second user editing at the same time (e.g. a shared Dropbox folder)
```
Trims and regions are stored in **seconds of analysis time** and shown as distance, so they stay in the right place if the run speed is corrected later.

### Project API (implemented)
| Function | What it does |
|---|---|
| `create_project(path)` / `open_project(path)` / `close_project(p)` | Create or open a folder; opening takes the lock (`read_only = TRUE` skips it, `force = TRUE` takes it over) |
| `add_runfiles(p, paths)` | Check the files against the import settings, copy them to `raw/` and register them. Identical files are skipped; clashing IDs are an error |
| `update_sample(p, id, ...)` | status (`new` / `reviewed` / `excluded`), flag, comment, trims, reverse, rezero, changepoint settings |
| `set_sample_settings(p, id, ...)` | Per-sample reduction overrides (`NULL` resets to the project value) |
| `set_regions(p, id, regions)` | Replace a sample's regions |
| `update_project_settings(p, overrides)` | Change the project defaults |
| `project_data(p)` / `project_regions(p)` | Cached reduction with each sample's own settings and edits applied |
| `remove_samples(p, ids)` | Remove samples with their regions and cache. The runfile moves to `raw/removed/` (never deleted) and the removal is logged. To keep a sample but leave it out of exports, set its status to `excluded` instead |
| `read_log(p)` | The change log |

Every change is written to disk immediately, and every changed value is logged with its old and new value. Each function returns the updated project object, which the Shiny app will keep in a reactive value.

`samples.csv` columns: `sample_id, runfile, source, md5, added, added_by, status, flag, comment, trim_start_s, trim_end_s, reverse, rezero, cp_enabled, cp_method, cp_penalty, settings`. The `settings` column holds overrides as `key=value; key=value`.

Cache files are named `<hash of sample ID>_<hash of md5 + import + reduction settings + run speed + IsoFishR version>.rds`. Changing one sample's settings re-reduces only that sample.

The lock is advisory: Dropbox sync is not instant, so two people opening the same project within seconds of each other can still clash. A session whose lock was taken over refuses further writes.

## App (`run_app()`)
- **Start screen:** open a project folder (Browse uses the native folder dialog; recent projects are listed) or create one, and enter the name recorded in the log. If another user holds the lock, the app offers read-only access or a takeover.
- **Workspace:**
  - **Left:** sample table with status and flag, search, Add runfiles, and previous/next buttons.
  - **Centre:** interactive plotly profile with the smoothed line, uncertainty band, outliers, region shading, changepoint means, an optional seawater line, and total Sr, 83Kr and 85Rb/88Sr strips. Dragging selects a distance range and the footer shows its mean ± 2 SE.
  - **Edit tab:** keep only the selected range (trim), trims in µm, rezero, reverse, add/remove regions from the selection with a region statistics table, and changepoints.
  - **Reduction tab:** this sample's settings, with "Apply to this sample" and "Use project settings".
  - **Notes tab:** status, flag, comment, "Mark reviewed and go to next", sample details and history.
- **Project settings page:** reduction, sample/laser and runfile import defaults, plus default region names.
- **Change log page:** filterable `log.csv`.
- Edits save immediately. Text and number fields save when they lose focus, and a field is only written if its value differs from the stored one, so switching samples never writes anything.
- Each open project has its own lock token, so several browser tabs in one R process can't release each other's lock. The app stops once no tab has been connected for 5 seconds (a page reload does not stop it).

## Instrument import presets
A preset is a named runfile layout: header/footer lines, separator, and which column holds each mass and the time.
- **Built-in:** `Nu_UCDavis` (`inst/presets/Nu_UCDavis.yml`) is the default and matches `default_settings()$import`. Built-in presets cannot be overwritten or deleted.
- **User presets:** saved as YAML files in `import_preset_dir()`, which is the user's IsoFishR config folder, or the folder set by the `isofishr.preset_dir` option (e.g. a shared lab folder). They are shared by copying the file.
- **Functions:** `list_import_presets()`, `get_import_preset()`, `save_import_preset()`, `delete_import_preset()`, and `create_project(..., import_preset = )`.
- **In projects:** a project stores the preset name in `import.preset` plus a full copy of the values, so it still works on a computer without that preset, or after the preset is deleted. Changing only the preset name does not invalidate the cache.
- **In the app:**
  - **Start screen:** choose a preset when creating a project.
  - **Project settings > Runfile import:** choose a preset (loads its values into the form), see whether the form still matches it, save the current layout as a new preset, or delete a user preset.
  - **"Check the layout with a runfile":** reads a file with the unsaved layout and reports the cycle count, time range and maximum 88Sr. It warns if the time doesn't increase, 88Sr is near zero, or 86Sr/88Sr looks like swapped columns.

## Exports
`export_project()` (and the app's Export page) writes summary tables to `exports/<date_time>/` and keeps earlier exports. Per-sample PDF plots are no longer produced.

| File | Contents |
|---|---|
| `samples.csv` | One row per sample: source file, status, flag, comment, trims (µm), reverse, n points/outliers, profile mean/SD/SE, mean of smoothed values, number of regions and changepoint segments, setting overrides |
| `regions.csv` | One row per sample and region: mean, SD, n, SE, mean of smoothed values, distance range, start/end time |
| `changepoints.csv` | One row per segment for samples with changepoints enabled: mean, SD, n, SE, distance range, method, penalty |
| `settings.csv` | Every setting each sample was reduced with (project defaults plus overrides) |
| `data.csv` | Optional: every reduced point |
| `export_info.yml` | Project, date, user, IsoFishR version, options |

Statistics come from the individual points (outliers removed), not the smoothed values. v1 used the smoothed values, whose SE understates the uncertainty because neighbouring values are autocorrelated. The app's selection summary and region table use the same calculation, so what you see matches the export. Values are rounded on export (means 6 decimals, SD/SE 7, distances/times 0.1). Excluded samples are left out unless requested.

## Package layout
| File | Contents |
|---|---|
| `R/settings.R` | Default settings, validation, YAML input/output, v1 `_settings.csv` import |
| `R/import.R` | `read_runfiles()` |
| `R/reduce.R` | `reduce_runs()`: integration → blank → thresholds → distance → Rb and mass bias correction → outliers → smoothing |
| `R/roll.R` | O(n) rolling mean, SD and median |
| `R/edits.R` | `apply_edits()` (trim, reverse), `summarise_regions()`, `find_changepoints()` |
| `R/project.R` | Create/open/close, lock, log |
| `R/project-samples.R` | Add runfiles, sample edits, per-sample settings, regions, project settings |
| `R/project-data.R` | Cached per-sample reduction, `project_data()`, `project_regions()` |
| `R/project-io.R` | CSV/YAML reading and writing (fixed column types, atomic writes) |
| `R/app.R` | `run_app()`, start screen, lock conflicts, session handling |
| `R/app-workspace.R` | Workspace module (sample table, plot, Edit / Reduction / Notes) |
| `R/app-plot.R` | `profile_plot()` |
| `R/app-project.R` | Project settings and change log pages |
| `R/app-settings.R` | Settings form definitions shared by the project and sample forms |
| `R/app-utils.R` | Folder dialog, recent projects, error notifications |
| `R/export.R` | Summary tables and `export_project()` |
| `R/app-export.R` | Export page |
| `R/presets.R` | Instrument import presets (`inst/presets/` holds the built-in ones) |

## v1 bugs fixed (each has a regression test)
- Integration blocks spanned two runfiles (`rollapply` ignored the sample grouping).
- The right trim was applied to every sample (`&`/`|` precedence in the trim filter).
- Changepoint segment means were pooled across samples (grouped by segment number only).
- Spline smoothing never reached the analysis (`"Spline"` vs `"spline"`).
- The moving-average SE always divided by the full window width, even at the ends of a profile and with missing values.
- 84Sr/86Sr was not corrected for mass bias (added as `sr84_sr86_mb`; `sr84_sr86` is kept).

## Other changes from v1
- "No outlier rejection" is `outlier_sd: 0`.
- Column names are snake_case (`sr87_sr86`, `distance_um`, …). Exports can map them back to v1 names if needed.
- A sample with no blank cycles produces a warning and is left uncorrected (v1 returned all `NA`).

## Comparison with v1
Run `Rscript dev/compare_v1.R`. Where v1 and v2 produce the same integrated cycle, the ratios are identical to floating-point precision. Sample means differ slightly (about 5–15 ppm for the salmon profiles, about 80 ppm for the short seabass profile) because v1 groups the cycles into integration blocks at different boundaries, which changes which edge blocks pass the 88Sr threshold.

## Next steps
1. ~~Project folder functions~~ (done).
2. ~~Shiny workspace~~ (first version done).
3. ~~Exports~~ (done; no PDFs and no v1 project importer, by decision). ~~Removing samples~~ (done).
4. App polish once used with real data: keyboard shortcuts, a whole-project overview plot, layout on small screens.
