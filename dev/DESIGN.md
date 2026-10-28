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
Trims and regions are stored in **seconds of analysis time** and shown as distance, so they stay in the right place if the run speed is corrected later. Trims (`trim_start_s`, `trim_end_s`) are counted from the run's first and last raw cycle (project format 2), not from the first point passing the 88Sr thresholds, so changing thresholds, blank time or integration doesn't move them; the app shows them as µm cut from the profile (`trims_to_um()` / `um_to_trims()`). Format 1 projects are converted once on opening (`convert_trims_v1()`, logged as `convert_trims`). Trims are applied inside the reduction (`trim_runs()`, part of the cache key), before outlier rejection and smoothing, so trimmed data never affects them.

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
| `read_log(p)` | The session log (log.csv) |

Every change is written to disk immediately, and every changed value is logged with its old and new value. Each function returns the updated project object, which the Shiny app will keep in a reactive value.

`samples.csv` columns: `sample_id, runfile, source, md5, added, added_by, status, flag, comment, trim_start_s, trim_end_s, reverse, rezero, cp_enabled, cp_method, cp_penalty, settings`. The `settings` column holds overrides as `key=value; key=value`.

Cache files are named `<hash of sample ID>_<hash of md5 + import + reduction settings + run speed + IsoFishR version>.rds`. Changing one sample's settings re-reduces only that sample.

The lock is advisory: Dropbox sync is not instant, so two people opening the same project within seconds of each other can still clash. A session whose lock was taken over refuses further writes.

## App (`run_app()`): six steps
`app_ui` is a static `bslib::page_navbar(id = "step")` with one panel per step (`APP_STEPS`): `project`, `settings`, `runfiles`, `calibration`, `workspace`, `export`. **Session log** (a modal) and **Quit IsoFishR** sit on the right of the navigation bar on every step.
- **Navigation:** all steps but Project are hidden (`nav_hide`) until a project is open. On opening, the project goes to its last step (`read_last_step()`, `app_state.yml` in the project folder, written on every step change, not logged); a new project goes to *settings*. Closing a project hides the steps and returns to *project*.
- **Sidebars:** the side panels use `open = "always"`. They are set up while their step is hidden, and would otherwise start collapsed.
- **Project:** the open-project card (runs by type, "Continue to <last step>", Close), user name, open (Browse, recent projects), create (preset). If another user holds the lock, the app offers read-only access or a takeover.
- **Settings:** reduction, sample/laser and runfile import defaults (preset, layout check, extension), default region names.
- **Runfiles** (`runfiles_server`): keep/replace for same-name runs (replace clears the run's edits, per-sample settings and regions, keeps run type and session); file upload (`shiny.maxRequestSize` raised to 4 GB in `run_app()`); folder import (`choose_folder()` + `find_runfiles()`; same-name files in different subfolders are flagged and skipped); runs table in acquisition order with multi-select removal. After adding, it points to runs named after a default reference material. `add_runfiles()` checks each file on its own and never stops a batch: the per-file result (added/replaced/skipped/failed + detail) is the `import_report` attribute, shown as a modal. `read_runfile()` marks each line as data or not (`data_lines()`); if the data doesn't start right after `skip_header` lines, it uses the first block of ≥5 data lines (`data_start()`) and reports the line.
- **Look:** the default Bootstrap 5 theme (`app_theme()`), which bslib ships precompiled; colour and compact spacing come from CSS variables in `app_css`. A customised `bs_theme()` is compiled with Sass at startup and added 1-2 s (up to 5 s on first use). Step tabs are not numbered. The Windows folder dialog is the Explorer-style picker (`inst/windows/FolderPicker.cs`, compiled by PowerShell `Add-Type`), with the old folder tree as fallback.
- **Calibration** (`calibration_server`):
  - Suggestion banner per default reference material with matching run names (`default_reference_materials()`: name, regex pattern and value; SBO and GSFR 0.70918, RF (`(^|[^a-z])rf`) and NanoSr (`nano|srn`) without a default value, which the user must enter; `apply_reference_suggestion()`, value editable).
  - Reference material editor; primary; fit (`FIT_CHOICES`, session mean first and default); baseline mode; session gap.
  - Tabs: *Assign runs* first (types and sessions; the selection clears after each change), then *Reference materials* (`reference_material_plot()` + uncorrected/corrected summary), *Run details*.
- **Workspace** (`workspace_server`): mouse-wheel zoom (`scrollZoom`); custom 87Sr/86Sr lines per project in `app_state.yml` (`parse_ratio_lines()`, `read_app_state()`/`write_app_state()`, not logged); sample table with type; plotly profile with Sr, Kr and Rb panels at 40/20/20/20 % height; a corrected/uncorrected badge; Edit / Reduction / Notes panels. The Reduction panel shows only `SAMPLE_PANEL_KEYS` (run speed, 88Sr limits, outlier SD, moving average window); per-sample overrides can be any reduction setting or the run speed (`SAMPLE_OVERRIDE_KEYS`).
  - Edits save immediately. A field is only written if its value differs from the stored one, so switching samples never writes anything.
  - The plot's data key includes `calibration_inputs()`, so a changed calibration recalculates the plotted data.
- **Export** (`export_server`): one Export button (all files except data.csv, excluded samples left out), previews.
- **Locks and stopping:** each open project has its own lock token. The app stops on Quit, or once no tab has been connected for 5 seconds; closing or reloading the tab asks first.
- **Calibration speed:** `calibration_model()` is cached in memory (keyed by `calibration_inputs()`), so the many `project_data()` calls of the steps don't rebuild it.

### Plot performance
- **Redraws only when needed:** the plot rebuilds only when its data (sample, raw file, settings, edits), its regions or the signal-strips option change. Comments, flags, status changes and the other display checkboxes don't rebuild it. The checkboxes switch traces in the browser through `plotlyProxy` (restyle `visible`). The deduplicated keys are reactiveVals, which ignore identical values.
- **WebGL:** all traces are `scattergl`. They have to be all or nothing, because WebGL traces are drawn above SVG ones.
- **Smaller payload:** values are rounded to the precision shown, hover templates are sent once per trace instead of per point, and unevaluated plot attributes are dropped. For a 1,182-point sample this is 130 KB per redraw instead of 540 KB (`Rscript dev/bench_plot.R`).
- **Not done:** plotly's partial bundles (they need to be downloaded at run time, and the app runs locally anyway).
- **Don't pre-serialise data arrays as JSON:** Shiny sends them as a single string and plotly draws one point.

## Reference material correction (Iolite-style)
- **Sequence:** acquisition time per run, read from the runfile header (import `timestamp_line` and `timestamp_format`; Nu_UCDavis: line 45, `%m/%d/%Y %I:%M:%S %p`). Parsed in the C locale and treated as UTC, so DST never reorders runs. Stored in `samples.csv` as `acquired`, and re-read when the timestamp settings change.
- **Run types** (`samples.csv`): `run_type` is `sample`, `baseline` or `reference`, plus `reference_material`. Reference materials are listed in `reference_materials.csv` (name, value, uncertainty_2s, note). `calibration.primary` names the primary; all others are secondary.
- **Sessions:** a new session starts after a gap of more than `calibration.session_gap_h` (default 6 h). `samples.csv$session` overrides this per run. Baselines and corrections are never interpolated across sessions.
- **Baseline:** `calibration.baseline` is `per_run` (the run's own blank, as before) or `baseline_runs`. In the latter, the blank is the mean voltages of the session's baseline runs (after trims), interpolated linearly to each run's start time and held constant outside the bracketing runs. Without baseline runs in the session, the run falls back to its own blank with a warning.
- **Correction:** each primary run gives measured = mean of its points (trims applied, outliers removed, uncorrected) at its midpoint time, and factor = accepted / measured. The factor is fitted per session with `calibration.fit`:
  - `linear`: approx, held constant at the ends.
  - `spline`: smooth.spline, needs 4 or more runs, otherwise linear with a note; held constant outside the standards.
  - `step`: the most recent primary run, compared by start time, so a primary run is corrected only by its own factor.
  - `mean`: the session mean.

  The factor is evaluated at every point's own time (acquired + time_s) and multiplies all 87Sr/86Sr columns. Uncorrected values stay in `sr87_sr86_uncorrected`.
- **Uncertainty** (relative, 1σ): the primary runs' SE/mean propagated through the interpolation weights (`linear`), that of the run used (`step`), sd/√n (`mean`), or residual-based with a floor of the standards' own uncertainty × √(df/n) (`spline`, which otherwise collapses to zero when it passes through every standard). Standards without usable points (all rejected, or one point) are left out of the model. The accepted value's uncertainty is added in quadrature. Reported as `correction_u_rel` (data) and `se_combined` = √(se² + (mean·u)²) (summaries).
- **QC:** `reference_material_results()` gives each standard run's measured and corrected mean and deviation in ppm. Within uncertainty means |dev| ≤ √((2·se_combined)² + accepted_2s²). `summarise_reference_materials()` gives per RM and session the runs, mean, 2 SD and deviation.
- **Off by default:** with no primary set and per-run baselines, `project_data()` returns exactly the previous columns.
- **App:**
  - **Calibration page:** reference material editor; primary, fit, baseline and session-gap settings; the Runs table (multi-select, set type or session; the selection clears after each change because row clicks toggle); the correction plot; and the results tables.
  - **Workspace:** the plot's data key includes `calibration_inputs()`, so the plot follows calibration changes without recomputing on unrelated edits.

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
| `profiles.csv` | Always: every point of every profile in one table (after trims/reversal). Columns: distance, time, the point's 87Sr/86Sr (empty if rejected), the rejected value, the moving average and the spline with their SEs (each filled when the sample's smoothing uses it), region name(s), changepoint segment and mean, total Sr, 83Kr, 85Rb/88Sr, 84Sr/86Sr |
| `data.csv` | Optional: all reduction columns per point (raw voltages, mass bias, intermediate ratios), for checking the reduction itself |
| `export_report.md` | Always: `report_markdown(export_report())`, a readable record (runs, sessions, calibration method, primary and secondary reference materials with runs, mean and SD before and after correction, overall and per session, all settings, per-sample overrides). Missing values are shown as `-` |

Statistics come from the individual points (outliers removed), not the smoothed values. v1 used the smoothed values, whose SE understates the uncertainty because neighbouring values are autocorrelated. The app's selection summary and region table use the same calculation, so what you see matches the export. Values are rounded on export (means 6 decimals, SD/SE 7, distances/times 0.1). Excluded samples are left out unless requested.

## Package layout
| File | Contents |
|---|---|
| `R/settings.R` | Default settings, validation, YAML input/output, v1 `_settings.csv` import |
| `R/import.R` | `read_runfiles()` |
| `R/reduce.R` | `reduce_runs()`: integration → blank → thresholds → distance → Rb (default 85Rb/87Rb = 2.59265, NIST and iolite; v1 used 2.59712) and mass bias correction → trims → outliers (Hampel filter: deviation from the median of the neighbouring `window` cycles, without the cycle itself, over 1.4826 × MAD estimated over ≥201 cycles; about 0.3% false flags on pure noise, and it finds clusters of spikes that a rolling SD misses) → smoothing (`SMOOTHING_METHODS`: none, ma, spline, ma_spline = default). `smooth_spline()`: adaptive penalised spline (`mgcv::bam`, `bs = "ad"`, fREML, weights = total Sr, k = n/10 within 10–80; `tp`/REML below 60 points; falls back to a line if the fit fails); edf in `sr87_sr86_spline_edf`. About 0.2–0.4 s per run, cached. `discrete = TRUE` was tried and rejected: it changed some fits. |
| `R/roll.R` | O(n) rolling mean, SD and median |
| `R/edits.R` | `apply_edits()` (trim, reverse), `summarise_regions()`, `find_changepoints()` |
| `R/project.R` | Create/open/close, lock, log |
| `R/project-samples.R` | Add runfiles, sample edits, per-sample settings, regions, project settings |
| `R/project-data.R` | Cached per-sample reduction, `project_data()`, `project_regions()` |
| `R/project-io.R` | CSV/YAML reading and writing (fixed column types, atomic writes) |
| `R/app.R` | `run_app()`, start screen, lock conflicts, session handling |
| `R/app-workspace.R` | Workspace module (sample table, plot, Edit / Reduction / Notes) |
| `R/app-plot.R` | `profile_plot()` |
| `R/app-project.R` | Project settings and session log pages |
| `R/app-settings.R` | Settings form definitions shared by the project and sample forms |
| `R/app-utils.R` | Folder dialog, recent projects, error notifications |
| `R/export.R` | Summary tables and `export_project()` |
| `R/app-export.R` | Export page |
| `R/runs.R` | Run sequence, sessions, run types, reference material table, acquisition times |
| `R/calibration.R` | Baseline runs, correction model and application, reference material results |
| `R/app-calibration.R` | Step 4, Calibration, and `reference_material_plot()` |
| `R/app-runfiles.R` | Step 3, Runfiles |
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
