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
| `read_log(p)` | The change log |

Every change is written to disk immediately, and every changed value is logged with its old and new value. Each function returns the updated project object, which the Shiny app will keep in a reactive value.

`samples.csv` columns: `sample_id, runfile, source, md5, added, added_by, status, flag, comment, trim_start_s, trim_end_s, reverse, rezero, cp_enabled, cp_method, cp_penalty, settings`. The `settings` column holds overrides as `key=value; key=value`.

Cache files are named `<hash of sample ID>_<hash of md5 + import + reduction settings + run speed + IsoFishR version>.rds`. Changing one sample's settings re-reduces only that sample.

The lock is advisory: Dropbox sync is not instant, so two people opening the same project within seconds of each other can still clash. A session whose lock was taken over refuses further writes.

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
2. Shiny workspace (sample table, plots, Reduction / Edit / Notes panels), with autosave.
3. Exports and a v1 project importer. Also removing samples, which is not supported yet.
