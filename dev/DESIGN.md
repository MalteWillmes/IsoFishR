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

## Package layout
| File | Contents |
|---|---|
| `R/settings.R` | Default settings, validation, YAML input/output, v1 `_settings.csv` import |
| `R/import.R` | `read_runfiles()` |
| `R/reduce.R` | `reduce_runs()`: integration → blank → thresholds → distance → Rb and mass bias correction → outliers → smoothing |
| `R/roll.R` | O(n) rolling mean, SD and median |
| `R/edits.R` | `apply_edits()` (trim, reverse), `summarise_regions()`, `find_changepoints()` |

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
1. Project folder functions: create/open, add runfiles, sample registry, log, cache, lock.
2. Shiny workspace (sample table, plots, Reduction / Edit / Notes panels), with autosave.
3. Exports and a v1 project importer.
