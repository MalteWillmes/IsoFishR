<img src="https://raw.githubusercontent.com/MalteWillmes/IsoFishR/master/www/logo.png" alt="IsoFishR" height="72">

# IsoFishR 2 (development version)

Data reduction and analysis of <sup>87</sup>Sr/<sup>86</sup>Sr ratios measured by laser-ablation MC-ICP-MS, for example along otolith transects. IsoFishR turns instrument runfiles into blank-corrected, Rb-interference- and mass-bias-corrected profiles. You then trim them, mark regions, detect changepoints and export the results, all in an app that runs in your browser.

> **Looking for the original IsoFishR (v1)?** It is still available as the [v1.1 release](https://github.com/MalteWillmes/IsoFishR/releases/tag/v1.1), as described in [Willmes et al. 2018, PLOS ONE](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0204519).

## Installation

You need [R](https://cloud.r-project.org/) (4.1 or newer). [RStudio](https://posit.co/download/rstudio-desktop/) is optional. In R, run:

```r
install.packages("IsoFishR", repos = c("https://maltewillmes.r-universe.dev", "https://cloud.r-project.org"))
```

Run the same line again to update. No extra build tools are needed on Windows, macOS or Linux.

## Start the app

```r
IsoFishR::run_app()
```

The app opens in your web browser. To stop it, close the browser tab, or press Esc in R.

## A five-minute tour

1. **Try the example data.** On the start screen, click **Try with example data**. This opens a temporary project with five Chinook salmon otoliths and one white seabass otolith.
2. **Workspace.** Pick a sample on the left to see its <sup>87</sup>Sr/<sup>86</sup>Sr profile, with total Sr, <sup>83</sup>Kr and <sup>85</sup>Rb/<sup>88</sup>Sr below it.
   - Drag across the plot to select a range. The footer shows its mean ± 2 SE.
   - **Edit:** keep only the selected range (trim), add it as a named region, reverse the profile, or detect changepoints.
   - **Reduction:** change settings (e.g. blank time or smoothing) for this sample only.
   - **Notes:** set the status (new, reviewed, excluded), flag the sample, and add a comment. "Mark reviewed and go to next" steps through samples quickly.
3. **Export.** Writes summary tables and every profile point by point to a dated folder inside the project.

Everything is saved as you work. Close the app at any time, then reopen the project later to continue or add more samples.

## Your own data

1. **Create a project** on the start screen. A project is a folder you choose, e.g. in Dropbox or on a shared drive, and it holds everything about a set of samples.
2. **Choose the instrument preset** describing your runfile layout. `Nu_UCDavis` (Nu Plasma, the default) is built in. For another layout, set it under **Project settings > Runfile import**, check it with **Check the layout with a runfile**, and **save it as a preset** to reuse it in other projects.
3. **Set the reduction defaults** under **Project settings**: run speed, integration, blank time, 88Sr limits, outlier rejection and smoothing.
4. **Add runfiles** in the Workspace. You can add more at any time, for example after the next session at the instrument.

### Reference materials and drift correction

IsoFishR can correct samples to a reference material measured during the session, in the style of Iolite. Set this up on the **Calibration** page:

1. **Define the reference materials** with their accepted <sup>87</sup>Sr/<sup>86</sup>Sr and ± 2σ.
2. **Mark the runs.** The Runs tab lists all runs in acquisition order, read from the runfile timestamps. Select runs and set them to *Sample*, *Baseline* or a reference material.
3. **Choose the primary reference material.** Its runs are used for the correction; all other reference materials are secondary and serve as accuracy checks.
4. **Choose how the correction follows time:**
   - linear between the bracketing standards (default)
   - smoothing spline
   - step forward
   - session mean
5. **Optionally use baseline runs** (gas blanks between analyses) instead of the blank at the start of each run. They are interpolated in time to each run.

**How the correction is applied.** The factor *accepted / measured* of the primary runs is fitted through time within each analytical session. A session starts automatically after a gap of more than 6 hours, and runs can be moved to another session by hand. The factor is applied to every point of every sample at the time that point was measured.

**What is reported.** Each sample keeps its own SE. The uncertainty of the correction, from the primary measurements and the accepted value's uncertainty, is reported separately and in a combined SE. The **Correction** tab plots the primary factors with the fitted curve and the secondary deviations in ppm. **Reference material results** lists every standard run with its deviation from the accepted value.

Without a primary reference material, values are left uncorrected, exactly as before.

### What is in a project folder

```
MyProject/
  project.yml   project settings, including the import preset
  raw/          copies of the original runfiles (never changed)
  samples.csv   per run: type, acquisition time, session, status, flag, comment, trims, setting overrides
  regions.csv   regions per sample
  reference_materials.csv   reference materials with accepted values
  log.csv       every change: when, who, what, old and new value
  cache/        reduced data (safe to delete; rebuilt automatically)
  exports/      one dated folder per export
```

Projects can be shared by sharing the folder. Only one person can edit a project at a time; others can open it read-only.

### Exports

Each export creates `exports/<date_time>/` containing:

| File | Contents |
|---|---|
| `profiles.csv` | Every point of every profile in one table: distance, time, <sup>87</sup>Sr/<sup>86</sup>Sr, smoothed value and SE, region, changepoint segment, and signal intensities |
| `samples.csv` | One row per sample: type, session, status, comment, edits, profile mean, SD and SE (and, when corrected, the uncorrected mean, correction factor and combined SE) |
| `regions.csv` | One row per sample and region: mean, SD, n, SE, distance range |
| `changepoints.csv` | One row per changepoint segment |
| `runs.csv` | All runs in acquisition order with type and session |
| `reference_materials.csv` | Every reference material run: measured and corrected mean, deviation from the accepted value (ppm), whether it is within uncertainty |
| `reference_material_summary.csv` | Per reference material and session: runs, mean, 2 SD, deviation |
| `settings.csv` | Every setting each sample was reduced with, including the calibration |
| `export_info.yml` | Date, user, IsoFishR version |

## Using IsoFishR from R code

All of the app's functions are available in R scripts, for example:

```r
library(IsoFishR)
p <- create_example_project()
p <- set_regions(p, "PC_2016_01_A2_4R",
                 data.frame(region = "core", start_s = 200, end_s = 260))
summarise_project_regions(p)
export_project(p)
close_project(p)
```

## What changed from v1

- **One workspace:** data reduction and analysis share a single screen, and you can move back and forth freely.
- **Non-destructive edits:** trims, regions and setting changes are stored as choices, not baked into the data, so they can always be undone or changed.
- **Change log:** every choice is recorded in the project's change log.
- **Projects anywhere:** projects live in any folder, not inside the app.
- **Faster:** reduction is several times faster, and only samples whose data or settings changed are reprocessed.
- **v1 bugs fixed:**
  - Integration blocks no longer span two runfiles.
  - The right-hand trim no longer applies to every sample.
  - Changepoint means are no longer pooled across samples.
  - Spline smoothing now works.
  - The moving-average SE now uses the points actually in each window.
- **SE from individual points:** region and sample statistics are calculated from the individual points rather than smoothed values, so the reported SE is no longer underestimated.
- **No PDF plots:** per-sample PDF plots are no longer produced. Summaries and full profiles are exported as CSV instead.

Details are in [`dev/DESIGN.md`](dev/DESIGN.md).

## Citation

Willmes M, Ransom KM, Lewis LS, Denney CT, Glessner JJG, Hobbs JA (2018). IsoFishR: An application for reproducible data reduction and analysis of strontium isotope ratios (<sup>87</sup>Sr/<sup>86</sup>Sr) obtained via laser-ablation MC-ICP-MS. *PLOS ONE* 13(9): e0204519. https://doi.org/10.1371/journal.pone.0204519

## License

MIT. Questions and bug reports: [GitHub issues](https://github.com/MalteWillmes/IsoFishR/issues).
