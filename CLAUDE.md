# airquality

Analysis repository for a systematic compilation, evaluation and documentation of air quality in the
Canton of Zurich (AWEL), based on public data. It reads input metadata (`inst/extdata/meta/`),
monitoring data from the data package `airquality.data` and open government data (opendata.swiss,
geolion WFS, data.geo.admin.ch rasters, BFS), and writes CSV files for automated downstream
processing by external processes. A Quarto website in `docs/` documents the results. Updated about
**twice a year** and extended step by step. Generic, reusable functions live in
[`airquality.methods`](../airquality.methods); functions that only make sense here live in `R/`.

## Rules (always)

* **Discuss first, implement only after the user's explicit go.** Present options with quantified
  consequences and a recommendation; methodological questions are decided by the user.
* **Never change the output contract** (below) silently.
* **Deliberate content changes get their own commit**, so refactorings stay byte-identical; check
  refactorings with the regression workflow (`notes/regression.md`). The user says when to commit.
* Per topic: regression reference → tests first (testthat 3e, synthetic data, no network) → pure
  functions in `R/<topic>.R` → thin script → regression byte-identical → document in `notes/` → commit.
* **Before working on a topic, read its file in `notes/`** (table below). Record new decisions and
  findings there, with numbers; keep this file to a summary.
* Code, comments, roxygen and messages in English; reports and plots in German.
* tidyverse style, native pipe, `.by =`, `join_by()`, `purrr::map_*()`, `cli` for messages/errors.
  Only functions exported by `airquality.methods` carry the `airquality.methods::` prefix.
* Run `devtools::document()` after changing roxygen comments. After (re)installing
  `airquality.methods`, restart the R session (decision 1).

## Structure

An RStudio project with a package-like layout (DESCRIPTION for dependencies, `R/` loaded with
`devtools::load_all()`), renv-managed. Not an installable package.

| Path | Content |
|---|---|
| `scripts/analyse_airquality.R` | entry point: sources `_setup.R`, then the `_compile_*.R` scripts in order |
| `scripts/_setup.R` | packages, `load_all()`, sources `_settings.R`, municipality map |
| `scripts/_settings.R` | **all analysis settings** (decision 7), pure assignments |
| `scripts/_compile_*.R` | one script per topic: emissions, monitoring, trends, exposition, outcomes |
| `scripts/_plot_setup.R`, `_plot_<topic>.R` | plots from the output CSVs, one script per report topic, each delivering a plot catalog `plots_<topic>` (`get_plot()`); presentation settings in `_plot_setup.R`; sourced by the Quarto pages, usable in the console (decision 9) |
| `scripts/_plot_airquality.R` | sources `_plot_setup.R` and all topic plot scripts (interactive use) |
| `R/` | analysis-specific functions, one file per topic (`exposition.R`, `emissions.R`, `monitoring.R`, `helpers.R`; plots: `plot.R` shared, `plot_<topic>.R`); `prepare.R`, `aggregate.R`, `read.R`, `*_helpers.R` hold only outcome/trend code (WIP) |
| `inst/extdata/meta/` | input metadata (resources, thresholds, RSD filters, subsector lookup, …) |
| `inst/extdata/output/` | **output CSVs – the contract with external processes** |
| `inst/extdata/log/` | run logs, appended on every run; not part of the contract |
| `docs/` | Quarto website (`quarto::quarto_render("docs/")`); plots per year as year sliders (`year-slider.html`, decision 11) |
| `tests/testthat/` | unit tests (`devtools::test()`) and the output schema test |
| `tests/regression/` | frozen baseline outputs, regression runs on frozen inputs, generic comparison; figures (`run_plots.R`), pages without rendering (`check_pages.R`) |
| `notes/` | decisions, findings per topic and plans in detail (read on demand) |

## Output contract

The 14 CSV files in `inst/extdata/output/` are read by external processes. File names, column names,
column order and format must not change (the directory may change; external paths can be adjusted):

* delimiter `;`, UTF-8, one header line, `NA` for missing values, full numeric precision
  (`airquality.methods::write_local_csv()`)
* `data_exposition_weighted_means_*.csv` use the Greek letter `μ` (U+03BC) in `unit`; other files use
  the micro sign `µ` (U+00B5). Historical inconsistency, kept on purpose – do not "fix" silently.
* `tests/testthat/test-output-schema.R` checks names and column order against
  `tests/regression/baseline/` (outputs as of commit `c0cb59c`).

## Status (2026-09-24)

* **Step 1 done**: exposition reworked.
* **Step 2, phase 2a in progress**: emissions, monitoring done; plots/report done for all five
  topics, including a second round (plot catalog, year slider, logic out of the pages, consistent
  names; figures byte-identical, `notes/findings_plots.md`). The data scripts and functions of
  trends are still WIP, untouched on the user's decision. **Each topic only after the user's go.**
* **Outcomes rework in progress** (user's go 2026-09-24; plan and decisions in `notes/plan_outcomes.md`):
  step 3a done – premature deaths refactored into `R/outcomes.R` + thin script with the old behaviour
  (deaths per year identical to the old code; the old Monte-Carlo range is unseeded). Next: E1 (deaths
  from age 30 only, suppressed cells = 2, summed without the population join), E2 (deterministic
  range), years of life lost (healthiar life table), plots/page.
* **Phase 2b (targets) not started**; plan in `notes/plan_phase2b.md`.
* All content changes so far (subsector grouping of the emissions, the two monitoring fixes, the ndep
  `datasource` clean-up) are in `inst/extdata/output/` since commit `fc1e744`.

## Decisions (summary; details, reasons and numbers in `notes/decisions.md`)

Guiding lesson: **make every step visible and recomputable** – no communication through globals, no
appending to own outputs, no work lists derived from earlier results.

1. `airquality.methods` 0.4.0 is installed into renv from the local repo; restart R after reinstalling.
2. Exposition is recomputed completely on every run; downloads are cached, uncompressed GeoTIFFs are
   streamed from the web every time.
3. The cell table (one row per inhabited 100 m STATPOP cell and year) is the unit of work for
   exposition.
4. Canton and municipality results come from the same cells; every cell gets exactly one
   municipality (exclaves included, Kloster Fahr removed, lake cells to the nearest municipality).
5. STATPOP collector pixels are subtracted and spread over their municipality in proportion to the
   located inhabitants.
6. Derived parameters (O3 peak season from NO2, PM2.5 before 2015 from PM10) are refitted on every
   run; coefficients are logged in `inst/extdata/log/`.
7. All analysis constants live in `scripts/_settings.R`, prefixed by topic; functions in `R/` get them
   as arguments; every year range ends at `year_last`.
8. Inputs are checked where they enter (`check_columns()`, error class `airquality_input_error`).
9. The report builds its plots while rendering (no rds files); the plot scripts stay usable in the
   console.
10. Generic building blocks live in `airquality.methods` (municipality assignment, collector pixel
    redistribution, grouped legend, `check_names()`); only analysis-specific code stays in `R/`.
11. Plots per year are shown with a year slider (`print_year_slider()`, `docs/year-slider.html`), not tabsets.

## Read before working on …

| Topic | File |
|---|---|
| any decision above | `notes/decisions.md` |
| exposition | `notes/findings_exposition.md` |
| emissions (incl. RSD, subsector grouping) | `notes/findings_emissions.md` |
| monitoring (air quality, nitrogen deposition) | `notes/findings_monitoring.md` |
| plots, legends, report | `notes/findings_plots.md`, decision 9 |
| trends, health outcomes | `notes/findings_trends_outcomes.md`; plan of the outcomes rework `notes/plan_outcomes.md` |
| refactoring checks | `notes/regression.md` |
| phase 2b / targets | `notes/plan_phase2b.md` |
| functions of `airquality.methods` | `../airquality.methods/CLAUDE.md` |

Pattern for the remaining topics: `R/emissions.R` + `scripts/_compile_emission_data.R`,
`R/monitoring.R` + `scripts/_compile_monitoring_data.R`.

## Open items

* `airquality.methods`: its `CLAUDE.md` note from step 1 (double counting, not a data-vintage effect)
  is still uncommitted there; the user decides.
* Methodological question: year-specific O3 peak-season slope (decision 6).
