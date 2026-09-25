# airquality

Analysis repository for a systematic compilation, evaluation and documentation of air quality in the
Canton of Zurich (AWEL), based on public data. It reads input metadata (`data/meta/`),
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
* Per sub-analysis: regression reference → tests first (testthat 3e, synthetic data, no network) → pure
  functions in `R/<topic>.R` → targets in `pipelines/<sub-analysis>.R` → regression byte-identical
  (`tests/regression/run_pipeline.R`) → document in `notes/` → commit.
* **Before working on a topic, read its file in `notes/`** (table below). Record new decisions and
  findings there, with numbers; keep this file to a summary.
* Code, comments, roxygen and messages in English; reports and plots in German.
* tidyverse style, native pipe, `.by =`, `join_by()`, `purrr::map_*()`, `cli` for messages/errors.
  Only functions exported by `airquality.methods` carry the `airquality.methods::` prefix.
* No package: `R/` is sourced (`tar_source()`, the test helper, the plot setup); roxygen comments are
  in-code documentation only. Tests: `testthat::test_dir("tests/testthat")`. A package used anywhere must
  be in DESCRIPTION (`test-dependencies.R`); renv snapshots it explicitly. After (re)installing
  `airquality.methods`, restart the R session (decision 1).

## Structure

A `targets` project (phase 2b, `notes/plan_phase2b.md`), renv-managed. Not an installable package.

| Path | Content |
|---|---|
| `run.R` | entry point: `tar_make()` of the outputs, optionally `wip/trends.R`, then `tar_make()` of the report |
| `_targets.R` | sources `R/`, `settings.R` and `pipelines/`; combines the target lists `pipeline_*` |
| `settings.R` | **all analysis settings and paths** (decisions 7 and 8), pure assignments; `path_output`/`path_log` from `AIRQUALITY_OUTPUT_DIR`/`AIRQUALITY_LOG_DIR` |
| `pipelines/` | one target list per sub-analysis: `setup`, `emissions_emikat`, `emissions_rsd`, `monitoring_airquality`, `monitoring_ndep`, `exposition_population`, `exposition_ecosystems`, `outcomes`, `report`; target names `<topic>_<sub-analysis>_<stage>`, outputs as `format = "file"` targets (`*_out*`); only the version states of the downloads (`*_state`, opendata.swiss) and of the raster assets (`*_assets`) run with `tar_cue("always")`, the downloads and raster reads only when the state changed; the geolion municipality map (no metadata) is read on every run |
| `R/` | analysis-specific functions, one file per topic (`exposition.R`, `emissions.R`, `monitoring.R`, `outcomes.R`, `helpers.R`, `pipeline.R`; plots: `plot.R` shared, `plot_<topic>.R`); `prepare.R`, `aggregate.R` hold only trend code (WIP) |
| `wip/` | work in progress outside the pipeline: `trends.R` (about 30 min), see `wip/README.md` |
| `report/` | Quarto sources (`*.qmd`, `_quarto.yml`, `styles.css`, `year-slider.html`); plots per year as year sliders (decision 11); built by the target `report_site` |
| `report/plots/_plot_setup.R`, `_plot_<topic>.R` | plots from the output CSVs, one script per report topic, each delivering a plot catalog `plots_<topic>` (`get_plot()`); presentation settings in `_plot_setup.R`; sourced by the Quarto pages, usable in the console (decision 9) |
| `report/plots/_plot_airquality.R` | sources `_plot_setup.R` and all topic plot scripts (interactive use) |
| `docs/` | rendered website only (GitHub Pages, `output-dir: ../docs`) |
| `data/meta/` | input metadata (resources, thresholds, RSD filters, subsector lookup, …) |
| `data/output/` | **output CSVs – the contract with external processes** |
| `data/log/` | run logs, appended when the logged values change; not part of the contract |
| `data/restricted/` | non-public inputs (mortality), gitignored except its README |
| `tests/testthat/` | unit tests, the output schema test and the dependency test |
| `tests/regression/` | frozen baseline outputs, the pipeline on frozen inputs (`run_pipeline.R`), generic comparison; figures (`run_plots.R`), pages without rendering (`check_pages.R`) |
| `notes/` | decisions, findings per topic and plans in detail (read on demand) |

Workflow: `source("run.R")` or `targets::tar_make()` (one sub-analysis: `tar_make(names =
starts_with("emis_rsd_"))`); inspect with `tar_visnetwork()`, `tar_read()`, `tar_load()`, after an error
`tar_workspace()`. The store `_targets/` is gitignored. Rendering the report alone with `shortcut = TRUE` needs
`names = c("report_sources", "report_site")`, otherwise it is skipped (see `run.R`).

## Output contract

The 14 CSV files in `data/output/` are read by external processes. File names, column names,
column order and format must not change (the directory may change; external paths can be adjusted):

* delimiter `;`, UTF-8, one header line, `NA` for missing values, full numeric precision
  (`airquality.methods::write_local_csv()`)
* `data_exposition_weighted_means_*.csv` use the Greek letter `μ` (U+03BC) in `unit`; other files use
  the micro sign `µ` (U+00B5). Historical inconsistency, kept on purpose – do not "fix" silently.
* `tests/testthat/test-output-schema.R` checks names and column order against
  `tests/regression/baseline/` (outputs as of commit `c0cb59c`).

## Status (2026-09-24)

* **Step 1 done**: exposition reworked.
* **Step 2, phase 2a done** except the trends: emissions, monitoring, outcomes reworked; plots/report
  done for all five topics (`notes/findings_plots.md`). The trends stay WIP on the user's decision.
  **Each topic only after the user's go.**
* **Outcomes reworked** (2026-09-24; plan, decisions E1–E6 and numbers in `notes/plan_outcomes.md` and
  `notes/findings_trends_outcomes.md`): `R/outcomes.R` + `pipelines/outcomes.R`; premature deaths from the deaths
  aged ≥ 30 (E1), deterministic estimate and range from the RR bounds (E2); years of life lost with the
  healthiar life table as new rows of `data_health_outcomes.csv` (same columns); page with tabsets for
  deaths and years of life lost (the long-term mean per premature death in the subtitle). Reduction of
  life expectancy not implemented (only with an official life table).
* **Phase 2b (targets) done** (2026-09-24; plan and status in `notes/plan_phase2b.md`): data in `data/`,
  Quarto sources in `report/`, one pipeline per sub-analysis incl. the outcomes and the report, package
  skeleton and `scripts/` removed. On frozen inputs 11 of 12 pipeline outputs are byte-identical to the
  old scripts, the health outcomes within 4.5e-11 (no CSV round trip of the canton means). Left: the
  trends as sub-analysis once they are finished (plan step 7).
* All content changes so far are in `data/output/`: emissions, monitoring and ndep since commit
  `fc1e744`, health outcomes since `b9fe256` (last digits since the pipeline run of 2026-09-24).

## Decisions (summary; details, reasons and numbers in `notes/decisions.md`)

Guiding lesson: **make every step visible and recomputable** – no communication through globals, no
appending to own outputs, no work lists derived from earlier results.

1. `airquality.methods` 0.5.1 is installed into renv from GitHub (`awelZH/airquality.methods`, pinned
   sha in `renv.lock`); restart R after reinstalling.
2. Exposition is recomputed completely on every run; downloads are cached, uncompressed GeoTIFFs are
   streamed from the web every time.
3. The cell table (one row per inhabited 100 m STATPOP cell and year) is the unit of work for
   exposition.
4. Canton and municipality results come from the same cells; every cell gets exactly one
   municipality (exclaves included, Kloster Fahr removed, lake cells to the nearest municipality).
5. STATPOP collector pixels are subtracted and spread over their municipality in proportion to the
   located inhabitants.
6. Derived parameters (O3 peak season from NO2, PM2.5 before 2015 from PM10) are refitted on every
   run; coefficients are logged in `data/log/`. The O3 model keeps one common slope for all
   years (user decision 2026-09-24).
7. All analysis constants live in `settings.R`, prefixed by topic; functions in `R/` get them
   as arguments; every year range ends at `year_last`. The paths live there too (phase 2b decision 8, no
   `config.yml`).
8. Inputs are checked where they enter (`check_columns()`, error class `airquality_input_error`).
9. The report builds its plots while rendering (no rds files); the plot scripts stay usable in the
   console.
10. Generic building blocks live in `airquality.methods` (municipality assignment, collector pixel
    redistribution, grouped legend, `check_names()`; since 0.5.0 the nitrogen deposition classes and the plot
    catalog with `get_plot()` and `print_tabset()`); only analysis-specific code stays in `R/`.
11. Plots per year are shown with a year slider (`print_year_slider()`, `report/year-slider.html`), not tabsets.

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

Pattern for new or remaining sub-analyses: `R/outcomes.R` + `pipelines/outcomes.R`, `R/emissions.R` +
`pipelines/emissions_rsd.R`; tests in `tests/testthat/test-<topic>.R`.

## Open items

* Trends (Wirkungsmonitoring): data scripts and functions still WIP on the user's decision (unseeded
  random forest; the detailed trend plot runs up to the current calendar year, not `year_last`).
