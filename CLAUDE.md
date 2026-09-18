# Aim and context of project

`airquality` is the analysis repository for a systematic, comprehensive compilation, evaluation and
documentation of air quality in the Canton of Zurich (AWEL), based on public data. It reads input
metadata (`inst/extdata/meta/`), pre-compiled monitoring data from the data package `airquality.data`,
and open government data from several online sources (opendata.swiss, geolion WFS, data.geo.admin.ch
rasters, BFS). It restructures, combines, computes and aggregates them and writes the results as CSV
files for automated downstream processing by external processes. A Quarto website in `docs/`
documents the results.

The analysis is updated about **twice a year** with new data points and is extended step by step.

Generic, reusable functions live in the package
[`airquality.methods`](../airquality.methods) (readers for public data sources, raster alignment,
plotting scales). Functions that only make sense in this analysis live here in `R/`.

## What this repo is

An RStudio project with a package-like layout (DESCRIPTION for dependencies, `R/` loaded with
`devtools::load_all()`), renv-managed. Not an installable package.

| Path | Content |
|---|---|
| `scripts/analyse_airquality.R` | entry point: sources `_setup.R`, then the `_compile_*.R` scripts in order |
| `scripts/_setup.R` | packages, `load_all()`, global settings (`year_offset`, `base_scenario_year`, `expo_correct_noloc`, `crs`), municipality map |
| `scripts/_compile_*.R` | one script per topic: emissions, monitoring, trends, exposition, outcomes |
| `scripts/_plot_airquality.R` | builds all plots from the output CSVs, saved as `docs/plots_*.rds` for Quarto |
| `R/` | analysis-specific functions; `R/exposition.R` is the reworked exposition chain |
| `inst/extdata/meta/` | input metadata (resources, thresholds, RSD filters, ...) |
| `inst/extdata/output/` | **output CSVs – the contract with external processes** |
| `inst/extdata/log/` | run logs, appended on every run (e.g. coefficients of derived parameters); not part of the contract |
| `docs/` | Quarto website (`quarto::quarto_render("docs/")`) |
| `tests/testthat/` | unit tests (`devtools::test()`) and the output schema test |
| `tests/regression/` | frozen baseline outputs and the regression comparison |

## Output contract

The 14 CSV files in `inst/extdata/output/` are read by external processes. File names, column names,
column order and format must not change (the directory may change; external paths can be adjusted):

* delimiter `;`, UTF-8, one header line, `NA` for missing values, full numeric precision
  (`airquality.methods::write_local_csv()`)
* `data_exposition_weighted_means_*.csv` use the Greek letter `μ` (U+03BC) in `unit`; other files use
  the micro sign `µ` (U+00B5). Historical inconsistency, kept on purpose – do not "fix" silently.
* `tests/testthat/test-output-schema.R` checks names and column order against
  `tests/regression/baseline/` (outputs as of commit `c0cb59c`).

# Project Decision Log

## The one recurring lesson

**Make every step visible and recomputable.** Scripts that communicate through global variables,
append to their own outputs and derive their work list from earlier results (`get_years()`) are hard
to debug and silently carry old errors forward: the double counting of exclave cells (see below)
survived for years because every run only appended new years.

## Architecture decisions and why

**1. `airquality.methods` 0.4.0 is used from a local installation for now.**
Installed into renv with `renv::install("C:/Users/Public/Git Repos/airquality.methods")`; the lockfile
records a local source. Switch to `awelZH/airquality.methods@<sha>` once 0.4.0 is pushed. The
0.3.0 version from GitHub (sha 355055a) still contained all functions and made the old
`airquality.methods::` prefixes work; with 0.4.0 only its exports may carry the prefix.
**After (re)installing `airquality.methods`, restart the R session** before running `_setup.R`:
`library()` does not reload an already loaded namespace, so `load_all()` would see the old version
and offer to install it (answer "No"). Check with `getNamespaceVersion("airquality.methods")`.
Run `devtools::document()` after changing roxygen comments in `R/` (keeps `NAMESPACE` and `man/` in sync).

**2. Exposition is recomputed completely on every run** (decision 2026-09-18). Downloads are cached by
`airquality.methods` (`geo_admin_cache_dir()`), so a full run is cheap. The four exposition CSVs are
overwritten, not appended. `get_years()` and `read_all_raster` are gone.

**3. The cell table is the unit of work for exposition.** One row per inhabited 100 m STATPOP cell and
year (`x`, `y`, `year`, `population`, `bfsnr`, `gemeindename`, one column per pollutant), built by
`read_exposition_rasters()` → `rasters_to_cells()` → `assign_municipalities()`. Pollutants are
averaged onto the STATPOP grid of the same year (`align_to_reference()`); all STATPOP years share one
grid, so the base scenario is a join on `x`, `y`.

**4. Two aggregation levels, canton and municipality, from the same cells.**
Each cell gets the municipality its centre lies in (`sf::st_intersects`, current municipal boundaries
from geolion for all years – decision 2026-09-18). Special features of the geolion map (decision
2026-09-18):
* exclaves (Glattfelden, Mönchaltorf) are separate features with the municipality's `bfs`: they belong
  to the municipality and are included; each cell matches exactly one feature, so nothing is counted
  twice
* the Kloster Fahr (`bfs = 0`, "ausserkantonale Enklave") belongs to the Canton of Aargau and is
  removed from the map in `_setup.R` (`drop_foreign_enclaves()`, 1 cell / 23 inhabitants in 2024)
* cells whose centre lies in a lake without municipality (`bfs = 0`) are inhabited shore cells: they
  get the nearest municipality (`sf::st_nearest_feature`, 68 cells / 454 inhabitants in 2024)

Hence every cell inside the canton has a municipality:
* canton: all cells with a `bfsnr`
* municipality: the same cells, one row per `bfsnr`
* consistency: municipality populations add up to the canton, and the population-weighted combination
  of municipality means equals the canton mean (tested).
Municipality outputs: population-weighted means only (no distributions, no base scenario).

**5. STATPOP collector pixels are subtracted and spread over their municipality**
(`expo_correct_noloc <- TRUE`, decisions 2026-09-18). Inhabitants BFS cannot locate sit in one
collector pixel per municipality (2024: 159 pixels, 7'141 inhabitants ≈ 0.44 %; Zürich 1'449); they
used to carry the concentration of that arbitrary cell. `read_statpop_ha()` subtracts them;
`redistribute_noloc()` gives them back to the municipality the collector pixel lies in, spread over
its cells with > 0 inhabitants **in proportion to the cells' inhabitants** (factor
`1 + noloc / located`). Assumption (not verifiable): they are exposed like the located inhabitants of
their municipality. Consequences: municipality means unchanged, municipality and canton populations
complete, no artificial peak in the distributions, municipalities still add up to the canton.
Rejected: canton mean for all of them (artificial peak in one class, loses the municipality), a
pseudo-municipality "Sammelpixel" (breaks one row per `bfsnr`), an equal share per inhabited cell
(shifts municipality means towards sparsely populated cells). Inhabitant counts are rounded to whole
persons on output (`round_population()`).

**6. Derived parameters stay statistical, but are fitted once and applied explicitly.**
O3 peak season from NO2 (`fit_o3_peakseason_model()`: robust regression, common slope, one offset per
year, years with ≥ 7 sites) and PM2.5 before 2015 from PM10 (`fit_pm_ratio()`: robust mean
PM2.5:PM10 ratio per year at NABEL sites, without Bern-Bollwerk). Years without coefficients yield
`NA` with a warning instead of an error.
Both models are **refitted on every run** with the current monitoring data (decision 2026-09-18):
fitting takes < 1 s, it uses the best available data (incl. corrected measurements in
`airquality.data`), and all years share one method and one data state – consistent with the full
recompute (decision 2). Price: earlier years shift slightly with every update (O3 peak season via the
common slope; vs. the old outputs up to 0.9 % in the mean and up to 18 pp between neighbouring
distribution classes; PM2.5 up to 0.5 %). To keep this traceable, every run appends its coefficients
to `inst/extdata/log/exposition_derivation_coefficients.csv` (`run`, `parameter`, `year`, `term`,
`value`; not part of the output contract). Open for the methodological revision: a year-specific O3
slope would decouple the years but is less certain with 7–15 sites per year.

## Findings about the old pipeline

* **Double counting of exclave and lake cells** (fixed). `merge_statpop_with_subareas()` joined
  municipality names by `bfs` onto the cells, but the geolion map has several features for some
  `bfs` numbers: Glattfelden (58) and Mönchaltorf (196) have an exclave, `bfs = 0` has three features
  (two lake areas, the Kloster Fahr enclave). Their cells were counted twice resp. three times in the
  weighted means (e.g. Glattfelden 2023: 10'862 instead of ~5'400 inhabitants). The distributions were
  not affected.
* **Cropping**: the old readers cropped every raster to the canton polygon at its native resolution
  before warping to 100 m; now rasters are read for the bounding box, warped, and cells are selected by
  their 100 m centre. Affects only cells at the canton border.

## Scope decisions (deliberate, not technical limits)

* Step 1 (done): exposition reworked; all other scripts still use the old functions, only the
  obsolete `airquality.methods::` prefixes were removed.
* Step 2 (planned): `targets` pipeline, one `R/pipeline_<topic>.R` per current script, pure functions
  in `R/fn_<topic>.R`, `config.yml`, outputs as file targets, Quarto via `tarchetypes::tar_quarto()`,
  `dev/` for interactive work with `tar_load()`.

## Regression results (step 1)

See `tests/regression/`: `run_exposition_variants.R` computes the variants `legacy` (old semantics),
`fixed` (no double counting) and `production` (plus collector pixels subtracted);
`compare_exposition.R` compares them with the baseline.

First full run 2026-09-18 (years 2010–2025; 2025 is new, the baseline ends 2024). Summary
(max. absolute relative deviation from the baseline, canton weighted means):

| Effect | Evidence |
|---|---|
| **Refactoring reproduces the old results** (`legacy`) | population 2020–2024 identical, 2010–2019 +0.04 %; weighted means of NO2, PM10, O3_max_98p_m1 ≤ 0.01 %; distribution curves of these ≤ 0.0003 |
| border cells before 2020 kept | 2010–2019 the old code cropped 200 m rasters to the canton polygon *before* refining and lost ~600 inhabitants/year in 17 border municipalities (e.g. Wil, Richterswil, Laufen-Uhwiesen, up to 2.4 %); from 2020 (100 m/20 m rasters) identical |
| derived parameters refitted | O3 peak season up to 0.9 %, PM2.5 2010–2014 up to 0.5 % (2015+ identical). A full recompute refits `fit_o3_peakseason_model()` (common slope over all years) and `fit_pm_ratio()` with the current monitoring data; coefficients fitted on data up to 2022 vs. 2025 differ by the same order. O3 peak-season inhabitants sit in two 2 µg classes, so a 0.9 % shift moves up to 18 % of the distribution curve (max. curve distance 0.18). |
| double counting fixed (`fixed`) | canton population −0.66 %, Glattfelden/Mönchaltorf halved (2023: 5'393 / 4'217), canton means ≤ 0.2 % |
| collector pixels subtracted (`production`) | canton population a further ≈ −0.43 % (total ≈ −1.1 %), municipal NO2 means up to 1.5 %, distributions ≤ 0.6 % population |
| ndep distribution | identical |

The production run of `scripts/_compile_exposition_data.R` is byte-identical to the `production`
variant. Rerun after the enclave/lake rule and the collector redistribution (2026-09-18), variant
`noloc_dropped` = collector inhabitants subtracted but not redistributed. 2024, NO2:

| variant | canton population | population-weighted mean NO2 |
|---|---|---|
| baseline (old pipeline) | 1'634'927 | 12.3841 |
| legacy | 1'634'927 | 12.3841 |
| fixed | 1'624'158 | 12.4010 |
| noloc_dropped | 1'617'017 | 12.3983 |
| production | 1'624'157 | 12.3973 |

Production restores the full population of `fixed` (difference 1 = rounding) while the municipality
means equal `noloc_dropped` (difference < 1e-13). All collector inhabitants in the canton could be
redistributed. The 160 municipalities add up to the canton: population difference ≤ 1 and weighted
mean difference ≤ 2e-6, both from rounding the inhabitant counts on output.

Full project run with 0.4.0 (2026-09-18): all `_compile_*.R` scripts and `_plot_airquality.R` run
without errors (trends ≈ 30 min, the rest a few minutes). Changed outputs besides exposition:
`data_health_outcomes.csv` (derived from the canton weighted means, now incl. 2025) and the two
trend files: the random forest meteo-normalisation has no seed, so `type == "Trend"` values vary
between runs (median 0.3 pp, max 2.5 pp of relative immission) – set a seed in step 2.

Derived parameters: refitted on every run, coefficients logged (see architecture decision 6).

## Conventions

* Code, comments, roxygen, messages in English; reports in German (ask when unclear).
* tidyverse style, native pipe, `.by =`, `join_by()`, `purrr::map_*()`, `cli` for messages/errors.
* Only functions exported by `airquality.methods` carry the `airquality.methods::` prefix.
* Tests first (testthat 3e); no test touches the network.

## Where to look next

* `scripts/_compile_exposition_data.R` and `R/exposition.R` – the reworked chain.
* `tests/regression/` – how new results are checked against the old ones.
* `../airquality.methods/CLAUDE.md` – the function library and its migration table.
