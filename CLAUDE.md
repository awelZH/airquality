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
| `scripts/_setup.R` | packages, `load_all()`, sources `_settings.R`, municipality map |
| `scripts/_settings.R` | **all analysis settings** (see decision 7), pure assignments |
| `scripts/_compile_*.R` | one script per topic: emissions, monitoring, trends, exposition, outcomes |
| `scripts/_plot_setup.R`, `_plot_<topic>.R` | plots from the output CSVs, one script per report topic (`plots_<topic>`); sourced by the Quarto pages and usable in the console (decision 9) |
| `scripts/_plot_airquality.R` | sources `_plot_setup.R` and all topic plot scripts (interactive use) |
| `R/` | analysis-specific functions, one file per topic: `exposition.R`, `emissions.R`, `monitoring.R`, `plot.R`, `helpers.R`, …; the others (`prepare.R`, `aggregate.R`, …) still hold the not yet reworked topics |
| `inst/extdata/meta/` | input metadata (resources, thresholds, RSD filters, subsector lookup, …) |
| `inst/extdata/output/` | **output CSVs – the contract with external processes** |
| `inst/extdata/log/` | run logs, appended on every run (e.g. coefficients of derived parameters); not part of the contract |
| `docs/` | Quarto website (`quarto::quarto_render("docs/")`) |
| `tests/testthat/` | unit tests (`devtools::test()`) and the output schema test |
| `tests/regression/` | frozen baseline outputs, regression runs on frozen inputs (`run_topic.R`) and the generic comparison (`compare_outputs.R`) |

## Output contract

The 14 CSV files in `inst/extdata/output/` are read by external processes. File names, column names,
column order and format must not change (the directory may change; external paths can be adjusted):

* delimiter `;`, UTF-8, one header line, `NA` for missing values, full numeric precision
  (`airquality.methods::write_local_csv()`)
* `data_exposition_weighted_means_*.csv` use the Greek letter `μ` (U+03BC) in `unit`; other files use
  the micro sign `µ` (U+00B5). Historical inconsistency, kept on purpose – do not "fix" silently.
* `tests/testthat/test-output-schema.R` checks names and column order against
  `tests/regression/baseline/` (outputs as of commit `c0cb59c`).

## Status (2026-09-21)

* **Step 1 done** (commits 889a80b, 696ec49, 39ebaa8 on `dev`): exposition reworked.
* **Step 2, phase 2a in progress**: emissions done (incl. plots/legend), monitoring done. Next in
  the agreed order: outcomes → trends → plots/report. **Each topic only after the user's go.**
* **Phase 2b (targets) not started**; its plan is below.
* Content changes of the last rounds that are **not yet in `inst/extdata/output/`** unless the user
  reran the scripts: subsector grouping of the emissions, the two monitoring fixes.

## How we work (process)

* The user plans interactively: discuss options with quantified consequences and a recommendation,
  **implement only after an explicit go**; methodological questions are decided by the user.
* Per topic: regression reference first → tests first (TDD) → pure functions in `R/<topic>.R` →
  thin script → regression byte-identical → document here → commit (the user says when).
* Deliberate content changes get their **own commit** before or after the refactoring, so the
  refactoring itself always stays byte-identical. Restore points are tagged (see below).
* Code, comments, roxygen and messages in English; reports and plots in German.
* tidyverse style, native pipe, `.by =`, `join_by()`, `purrr::map_*()`, `cli` for messages/errors.
* Only functions exported by `airquality.methods` carry the `airquality.methods::` prefix.
* Tests first (testthat 3e), synthetic data, no test touches the network.
* Run `devtools::document()` after changing roxygen comments (keeps `NAMESPACE` and `man/` in sync).

# Project Decision Log

## The one recurring lesson

**Make every step visible and recomputable.** Scripts that communicate through global variables,
append to their own outputs and derive their work list from earlier results (`get_years()`) are hard
to debug and silently carry old errors forward: the double counting of exclave cells (see below)
survived for years because every run only appended new years.

## Architecture decisions and why

**1. `airquality.methods` 0.4.0 is used from a local installation for now.**
Installed into renv with `renv::install("C:/Users/Public/Git Repos/airquality.methods")`; the lockfile
records a local source. Switch to `awelZH/airquality.methods@<sha>` once 0.4.0 is pushed. With 0.4.0
only its exports may carry the `airquality.methods::` prefix.
**After (re)installing `airquality.methods`, restart the R session** before running `_setup.R`:
`library()` does not reload an already loaded namespace, so `load_all()` would see the old version
and offer to install it (answer "No"). Check with `getNamespaceVersion("airquality.methods")`.

**2. Exposition is recomputed completely on every run** (2026-09-18). Downloads are cached by
`airquality.methods` (`geo_admin_cache_dir()`), so a full run is cheap. The four exposition CSVs are
overwritten, not appended. `get_years()` and `read_all_raster` are gone.
Exception: **uncompressed GeoTIFFs are never cached** – they are streamed through GDAL `/vsicurl/`
(only the canton window is transferred), so they are read from the web on every run; csv/parquet
and compressed assets are cached. Every `Reading "<item>" (<format>, <source>).` message says which
applies: `streamed from the web`, `from cache` or `downloading` (`inform_reading()` in
`airquality.methods`, 2026-09-21).

**3. The cell table is the unit of work for exposition.** One row per inhabited 100 m STATPOP cell and
year (`x`, `y`, `year`, `population`, `bfsnr`, `gemeindename`, one column per pollutant), built by
`read_exposition_rasters()` → `rasters_to_cells()` → `assign_municipalities()`. Pollutants are
averaged onto the STATPOP grid of the same year (`align_to_reference()`); all STATPOP years share one
grid, so the base scenario is a join on `x`, `y`.

**4. Two aggregation levels, canton and municipality, from the same cells** (2026-09-18). Each cell
gets the municipality its centre lies in (`sf::st_intersects`, current municipal boundaries from
geolion for all years). Special features of the geolion map:
* exclaves (Glattfelden, Mönchaltorf) are separate features with the municipality's `bfs`: they belong
  to the municipality and are included; each cell matches exactly one feature, so nothing is counted
  twice
* the Kloster Fahr (`bfs = 0`, "ausserkantonale Enklave") belongs to the Canton of Aargau and is
  removed from the map in `_setup.R` (`drop_foreign_enclaves()`, 1 cell / 23 inhabitants in 2024)
* cells whose centre lies in a lake without municipality (`bfs = 0`) are inhabited shore cells: they
  get the nearest municipality (`sf::st_nearest_feature`, 68 cells / 454 inhabitants in 2024)

Hence every cell inside the canton has a municipality; municipality populations add up to the canton
and the population-weighted combination of municipality means equals the canton mean (tested).
Municipality outputs: population-weighted means only (no distributions, no base scenario).

**5. STATPOP collector pixels are subtracted and spread over their municipality**
(`expo_correct_noloc <- TRUE`, 2026-09-18). Inhabitants BFS cannot locate sit in one collector pixel
per municipality (2024: 159 pixels, 7'141 inhabitants ≈ 0.44 %; Zürich 1'449); they used to carry the
concentration of that arbitrary cell. `read_statpop_ha()` subtracts them; `redistribute_noloc()`
gives them back to the municipality the collector pixel lies in, spread over its cells with > 0
inhabitants **in proportion to the cells' inhabitants** (factor `1 + noloc / located`). Assumption
(not verifiable): they are exposed like the located inhabitants of their municipality. Consequences:
municipality means unchanged, populations complete, no artificial peak in the distributions.
Rejected: canton mean for all of them, a pseudo-municipality "Sammelpixel", an equal share per
inhabited cell. Inhabitant counts are rounded to whole persons on output (`round_population()`).

**6. Derived parameters stay statistical, but are fitted once and applied explicitly.**
O3 peak season from NO2 (`fit_o3_peakseason_model()`: robust regression, common slope, one offset per
year, years with ≥ 7 sites) and PM2.5 before 2015 from PM10 (`fit_pm_ratio()`: robust mean
PM2.5:PM10 ratio per year at NABEL sites, without Bern-Bollwerk). Years without coefficients yield
`NA` with a warning instead of an error. Both models are **refitted on every run** with the current
monitoring data (2026-09-18): fitting takes < 1 s and all years share one method and one data state –
consistent with the full recompute (decision 2). Price: earlier years shift slightly with every
update (O3 peak season up to 0.9 %, PM2.5 up to 0.5 %). Every run appends its coefficients to
`inst/extdata/log/exposition_derivation_coefficients.csv` (not part of the contract). Open: a
year-specific O3 slope would decouple the years but is less certain with 7–15 sites per year.

**7. All analysis constants live in `scripts/_settings.R`** (2026-09-18; own file since 2026-09-21,
sourced by `_setup.R`, the plot scripts and the report), grouped by topic:
* names carry the topic as prefix (`emis_`, `mon_`, `trend_`, `expo_`, `plot_`), general settings
  without (`year_offset`, `year_last`, `base_scenario_year`, `crs`); maps 1:1 to `config.yml` in 2b
* scripts only use them and do not `rm()` them; functions in `R/` get them as arguments, never as
  globals
* **every year range ends at `year_last = current year − year_offset`**. Exception with its own
  meaning: `emis_year_max` = current year (EMIKAT projections beyond it are dropped, and it is the
  newest RSD vehicle model year)
* `base_scenario_year` (exposition/outcomes) and `plot_reference_year_emissions` are **independent**
  settings, even though both are 2015
* graphical settings (sizes, colours, line types, `siteclass_levels`) stay in `_plot_setup.R`:
  presentation, not analysis
* method constants with documented defaults in `R/` are not settings: `fit_pm_ratio()`, the
  classification thresholds of the monitoring (`classify_*()`)

**8. Inputs are checked where they enter** (2026-09-19). `check_columns()` (R/helpers.R) and the
topic-specific checks (`check_rsd_filters()`) stop with a `cli` error of class
`airquality_input_error` naming the dataset and what is missing. Reason: online sources and
`airquality.data` change between the twice-yearly updates; before, a missing filter criterion
silently filtered everything or nothing.

**9. The report builds its plots while rendering; no plot rds files** (2026-09-21). Each Quarto
page sources `scripts/_plot_setup.R` and its topic script(s) inside `withr::with_dir("..", …)`.
Not knitr's `root.dir: ".."`: the year tabs are knitted inline (`knitr::knit(text = …)`) and would
write their figures into the project root, while the HTML points to `docs/`. The same scripts work
in the console (`source("scripts/_plot_setup.R"); source("scripts/_plot_exposition.R")`, then
`get_plot(plots_exposition, "...")`), so plots are developed outside Quarto. `_plot_exposition.R`
reads the municipality map from geolion itself (one WFS call). Reason: `docs/plots_exposition.rds`
was 2.4 GB, because every ggplot keeps its `plot_env`: each map built in `lapply()` inside
`plot_all_popweighmean_maps()` dragged along the wrapper frame, i.e. the municipality `sf` of all
years (132 MB) plus the list of all maps of that parameter (708 MB per map on its own). Timings
(2026-09-21): building all plots 19 s, but `saveRDS()` made the old plot script take 289 s; render
of the site 459 s old (without the plot script) vs. 393 s new. Of the 309 figures the pages
reference, 306 were byte-identical; the other 3 used `geom_jitter()` without a seed and changed on
every render (grenzwertvergleich, ndep-all, ndep-all-cln). They now use
`geom_point(position = position_jitter(..., seed = jitter_seed))` (`jitter_seed` in
`_plot_setup.R`), so they stay the same between renders. 9 stale PNGs that no page referenced were
deleted from `docs/*_files/`. Keep this in mind for phase 2b: plots as targets would put the same
bloat into the store, so the report depends on the output CSVs, not on plot targets.

## Analysis decisions and findings per topic

### Exposition (step 1)

* **Double counting of exclave and lake cells** (fixed): `merge_statpop_with_subareas()` joined by
  `bfs`, but the geolion map has several features for some `bfs` numbers (Glattfelden 58,
  Mönchaltorf 196, `bfs = 0` three features). Their cells were counted twice resp. three times in the
  weighted means (Glattfelden 2023: 10'862 instead of ~5'400 inhabitants); distributions unaffected.
* **Cropping**: the old readers cropped every raster to the canton polygon at its native resolution
  before warping to 100 m; now rasters are read for the bounding box, warped, and cells are selected
  by their 100 m centre. Affects only cells at the canton border (2010–2019 ~600 inhabitants/year in
  17 border municipalities, up to 2.4 %; from 2020 identical).
* Regression 2026-09-18 vs. the old pipeline (2024, NO2, canton): population 1'634'927 → 1'624'157
  (double counting −0.66 %, collector pixels a further −0.43 %), weighted mean 12.3841 → 12.3973.
  The refactoring alone (`legacy`) reproduced the old results (≤ 0.01 %). Details and the variant
  scripts: `tests/regression/run_exposition_variants.R`, `compare_exposition.R`.

### Emissions (phase 2a, 2026-09-18/19)

`scripts/_compile_emission_data.R`: read → prepare → aggregate → write; functions in `R/emissions.R`,
92 expectations in `tests/testthat/test-emissions.R`. Refactoring (commit `f4d941f`) byte-identical;
the reference was byte-identical to the committed outputs.

| old | new |
|---|---|
| `prepare_emmissions(data, filter_args = <quoted expression>)` | `prepare_emissions(data, canton, exclude_subsectors, year_max)` |
| `aggregate_emmissions()` incl. plot colours and an unused, with 0.4.0 no longer working automatic regrouping `groups_emission_subsector()` | `aggregate_emissions()` + `group_minor_subsectors()` + `add_emission_colours()` |
| `prepare_rsd(data, rsd_auxiliary)` with hidden `year(Sys.Date())` as newest vehicle model year | `prepare_rsd(data, meta, filters, model_year_max)`, script passes `emis_year_max` |
| `aggregate_rsd_nox(data, rsd_auxiliary, groups)` + `aggregate_rsd()` | `aggregate_rsd_nox(data, meta, filters, groups)` |
| `source` hard-coded, `tidyr::spread()`, `group_by()`/`ungroup()` | `source` from the data, `pivot_wider()`, `.by` |

**Subsector grouping: lookup table + per-pollutant rule** (user decision 2026-09-19). Criteria: at
most 4 subsectors per pollutant and sector **including** "verschiedene"; subsectors with a mean
yearly share < 5 % of the pollutant's canton total go into "verschiedene"; the lookup table lists all
subsectors.
* `emikat_subsector_new.csv` lists all 35 subsectors and only merges/renames thematically; no entry
  maps to "verschiedene" any more. `aggregate_emissions()` warns about missing subsectors.
* `group_minor_subsectors(min_share = emis_subsector_min_share, max_per_sector =
  emis_subsectors_max)` (settings 0.05 and 4): mean of the **yearly** shares (years without emission
  count as 0), so a pollutant keeps the same groups over its whole time series; if an "verschiedene"
  group exists or there are more than 4 subsectors, only the 3 largest keep their name.
* Result: `data_emissions.csv` 3996 → 2772 rows, totals per year, pollutant and sector unchanged
  (≤ 4e-16). Now named e.g. Zonenverkehr (CO 28 %), Flächenquellen (SO2 18 %); now in
  "verschiedene" e.g. Feuerungen Öl & Gas for PM/eBC/NH3/CO. Some sector/pollutant combinations
  consist of "verschiedene" only (e.g. NOx Industrie 11 %).
* Why a lookup table alone cannot do it: "small" depends on the pollutant (39 named combinations
  stayed below 5 % in every year, while 14 subsectors in "verschiedene" reached ≥ 5 %).
* **Restore point** of the lookup-only state: commit `f4d941f`, git tag `emissions-lookup-only`.
* Plot order and colours are ranked **without** the projections 2030/2040/2050
  (`prepare_emissions(year_max = )`); with the current data the ranking is the same either way.

Findings: the RSD aggregation completes all factor-level combinations with `n = 0` (two empty rows
today). An experiment that read `emikat_subsector_new.csv` without `locale(encoding = "UTF-8")`
matched only 19 of 32 entries (umlauts) – always pass the locale.

### Monitoring (phase 2a, 2026-09-19)

`scripts/_compile_monitoring_data.R` is 33 lines (was 127 with 7 helper functions inside); functions
in `R/monitoring.R`, 40 expectations in `tests/testthat/test-monitoring.R`, cut along the later
sub-analyses `mon_aq_` and `mon_ndep_`. Inputs are package datasets of `airquality.data` 0.1.3 (no
network). **Two bugs were fixed first (commit `c390948`), then the refactoring (commit `e77e627`)
reproduced the fixed outputs byte-identically.**

* `prepare_monitoring_airquality(data, cantons = mon_cantons)`; the extra NABEL site names were
  redundant (both carry `canton = "ZH"`).
* ndep: `prepare_ndep_site_meta()` → `prepare_ndep_parameters()` → `aggregate_ndep()`, plus
  `recode_ecosystems()`, `derive_source_category()` and the `classify_*()` functions (thresholds as
  documented defaults).
* Fixes: the factor levels `c("hoch", "mittel", "tiel")` turned every "tief" into `NA` (14 of 132
  rows, 98 of 924); ecosystem "Siedlungen" (only site WIE, `cln = NA`) now counts as
  "kein empf. Ökosys." (user decision). No value changed, and the plots are unaffected (they drop
  `NA`/"Siedlungen" ecosystems and rows without `cln` and overwrite the site class).
* Note: the committed ndep outputs had been written with an older code version than the script.
* Removed as dead code: `aggregate_nitrogen_deposition()`, `simplify_nitrogen_parameters()`.
* **`datasource` cleaned up** (user decision 2026-09-21, own commit): the column mixed spellings and
  duplicates, because the input already contains combined entries such as "FUB, Ostluft" and the old
  code only pasted the distinct *entries* in row order. `combine_sources()` now splits at the comma,
  trims, deduplicates and sorts, so the column no longer depends on the row order of the input. The
  132 rows of `data_ndep_monitoring_y1.csv` went from 6 spellings (`FUB,Ostluft` 24, `Ostluft,FUB`
  22, `FUB,FUB, Ostluft,Ostluft` 3, `FUB,Ostluft,FUB, Ostluft` 5, `Ostluft,FUB, Ostluft,FUB` 2,
  `Ostluft` 76) to 2 (`FUB,Ostluft` 56, `Ostluft` 76). Only this column changed; no value, and the
  other two outputs stay byte-identical.

### Plots: grouped legend (2026-09-19)

`ggplot_emissions()` shows one legend block per sector, with the sector as title and the subsectors
without the pasted sector (was "Sektor / Subsektor"); the subsector names in the lookup table were
shortened accordingly by the user. Generic functions in `R/plot.R` (to move to `airquality.methods`):
* `grouped_key(group, key, order, group_order)` – unique key `"group::key"` as factor; its levels set
  the order of stack and legend ("verschiedene" exists in several sectors)
* `add_grouped_legend(plot, aesthetic, sep, spacing, subtitle, key_spacing)` – sets
  `legendry::guide_legend_group(key_group_split(sep))` plus the theme (block titles from the plot's
  `legend.text`, group spacing 3 mm, `legend.key.spacing.y` 0). The plot stays an ordinary ggplot.

The NH3 special case (agriculture last) is the argument `ggplot_emissions(sectors_last = )` instead
of `%+%` on the finished plot. **Rejected: blocks in two columns** (restore point, commit `3f759b1`):
`legendry` arranges blocks only in one row or column, so the columns had to be drawn separately and
placed with `guide_custom()` – legend twice as wide, fixed when drawn, and building grobs needed a
temporary `ragg` device (a pdf device leaves `Rplots.pdf` and does not know Arial from `theme_ts`).
With the native legend, NMVOC (6 blocks, 11 entries) just fits a 5 in high figure – check when
figures get smaller. Findings: `longpollutant()` is called without the `airquality.methods::` prefix
in several functions of `R/plot.R` (works only while the package is attached; fixed in
`ggplot_emissions()`); neighbouring subsectors of a sector can get similar shades because the colour
ramp is assigned over all pollutants (e.g. PM2.5 Haushalte).

### Trends and outcomes (not yet reworked)

The random forest meteo-normalisation has no seed, so `type == "Trend"` values vary between runs
(median 0.3 pp, max 2.5 pp of relative immission) – set a seed in phase 2a. A full project run takes
≈ 30 min for the trends and a few minutes for the rest. The YLL part of `_compile_outcomes.R` is
unfinished work (`year <- 2018`).

## Regression workflow (phase 2a)

Online inputs can change between runs, so a comparison with the committed outputs mixes data and
code changes. Instead, old and new code run on the **same frozen inputs**:

1. before refactoring: `source("tests/regression/run_topic.R"); run_topic("<topic>", "reference")`
2. after refactoring: `run_topic("<topic>", "candidate")`
3. `source("tests/regression/compare_outputs.R")`;
   `compare_outputs("tests/regression/results/<topic>/reference", ".../candidate")`

`run_topic()` sources the unchanged topic script; `testthat::with_mocked_bindings()` replaces the
network readers of `airquality.methods` (`read_opendataswiss()`, `read_geolion_wfs()`) by a
record/replay version (`tests/regression/inputs/*.rds`, keyed by `rlang::hash()` of the arguments,
`refresh = TRUE` downloads again) and redirects `write_local_csv()` to
`tests/regression/results/<topic>/<label>/`, so `inst/extdata/output/` is never touched. Settings are
evaluated from `scripts/_setup.R` and `scripts/_settings.R` (only the listed assignments; no package
loading, no `airquality.data` update). A new topic needs an entry in `topics` (script, settings, `attach` for
packages an old script expects to be attached; further readers in `network_readers`). Inputs and
results are gitignored. Topics whose inputs are `airquality.data` datasets (monitoring, trends) need
no frozen downloads, but both runs must use the same installed version of that package.
Settings that depend on the date (`year_last`, `emis_year_max`) make a reference valid for the
current year only.

`compare_outputs()` works for all 14 files without configuration: byte identity, header (contract),
rows only in one file (key = non-numeric columns plus integer-valued ones such as `year`), values
that became `NA`, and the largest relative deviation.

# Step 2 plan: targets project

Approved 2026-09-18, refined 2026-09-19. Goal: a `targets` pipeline that is readable step by step,
easy to debug, try out and extend, robust for the twice-yearly update, with unchanged output file
names and schemas.

## User decisions

1. **Outputs move to `data/output/`**; the external processes will be adjusted to the new path.
2. **Pure targets project, no package skeleton** (option B). No NAMESPACE, `man/`, `@export`,
   `load_all()`: targets tracks functions loaded with `tar_source()` automatically, and the stale
   NAMESPACE/`man/` plus the `load_all()` version prompt caused trouble in step 1. Given up:
   `?function` help pages and the `devtools::test()` shortcut; roxygen comments stay as in-code docs.
3. **Website stays rendered into `docs/`** (GitHub Pages), Quarto sources move to `report/`.
4. **`tod_nat_gatu.csv`** (non-public mortality data, gitignored) stays in the project, in
   `data/restricted/`; may become OGD later.
5. Derived parameters refitted on every run, coefficients logged (already implemented).
6. **One target list per sub-analysis, not per topic** (2026-09-19), so each part can be built and
   inspected on its own (`tar_make(names = starts_with("emis_rsd_"))`):

   | sub-analysis (target prefix) | pipeline file | outputs |
   |---|---|---|
   | `emis_emikat_` | `pipelines/emissions_emikat.R` | `data_emissions.csv` |
   | `emis_rsd_` | `pipelines/emissions_rsd.R` | the 3 `data_nox_*rsd*.csv` |
   | `mon_aq_` | `pipelines/monitoring_airquality.R` | `data_airquality_monitoring_y1.csv` |
   | `mon_ndep_` | `pipelines/monitoring_ndep.R` | `data_ndep_pars_monitoring_y1.csv`, `data_ndep_monitoring_y1.csv` |
   | `expo_pop_` | `pipelines/exposition_population.R` | the 2 weighted-mean files, `data_exposition_distribution_pollutants.csv` |
   | `expo_eco_` | `pipelines/exposition_ecosystems.R` | `data_exposition_distribution_ndep.csv` |
   | `report_` | `pipelines/report.R` | `docs/` (plots built while rendering, decision 9) |
   | – (WIP, see 7) | `wip/outcomes.R` | `data_health_outcomes.csv` |
   | – (WIP, see 7) | `wip/trends.R` | the 2 trend files |

7. **Work in progress stays outside targets for now** (2026-09-19): health outcomes and trends remain
   plain scripts in `wip/` until the user has finished developing them, then they are integrated as
   sub-analyses. Consequences for phase 2b:
   * the WIP scripts read pipeline outputs from `data/output/` (outcomes ←
     `data_exposition_weighted_means_canton.csv`; trends ← `data_airquality_monitoring_y1.csv`,
     `data_emissions.csv`) and write their outputs there; they `source()` the `R/` files;
     `wip/README.md` says how to run them
   * their 3 outputs are part of the contract and used by the report: the report tracks them as
     external `format = "file"` targets, and a check target warns when a WIP output is older than the
     pipeline outputs it is based on
   * full run in `run.R`: `tar_make()` without the report → WIP scripts (optional) → `tar_make()`;
     each step can also be run alone
   * phase 2a still applies to them in full: tests first, pure functions in `R/outcomes.R` /
     `R/trends.R`, thin scripts, regression on frozen inputs, a seed for the random forest

## Target layout

```
_targets.R            options, tar_source("R"), combine pipelines/*
_targets.yaml         targets project settings
config.yml            years/year_offset, base_scenario_year, correct_noloc, crs, paths
run.R                 human entry point: tar_make(), progress summary, quarto render
DESCRIPTION           dependency manifest only (renv snapshot.type = "explicit")
R/                    pure functions per topic (roxygen comments kept as in-code docs)
pipelines/            one target list per sub-analysis (decision 6), plus setup
wip/                  work in progress outside targets (decision 7): outcomes.R, trends.R, README.md
data/meta|output|log/ from inst/extdata/… (contract: names, columns, format unchanged)
data/restricted/      non-public inputs; folder gitignored except README.md
report/               Quarto sources (*.qmd, _quarto.yml) and plot scripts; output-dir ../docs
docs/                 rendered website only
dev/                  interactive scripts (tar_load(), experiments)
tests/testthat/       unit tests per R/ file; helper sources R/; dependency test
tests/regression/     frozen baseline + generic comparison for all outputs
```

## Phase 2a – improve per topic (within the current script structure)

Order confirmed 2026-09-18: emissions → monitoring → outcomes → trends → plots/report. Per topic:
thin script (read → prepare → aggregate → write, no helper functions defined inside, no hidden
globals), pure functions in `R/<topic>.R`, tests first, decisions documented here, and every function
designed so that it can later become a target 1:1.

## Phase 2b – structural change (only after phase 2a)

1. **Skeleton**, no behaviour change: `_targets.R`, `_targets.yaml`, `config.yml` (`config` package),
   `run.R`, `pipelines/setup.R`; `tests/testthat/helper-source.R` sourcing `R/`; `tests/testthat.R` →
   `testthat::test_dir()`; renv `snapshot.type = "explicit"`; dependency test comparing
   `renv::dependencies()` with DESCRIPTION; `.gitignore` adds `_targets/`.
2. **Data move**: `git mv inst/extdata/{meta,output,log}` → `data/…`; `tod_nat_gatu.csv` →
   `data/restricted/` plus a committed README and gitignore rules; `ressources.csv` entry for the
   mortality source; update paths in `ressources.csv`, `prepare_ressources()`, `docs/index.qmd`,
   schema test and this file. Output path from `config.yml`.
3. **Sub-analysis pipelines** from the functions improved in phase 2a, order: expo_pop (raster
   metadata target with `tar_cue("always")`; checks as targets before writing) → expo_eco →
   emis_emikat → emis_rsd → mon_aq → mon_ndep → report (`tarchetypes::tar_quarto()` renders
   `report/` into `docs/`; it depends on the output CSVs as file targets, no plot targets; the plot
   scripts move along with the report and stay usable in the console, decision 9; WIP outputs as
   external file targets plus staleness check).
   Conventions: target names `<topic>_<subanalysis>_<stage>_<content>` (e.g. `emis_emikat_raw`,
   `expo_pop_out_weighted_means_canton`), `tar_plan()` syntax, outputs as `format = "file"` targets.
   Old scripts keep running until their sub-analysis is migrated.
4. **Remove the package skeleton** once all sub-analyses run in targets: NAMESPACE, `man/`,
   `@export` tags, `scripts/`, `analyse_airquality.R`; `tar_option_set(workspace_on_error = TRUE)`.
5. **airquality.methods**: after its 0.4.0 push, pin the GitHub sha in `renv.lock`; move the generic
   pieces there: `assign_municipalities()`, `append_log()`, the grouped legend (`grouped_key()`,
   `add_grouped_legend()`, dependency `legendry`) and `check_columns()`.
6. **Docs**: this file (structure, decisions, workflow `tar_make()` / `tar_load()` /
   `tar_workspace()`, `tar_mermaid()` diagram), README, `wip/README.md`.
7. **Later, when the user has finished them: integrate the WIP scripts** as sub-analyses `outcomes_`
   and `trends_`: restricted mortality file as `format = "file"` target with a clear `cli` error if
   missing; targets' per-target seeds for the random forest; optional `crew` for the ~30 min of the
   trends; the report's external file targets then become normal dependencies.

## Verification of phase 2b

* `testthat::test_dir("tests/testthat")` green after every step; `tar_validate()`, `tar_manifest()`,
  `tar_visnetwork()` show the intended graph.
* Each sub-analysis builds alone, then regression on frozen inputs: identical outputs (all
  deterministic). A second `tar_make()` without changes skips everything except the metadata cues.
* The WIP scripts run from `wip/` against `data/output/`; the report warns when a WIP output is
  stale; a missing restricted file stops with the README hint.
* Rendering via `tar_make()` updates `docs/` with the same pages as before.

## Open items outside the plan

* `airquality.methods`: the small `CLAUDE.md` update from step 1 is **not committed** – that repo is
  on `master` with the whole 0.4.0 work uncommitted; the user decides where to commit it.
* Methodological question: year-specific O3 peak-season slope (decision 6).

## Where to look next

* `R/emissions.R` + `scripts/_compile_emission_data.R`, `R/monitoring.R` +
  `scripts/_compile_monitoring_data.R` – the pattern for the remaining topics.
* `R/exposition.R` + `scripts/_compile_exposition_data.R` – the reworked chain of step 1.
* `tests/regression/` – how new results are checked against the old ones.
* `../airquality.methods/CLAUDE.md` – the function library and its migration table.
