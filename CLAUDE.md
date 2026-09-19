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
| `scripts/_setup.R` | packages, `load_all()`, **all analysis settings** (see decision 7), municipality map |
| `scripts/_compile_*.R` | one script per topic: emissions, monitoring, trends, exposition, outcomes |
| `scripts/_plot_airquality.R` | builds all plots from the output CSVs, saved as `docs/plots_*.rds` for Quarto |
| `R/` | analysis-specific functions; `R/exposition.R` is the reworked exposition chain |
| `inst/extdata/meta/` | input metadata (resources, thresholds, RSD filters, ...) |
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

**7. All analysis constants live in `scripts/_setup.R`** (decisions 2026-09-18), in one block
"analysis settings" grouped by topic, so that the whole setup can be seen in one place:
* names carry the topic as prefix (`emis_`, `trend_`, `expo_`, `plot_`), general settings without
  (`year_offset`, `year_last`, `base_scenario_year`, `crs`); this maps 1:1 to `config.yml` in phase 2b
* the scripts only use them and do not `rm()` them; functions in `R/` get them as arguments, never
  as globals (`prepare_data_trends()` now takes `cantons`)
* **every year range ends at `year_last = current year − year_offset`** (trends, exposition, plots;
  before, trends and plots used a hard-coded "− 1"). Exception with its own meaning:
  `emis_year_max` = current year (EMIKAT projections beyond it are dropped)
* `base_scenario_year` (exposition/outcomes) and `plot_reference_year_emissions` (relative emission
  plot) are **independent** settings, even though both are 2015
* graphical settings (sizes, colours, line types, `siteclass_levels`) stay in `_plot_airquality.R`:
  presentation, not analysis
* method details with documented defaults in `R/` that are not settings: `fit_pm_ratio()`
  (`source`, `exclude_sites`, `min_year`); the YLL draft in `_compile_outcomes.R` (`year <- 2018`)
  is unfinished work and is handled in phase 2a (outcomes)

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

* Step 1 (done, commits 889a80b, 696ec49, 39ebaa8 on `dev`): exposition reworked; all other scripts
  still use the old functions, only the obsolete `airquality.methods::` prefixes were removed.
* Step 2: see "Step 2 plan" below (phase 2a: improve each topic, phase 2b: targets structure) –
  **started 2026-09-18 with phase 2a.** Done: emissions, monitoring (see "Phase 2a results"). Each
  further topic only after the user's go.

## Step 2 plan: targets project (approved 2026-09-18, refined 2026-09-19; IN PROGRESS – phase 2a)

Goal: a `targets` pipeline that is readable step by step, easy to debug, try out and extend, robust
for the twice-yearly update, with unchanged output file names and schemas.

### User decisions (2026-09-18)

1. **Outputs move to `data/output/`**; the external processes will be adjusted to the new path.
2. **Option B: pure targets project, no package skeleton.** No NAMESPACE, `man/`, `@export`,
   `load_all()`. Reasons: targets tracks changes of functions loaded with `tar_source()`
   automatically (functions in a package namespace need `tar_option_set(imports = ...)`, easy to
   forget); stale NAMESPACE/`man/` and the DESCRIPTION version prompt of `load_all()` caused trouble in
   step 1; the functions are analysis-specific, so no internal package (option C) either.
   Given up: `?function` help pages and the `devtools::test()` shortcut; roxygen comments stay as
   in-code docs.
3. **Website stays rendered into `docs/`** (GitHub Pages), Quarto sources move to `report/`.
4. **`tod_nat_gatu.csv`** (non-public mortality data for the health outcomes, gitignored, read in
   `scripts/_compile_outcomes.R`) stays in the project, in `data/restricted/`; may become OGD later.
5. Derived parameters refitted on every run, coefficients logged (already implemented).

### User decisions (2026-09-19)

6. **One target list per sub-analysis, not per topic**, so each part can be built and inspected on
   its own (`tar_make(names = starts_with("emis_rsd_"))`, `tar_visnetwork()`): emissions EMIKAT,
   emissions RSD, monitoring pollutants, monitoring Ndep, exposition population, exposition
   ecosystems, report. Mapping of the 14 outputs:

   | sub-analysis (target prefix) | pipeline file | outputs |
   |---|---|---|
   | `emis_emikat_` | `pipelines/emissions_emikat.R` | `data_emissions.csv` |
   | `emis_rsd_` | `pipelines/emissions_rsd.R` | `data_nox_vehicle_emissions_rsd_per_norm.csv`, `data_nox_emissions_rsd_per_yearmodel.csv`, `data_nox_emissions_rsd_per_yearmeas.csv` |
   | `mon_aq_` | `pipelines/monitoring_airquality.R` | `data_airquality_monitoring_y1.csv` |
   | `mon_ndep_` | `pipelines/monitoring_ndep.R` | `data_ndep_pars_monitoring_y1.csv`, `data_ndep_monitoring_y1.csv` |
   | `expo_pop_` | `pipelines/exposition_population.R` | `data_exposition_weighted_means_canton.csv`, `data_exposition_weighted_means_municipalities.csv`, `data_exposition_distribution_pollutants.csv` |
   | `expo_eco_` | `pipelines/exposition_ecosystems.R` | `data_exposition_distribution_ndep.csv` |
   | `report_` | `pipelines/report.R` | plots, `docs/` |
   | – (WIP, see 7) | `wip/outcomes.R` | `data_health_outcomes.csv` |
   | – (WIP, see 7) | `wip/trends.R` | `data_airquality_trends_relative_y1.csv`, `data_airquality_trends_relative_aggregated_y1.csv` |

7. **Work in progress stays outside targets for now**: health outcomes (deaths, later YLL) and
   trends remain plain scripts in `wip/` until the user has finished developing them; then they are
   integrated as sub-analyses (`outcomes_`, `trends_`). Reason: develop freely without pipeline
   overhead. Consequences, to handle in phase 2b:
   * the WIP scripts read pipeline outputs from `data/output/` (outcomes ←
     `data_exposition_weighted_means_canton.csv`; trends ← `data_airquality_monitoring_y1.csv`,
     `data_emissions.csv`) and write their outputs there; they `source()` the `R/` files (no
     `load_all()`); `wip/README.md` says how to run them
   * their 3 outputs are part of the contract and used by the report: the report tracks them as
     external `format = "file"` targets, and a check target warns (`cli`, file names and dates)
     when a WIP output is older than the pipeline outputs it is based on (stale results)
   * full run in `run.R`: `tar_make()` without the report → WIP scripts (optional) → `tar_make()`
     (the report picks up the changed CSVs); each step can also be run alone
   * phase 2a still applies to them in full (user decision): tests first, pure functions in
     `R/outcomes.R` / `R/trends.R`, thin scripts, regression on frozen inputs, a seed for the random
     forest; the restricted mortality file stays a script input until the integration

### Target layout

```
_targets.R            options, tar_source("R"), combine pipelines/*
_targets.yaml         targets project settings
config.yml            years/year_offset, base_scenario_year, correct_noloc, crs, paths
run.R                 human entry point: tar_make(), progress summary, quarto render
DESCRIPTION           dependency manifest only (renv snapshot.type = "explicit")
R/                    pure functions per topic: emissions, monitoring, trends, exposition,
                      outcomes, plots, utils (roxygen comments kept as in-code docs)
pipelines/            one target list per sub-analysis (decision 6): setup, emissions_emikat,
                      emissions_rsd, monitoring_airquality, monitoring_ndep,
                      exposition_population, exposition_ecosystems, report
wip/                  work in progress outside targets (decision 7): outcomes.R, trends.R, README.md
data/meta/            from inst/extdata/meta
data/output/          from inst/extdata/output (contract: names, columns, format unchanged)
data/log/             from inst/extdata/log
data/restricted/      non-public inputs (tod_nat_gatu.csv); folder gitignored except README.md
report/               Quarto sources (*.qmd, _quarto.yml), read via tar_read(); output-dir ../docs
docs/                 rendered website only
dev/                  interactive scripts (tar_load(), experiments)
tests/testthat/       unit tests per R/ file; helper sources R/; dependency test
tests/regression/     frozen baseline + generalised comparison for all outputs
```

### Order: first improve each topic, then change the structure (user decision 2026-09-18)

The code of each analysis step is improved first **within the current script structure**, one
topic at a time, like step 1 did for the exposition – so the user can follow, try out and understand
the progress before the big restructuring.

**Phase 2a – improve per topic** (each topic its own commit(s), regression on frozen inputs before
moving on, see "Regression workflow (phase 2a)"; order confirmed by the user 2026-09-18: emissions →
monitoring → outcomes → trends → plots/report):
* script `scripts/_compile_<topic>.R`: thin and readable – read → prepare → aggregate → write, no
  helper functions defined inside scripts (today: `estimate_prelim_deaths()`,
  `estimate_all_prelim_deaths()`, `estimate_yll()` in `_compile_outcomes.R`; `recode_ecosys()`,
  `ostluft_siteclass()`, `cut_emissions_1km()`, `derive_source_cat()`, `cut_estimated()`,
  `cut_frac_estimated()`, `aggregate_ndep()` in `_compile_monitoring_data.R` – they move to `R/`),
  no hidden globals
* functions in `R/<topic>.R`: pure (inputs as arguments, no globals, no file paths inside), modern
  tidyverse (`.by`, `join_by()`, `cli`), roxygen with meaningful `@param`/`@return`
* tests `tests/testthat/test-<topic>.R` first (TDD), synthetic data, no network
* documentation: decisions and findings in this file
* design every function so that it can later become a target 1:1 (this keeps phase 2b mechanical)

**Phase 2b – structural change** (steps 1–6 below), starting only after phase 2a is complete.

### Phase 2b steps (each topic its own commit, regression before moving on)

1. **Skeleton**, no behaviour change: `_targets.R`, `_targets.yaml`, `config.yml` (`config`
   package), `run.R`, `pipelines/setup.R` (ressources, municipality map with
   `drop_foreign_enclaves()`, config); `tests/testthat/helper-source.R` sourcing `R/`;
   `tests/testthat.R` → `testthat::test_dir()`; renv `snapshot.type = "explicit"`; dependency test
   comparing `renv::dependencies()` with DESCRIPTION; `.gitignore` adds `_targets/`.
2. **Data move**: `git mv inst/extdata/{meta,output,log}` → `data/…`; `tod_nat_gatu.csv` →
   `data/restricted/` plus a committed `data/restricted/README.md` (which files, source, who
   provides them) and gitignore rules `data/restricted/*`, `!data/restricted/README.md`; add a
   `ressources.csv` entry for the mortality source ("not public, internal"); update paths in
   `ressources.csv` (`inst/extdata/meta`, GitHub tree links), `R/prepare.R` (`prepare_ressources()`
   detects `inst/extdata`), `docs/index.qmd` links, schema test paths, this file. Output path from
   `config.yml`.
3. **Sub-analysis pipelines** (decision 6) from the functions improved in phase 2a, order:
   expo_pop (raster metadata target with `tar_cue("always")`; checks as targets before writing,
   e.g. municipalities add up to the canton, schema = baseline) → expo_eco → emis_emikat →
   emis_rsd → mon_aq → mon_ndep → report (plots as targets, `tarchetypes::tar_quarto()` rendering
   `report/` into `docs/`; WIP outputs as external file targets plus the staleness check of
   decision 7). Outcomes and trends are not migrated here (decision 7): their scripts move to `wip/`.
   Conventions: target names `<topic>_<subanalysis>_<stage>_<content>` (e.g. `emis_emikat_raw`,
   `emis_rsd_out_per_norm`, `mon_ndep_out_pars`, `expo_pop_cells`,
   `expo_pop_out_weighted_means_canton`, `expo_eco_out_distribution`), `tarchetypes::tar_plan()`
   syntax, comments mirroring the old scripts, outputs as `format = "file"` targets. Regression with
   `tests/regression/compare_outputs.R` (already generic for all 14 output files, built in phase 2a).
   Old scripts keep running until their sub-analysis is migrated.
4. **Remove the package skeleton** once all sub-analyses run in targets: NAMESPACE, `man/`,
   `@export` tags, `scripts/` (the WIP scripts are in `wip/` by then), `analyse_airquality.R`;
   `tar_option_set(workspace_on_error = TRUE)`.
5. **airquality.methods**: after its 0.4.0 push, pin the GitHub sha in `renv.lock`; consider moving
   `assign_municipalities()` and `append_log()` there (generic). Planned move (user, 2026-09-19): the
   grouped legend `grouped_key()`, `add_grouped_legend()` (R/plot.R, tests in test-plot.R;
   dependency `legendry`); also a candidate: `check_columns()` (R/helpers.R).
6. **Docs**: this file (structure table, decisions, workflow `tar_make()` / `tar_load()` /
   `tar_workspace()`, `tar_mermaid()` diagram), README, `wip/README.md`.
7. **Later, when the user has finished them: integrate the WIP scripts** as sub-analyses
   `outcomes_` and `trends_` (from `pipelines/outcomes.R`, `pipelines/trends.R`): restricted mortality
   file as `format = "file"` target with a clear `cli` error pointing to `data/restricted/README.md`
   if missing; targets' per-target seeds for the random forest; optional `crew` for the ~30 min of
   the trends; the report's external file targets and staleness check then become normal
   dependencies.

### Verification

* `testthat::test_dir("tests/testthat")` green after every step (unit, schema, dependency tests).
* `tar_validate()`, `tar_manifest()`, `tar_visnetwork()` show the intended graph.
* Each sub-analysis builds alone: `tar_make(names = starts_with("<prefix>"))`, then regression
  on frozen inputs: identical outputs for all sub-analyses (all deterministic).
* The WIP scripts run from `wip/` against `data/output/`; outcomes identical, trends reproducible
  with the seed (before the seed: within the documented random-forest spread).
* The report warns when a WIP output is older than its pipeline inputs (test by touching an input).
* A second `tar_make()` without changes skips everything except the metadata cues.
* Missing `data/restricted/tod_nat_gatu.csv` stops the outcomes script (later: the pipeline) with
  the README hint.
* Rendering via `tar_make()` updates `docs/` with the same pages as before.

### Open items outside the plan

* airquality.methods: the small `CLAUDE.md` update from step 1 (regression correction, open items)
  is **not committed** – repo is on `master` with the whole 0.4.0 work uncommitted; the user decides
  where to commit it.
* Later methodological question: year-specific O3 peak-season slope (see decision 6).

## Regression workflow (phase 2a)

Online inputs can change between runs, so a comparison with the committed outputs mixes data and
code changes. Instead, old and new code run on the **same frozen inputs**:

1. before refactoring a topic, in a fresh R session from the project root:
   `source("tests/regression/run_topic.R"); run_topic("<topic>", "reference")`
2. after refactoring: `run_topic("<topic>", "candidate")`
3. `source("tests/regression/compare_outputs.R")`;
   `compare_outputs("tests/regression/results/<topic>/reference", "tests/regression/results/<topic>/candidate")`

`run_topic()` sources the unchanged topic script; `testthat::with_mocked_bindings()` replaces the
network readers of `airquality.methods` (`read_opendataswiss()`, `read_geolion_wfs()`) by a
record/replay version (`tests/regression/inputs/*.rds`, keyed by `rlang::hash()` of the arguments,
`refresh = TRUE` downloads again) and redirects `write_local_csv()` to
`tests/regression/results/<topic>/<label>/`, so `inst/extdata/output/` is never touched. The settings
a topic needs are evaluated from `scripts/_setup.R` (only the listed assignments; no package loading,
no `airquality.data` update). A new topic needs an entry in `topics` (script, settings, `attach` for
packages an old script expects to be attached; add further readers to `network_readers` if it uses
them). Inputs and results are gitignored. Topics whose inputs are `airquality.data` datasets
(monitoring, trends) need no frozen downloads, but both runs must use the same installed version of
that package (`_setup.R` updates it, `run_topic()` does not).
`compare_outputs()` works for all 14 files without configuration: byte identity, header (contract),
rows only in one file (key = non-numeric columns plus integer-valued ones such as `year`), values
that became `NA`, and the largest relative deviation.

Settings that depend on the date (`year_last`, `emis_year_max`) make the reference valid for the
current year only.

## Phase 2a results

### Emissions (2026-09-18)

`scripts/_compile_emission_data.R` is now read → prepare → aggregate → write; all functions in
`R/emissions.R`, tests in `tests/testthat/test-emissions.R` (92 expectations, synthetic data; the plot functions in `tests/testthat/test-plot.R`).
**Regression of the refactoring (commit `f4d941f`): all 4 outputs byte-identical** to the reference
(old code, same frozen inputs); the reference itself is byte-identical to the committed outputs
(online data unchanged). The subsector grouping (below) then changed `data_emissions.csv` on purpose;
the three RSD files stay byte-identical.

| old | new |
|---|---|
| `prepare_emmissions(data, filter_args = <quoted expression>)` | `prepare_emissions(data, canton, exclude_subsectors, year_max)` |
| `aggregate_emmissions()` incl. plot colours and an unused automatic regrouping `groups_emission_subsector()` (the script always passes the lookup table) | `aggregate_emissions(data, subsector_new)` + `group_minor_subsectors()` (new rule, see below) + `add_emission_colours(data, sector_colours)` |
| `prepare_rsd(data, rsd_auxiliary)` with hidden `lubridate::year(Sys.Date())` as newest vehicle model year | `prepare_rsd(data, meta, filters, model_year_max)`, script passes `emis_year_max` (same value: current year) |
| `aggregate_rsd_nox(data, rsd_auxiliary, groups)` + `aggregate_rsd()` | `aggregate_rsd_nox(data, meta, filters, groups)` |
| `source` hard-coded in the aggregation | taken from the data (`read_opendataswiss(source = )`) |
| `tidyr::spread()`, `group_by()`/`ungroup()`, `as.numeric()` coercion warnings to detect model years | `pivot_wider()`, `.by`, `is_model_year()` |

Findings:
* **Plot order and colours are ranked without projections** (user decision 2026-09-19): the inventory
  contains 2030/2040/2050; `prepare_emissions(year_max = emis_year_max)` now drops them first, so
  every later step only sees published years. Before, they were ranked over all years and dropped
  afterwards. With the current data the ranking is identical either way (outputs still
  byte-identical). A first experiment (2026-09-18) wrongly showed a difference because it read
  `emikat_subsector_new.csv` without `locale(encoding = "UTF-8")`: then only 19 of 32 lookup
  entries match (umlauts). The script passes the locale.
* The automatic regrouping `groups_emission_subsector()` (removed) no longer ran with
  `airquality.methods` 0.4.0 at all (grouped data frame passed to `aggregate_groups()`).
* The RSD aggregation completes all combinations of the factor levels (`vehicle_type`,
  `vehicle_fuel_type`) with `n = 0` (`airquality.methods::aggregate_groups()`); in the current
  outputs this only adds two empty rows (light duty vehicles, model year 2024).

**Subsector grouping: lookup table + per-pollutant rule** (user decision 2026-09-19). Criteria
(user): at most 4 subsectors per pollutant and sector **including** "verschiedene"; subsectors with
a mean yearly share < 5 % of the pollutant's canton total go into "verschiedene" (mean over all
published years, so a pollutant keeps the same groups over its whole time series); the lookup table
lists all subsectors. Implementation:
* `emikat_subsector_new.csv` lists all 35 subsectors and only merges/renames thematically
  (Feuerungen Holz & Kohle / Öl & Gas, Lösungsmittel, Flugverkehr, Kehrichtverbrennungsanlagen –
  typo "…anagen" corrected –, Stall-Laufhof & Hofdünger-Lager, Hofdüngerausbringung & Weiden); no
  entry maps to "verschiedene" any more. `aggregate_emissions()` warns about subsectors missing in
  the table (e.g. new ones in a future inventory).
* `group_minor_subsectors(min_share = emis_subsector_min_share, max_per_sector =
  emis_subsectors_max)` (settings 0.05 and 4 in `_setup.R`): per pollutant and sector, mean yearly
  share < 5 % → "verschiedene" (years without emission count as 0); if a "verschiedene" group
  exists or there are more than 4 subsectors, only the 3 largest keep their name.
* Result 2026-09-19 (frozen inputs): `data_emissions.csv` 3996 → 2772 rows, totals per year,
  pollutant and sector unchanged (≤ 4e-16), max. 4 groups, no named group < 5 %. Now named, e.g.
  Zonenverkehr (CO 28 %, NMVOC 15 %), Flächenquellen Industrie (SO2 18 %, PM 9 %), Landwirtschaftliche
  Nutzflächen (NH3 7 %); now in "verschiedene", e.g. Feuerungen Öl & Gas for PM/eBC/NH3/CO. Some
  sector/pollutant combinations consist of "verschiedene" only (e.g. NOx Industrie 11 %, made of
  subsectors each < 5 %).
* **Restore point:** the lookup-only state is commit `f4d941f`, git tag `emissions-lookup-only`
  (restore files with `git checkout emissions-lookup-only -- <files>`).

Analysis of the lookup-only state that led to this decision (2026-09-19). Shares = share of the
pollutant's canton total, published years 1990–2025, per pollutant (the plots are per pollutant):
* coverage: 35 subsectors in the data, 32 in the lookup; missing (keep their name):
  `Strassenverkehr`, `Baumaschinen` (Industrie), `Wälder` (natürl. Emissionen)
* max. 4 per sector: **fulfilled** (per pollutant max. 4: NMVOC Haushalte and Industrie; over all
  pollutants Haushalte and Industrie 4, the others ≤ 3)
* small ones in "verschiedene": **not fulfilled**, and a single table for all pollutants cannot
  fulfil it. 39 named pollutant/subsector combinations stay below 5 % in every year (e.g.
  `Feuerungen Öl & Gas` for PM, eBC, NH3, CO), while 14 subsectors in "verschiedene" reach ≥ 5 % in
  some year (`Zonenverkehr` CO up to 35 %, 2024: 23 %; `Flächenquellen Industrie` SO2 up to 27 %;
  `Haushalte andere Private etc` PM2.5 12 %; `Schienenverkehr Bau-/Dienstzüge` PM10 11 %;
  `Landwirtschaftliche Nutzflächen` NH3 10 %)
* the old automatic method was per pollutant (top 2 per pollutant and sector, plus < 5 % of the
  pollutant total into "verschiedene"; means over all years); lookup first and automatic second
  (the suspected combination) gives 1–3 per pollutant and sector.

**Input checks** (2026-09-19): `check_columns()` (R/helpers.R) and `check_rsd_filters()` stop with
a `cli` error of class `airquality_input_error` that names the dataset and what is missing (raw
inventory and RSD columns, lookup table, RSD metadata, filter criteria exactly once, measured
parameters NO/CO2/CO/HC/velocity/acceleration). Before, a missing filter criterion yielded an empty
bound and filtered silently. Outputs byte-identical.

**Grouped legend for the emission plots** (user decisions 2026-09-19: `legendry` as intended –
one legend on the right, no columns; block titles plain and as large as the entries; keys without
gaps as in a ggplot legend). `ggplot_emissions()` shows one block per sector with the sector as
title and the subsectors without the pasted sector (was "Sektor / Subsektor"); the subsector names
in `emikat_subsector_new.csv` were shortened accordingly by the user (e.g. "Private etc",
"Landw. Nutzflächen"). Generic functions in `R/plot.R` (to move to `airquality.methods` later):
* `grouped_key(group, key, order, group_order)` – unique key `"group::key"` as factor; its levels
  set the order of stack and legend ("verschiedene" exists in several sectors)
* `add_grouped_legend(plot, aesthetic, sep, spacing, subtitle, key_spacing)` – sets
  `legendry::guide_legend_group(key_group_split(sep))` for the aesthetic plus the theme
  (`legendry.legend.subtitle` from the plot's `legend.text`, `legendry.group.spacing` 3 mm,
  `legend.key.spacing.y` 0). The plot stays an ordinary ggplot (later `+ theme()` still applies).
The NH3 special case in `_plot_airquality.R` (agriculture last) is the argument
`ggplot_emissions(sectors_last = )` instead of `%+%` on the finished plot.
Rejected: blocks in two columns (commit `3f759b1`, restore point). `legendry` arranges blocks only
in one row or column (its `ncol`/`nrow` arrange the keys *within* a block, `nrow` fails in 0.3.0),
so the columns had to be drawn separately and placed with `ggplot2::guide_custom()`: legend twice
as wide, fixed when drawn (no later theme changes), and building grobs needed a temporary `ragg`
device (a pdf device leaves `Rplots.pdf` and does not know Arial from `theme_ts`). With the native
legend, NMVOC (6 blocks, 11 entries) just fits a 5 in high figure – check when figures get smaller.
Findings: `longpollutant()` is called without the `airquality.methods::` prefix in several
functions of `R/plot.R` (works only while the package is attached) – fixed in `ggplot_emissions()`,
the others belong to the plots topic. Within one pollutant, neighbouring subsectors of a sector can
get similar shades (the ramp is assigned over all pollutants), e.g. PM2.5 Haushalte – possible
later improvement.

### Monitoring (2026-09-19)

`scripts/_compile_monitoring_data.R` is 33 lines (was 127, with 7 helper functions defined inside);
all functions in `R/monitoring.R`, tests in `tests/testthat/test-monitoring.R` (40 expectations,
synthetic data). Cut along the later targets sub-analyses `mon_aq_` and `mon_ndep_`. Inputs are
package datasets of `airquality.data` 0.1.3 (no network). **Two bugs were fixed first (own commit
`c390948`), then the refactoring reproduced the fixed outputs byte-identically.**

| old | new |
|---|---|
| `data_monitoring_aq_y1` filtered with `canton == "ZH" \| site %in% c("Zürich-Kaserne", "Dübendorf-EMPA")` | `prepare_monitoring_airquality(data, cantons = mon_cantons)`; the site names were redundant (both NABEL sites carry `canton = "ZH"`), output unchanged |
| `recode_ecosys()`, `ostluft_siteclass()`, `cut_emissions_1km()`, `derive_source_cat()`, `cut_estimated()`, `cut_frac_estimated()`, `aggregate_ndep()` defined inside the script | `recode_ecosystems()`, `classify_ostluft_siteclass()`, `classify_nh3_emission()`, `derive_source_category()`, `classify_estimated()`, `classify_frac_estimated()` (all on `classify()`), `prepare_ndep_site_meta()`, `prepare_ndep_parameters()`, `aggregate_ndep()` in `R/monitoring.R` |
| unprefixed `mutate()`/`left_join()` (only work while dplyr is attached), `group_by_at()` | prefixed, `.by` + `arrange()` (same row order as the sorting `group_by()`) |
| hard-coded canton | setting `mon_cantons` in `_setup.R` |
| class thresholds inside the script | documented defaults of the classification functions (method constants, like `fit_pm_ratio()`) |
| – | input checks (`check_columns()`) for the three datasets; `recode_ecosystems()` warns about unknown ecosystem types instead of silently returning `NA` |
| `aggregate_nitrogen_deposition()`, `simplify_nitrogen_parameters()` (old ndep approach, unused) | removed |

Fixes (commit `c390948`, `data_airquality_monitoring_y1.csv` unchanged):
* **Site class typo**: `ostluft_siteclass()` had the factor levels `c("hoch", "mittel", "tiel")`, so
  every "tief" became `NA` – 14 of 132 rows in `data_ndep_monitoring_y1.csv`, 98 of 924 in the
  parameter file.
* **Ecosystem "Siedlungen"** (only site WIE, `cln = NA`) became `NA`; user decision 2026-09-19: it
  counts as "kein empf. Ökosys." (14 / 98 rows). Note: the committed outputs still contained
  "Siedlungen", i.e. they had been written with an older code version than the current script.
* Neither fix changes a value (`max_abs_rel` 0), and neither reaches the plots: `_plot_airquality.R`
  drops ecosystems that are `NA`/"Siedlungen" and rows without `cln`, and overwrites the site class.

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

* `scripts/_compile_exposition_data.R` and `R/exposition.R` – the reworked chain (step 1).
* `scripts/_compile_emission_data.R` and `R/emissions.R` – the first topic of phase 2a, the pattern for the next topics.
* `tests/regression/` – how new results are checked against the old ones.
* `../airquality.methods/CLAUDE.md` – the function library and its migration table.
