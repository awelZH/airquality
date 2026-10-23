# Project decision log

Architecture decisions with their reasons and numbers; `CLAUDE.md` holds a one-line summary of each.
Read this file before changing anything these decisions cover.

## The one recurring lesson

**Make every step visible and recomputable.** Scripts that communicate through global variables,
append to their own outputs and derive their work list from earlier results (`get_years()`) are hard
to debug and silently carry old errors forward: the double counting of exclave cells (see
`findings_exposition.md`) survived for years because every run only appended new years.

## Architecture decisions and why

**1. `airquality.methods` from GitHub** (0.11.0 since 2026-10-23; 0.11.0: the censuses 1990 and 2000 read by `read_statpop_ha()`, upgrade from 0.5.3 byte-identical on frozen inputs (12 outputs, 645 figures); 0.5.2: GeoTIFFs without EPSG code accepted when the STAC metadata agree; 0.5.3: markdown legend titles of `immissionscale()` via ggtext). Until 2026-09-24 installed from the local repo;
since then from GitHub, now `awelZH/airquality.methods@bc41d8e` (pinned in `renv.lock`, `renv::install("awelZH/airquality.methods@<sha>")`
for a newer version). Since 0.4.0 only its exports may carry the `airquality.methods::` prefix.
**After (re)installing `airquality.methods`, restart the R session** before running the pipeline:
`library()` does not reload an already loaded namespace. Check with
`getNamespaceVersion("airquality.methods")`.

**2. Exposition is recomputed completely on every run** (2026-09-18). Downloads are cached by
`airquality.methods` (`geo_admin_cache_dir()`), so a full run is cheap. The four exposition CSVs are
overwritten, not appended. `get_years()` and `read_all_raster` are gone.
Exception: **uncompressed GeoTIFFs are never cached** – they are streamed through GDAL `/vsicurl/`
(only the canton window is transferred), so they are read from the web on every run; csv/parquet
and compressed assets are cached. Every `Reading "<item>" (<format>, <source>).` message says which
applies: `streamed from the web`, `from cache` or `downloading` (`inform_reading()` in
`airquality.methods`, 2026-09-21).
**Downloads only when the version changed** (user decision 2026-09-25). The tables of opendata.swiss (emission
inventory 241 MB, RSD 22 files of together 905 MB, population by age 71 MB) were downloaded on every run
(about 30 s here, about 1.2 GB). Now a state target per dataset (`emis_emikat_state`, `emis_rsd_state`,
`outcomes_population_state`, `opendataswiss_state()` on `airquality.methods::get_opendataswiss_resources()`,
0.5.1) returns `download_url`, `modified` and `byte_size` of the resources that are read, with
`tar_cue("always")`; the download targets depend on it and rerun only when it changed, like the rasters
behind `geo_admin_asset_state()`. opendata.swiss publishes no checksum (`hash` is empty), so a file replaced
without new `modified` or size goes unnoticed. The geolion WFS map (5 MB) has no such metadata and is still
read on every run. Measured 2026-09-25: a run of the outputs without changes takes 21 s; the three
downloads it skips took 28 s (21.3 + 4.7 + 2.2 s) in the run before.

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
  removed from the map in `pipelines/setup.R` (`drop_foreign_enclaves()`, 1 cell / 23 inhabitants in 2024)
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
update (O3 peak season up to 0.9 %, PM2.5 up to 0.5 %). Every refit appends its coefficients to
`data/log/exposition_derivation_coefficients.csv` (not part of the contract; in the pipeline only when the monitoring data change). A year-specific
O3 slope would decouple the years but is less certain with 7–15 sites per year; the common slope stays
(user decision 2026-09-24).

**7. All analysis constants live in `settings.R`** (2026-09-18; own file since 2026-09-21, in the root
since phase 2b, with the paths; sourced by `_targets.R`, the plot scripts and the report), grouped by topic:
* names carry the topic as prefix (`emis_`, `mon_`, `trend_`, `expo_`, `plot_`), general settings
  without (`year_offset`, `year_last`, `base_scenario_year`, `crs`); no `config.yml` (phase 2b decision 8)
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

**8. Inputs are checked where they enter** (2026-09-19). `check_columns()` (R/helpers.R, a wrapper
around `airquality.methods::check_names()` since 2026-09-21) and the
topic-specific checks (`check_rsd_filters()`) stop with a `cli` error of class
`airquality_input_error` naming the dataset and what is missing. Reason: online sources and
`airquality.data` change between the twice-yearly updates; before, a missing filter criterion
silently filtered everything or nothing.

**9. The report builds its plots while rendering; no plot rds files** (2026-09-21). Each Quarto
page sources `report/plots/_plot_setup.R` and its topic script(s) inside `withr::with_dir("..", …)`.
Not knitr's `root.dir: ".."`: the year tabs are knitted inline (`knitr::knit(text = …)`) and would
write their figures into the project root, while the HTML points to `docs/`. The same scripts work
in the console (`source("report/plots/_plot_setup.R"); source("report/plots/_plot_exposition.R")`, then
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
*Extension (user decision 2026-09-25):* data outside the output contract that the report needs (the
pollutant maps of the canton and their verification, `pipelines/monitoring_maps.R`) are **data targets**
in the store, read by the plot script with `targets::tar_read(<target>, store = path_store)`; still no
plot targets. `path_store` (`settings.R`, `AIRQUALITY_STORE_DIR`) lets the regression point the plot
scripts to its own store. `report_site` lists these targets as dependencies. Consequence: the plot
script `_plot_monitoring.R` needs a built store, in the console as well.

**10. Generic building blocks live in `airquality.methods`** (user decision 2026-09-21). Moved there
unchanged and exported: `drop_foreign_enclaves()`, `assign_municipalities()` (decisions 3/4),
`noloc_from_aligned()`, `redistribute_noloc()` (decision 5; subtracting in `read_statpop_ha()` and
giving back now live in one package, so nobody who reads STATPOP with `correct_noloc = TRUE` loses
the 0.44 % silently), `grouped_key()`, `add_grouped_legend()` (`legendry` is a Suggests there).
Replaced instead of moved: `check_columns()` stays here as a one-line wrapper around the now
exported `airquality.methods::check_names(class = )` (it adds the error class and returns the data
for pipes); `append_log()` is gone, `write_local_csv(append = TRUE)` now creates missing
directories and writes the header when the file is new. Stays here: `round_population()` (output
formatting of this analysis). Regression on frozen inputs: all 11 outputs of emissions, monitoring
and exposition byte-identical, the 19 emission figures pixel-identical.
Moved as well with 0.5.0 (user decision 2026-09-25), because other analyses and reports use them: the
classes of the nitrogen deposition (`recode_ecosystems()`, `classify_ostluft_siteclass()`,
`classify_nh3_emission()`, `classify_estimated()`, `classify_frac_estimated()`,
`derive_source_category()`; `ndep.ostluft` had older copies of them with two bugs) and the plot catalog of
the report (`plot_catalog()`, `catalog_entries()`, `get_plot()`, `print_tabset(level = 5)`). Unchanged except
the error class of the catalog (`plot_catalog_error`, was `airquality_plot_error`). Stays here:
`print_year_slider()` (needs `report/year-slider.html` and German tab titles), `aggregate_ndep()` and
`prepare_ndep_*()` (column schema of this analysis).

**11. Plots per year are shown with a year slider** (user decision 2026-09-21). The year tabsets were a
workaround to click through the years (16–20 tabs per plot). `print_year_slider(catalog, plot,
parameter)` in a chunk with `#| output: asis` prints every year as an ordinary knitr figure (files,
lightbox as before) into a `.year-panel` div; `report/year-slider.html` (included after the body of every
page via `_quarto.yml`) puts a range input above them and shows the chosen year only; without
JavaScript all years stay visible. Order: "alle" (all years in one plot) first, then the years
ascending; the slider starts at "alle" where it exists, else at the newest year. Rejected: Observable JS
(images built in the browser, no lightbox), plotly animation (`quickText()` formulas lost, maps as JSON
of several MB each). The "absolut/relativ" tabs of the outcomes stay tabs (`print_tabset()`).

**12. Exposition from 1990: the censuses 1990 and 2000 as population rasters** (user decisions 2026-10-23).
`ch.bfs.statistik-bevoelkerung_haushalte` also holds the federal censuses 1990 and 2000 (LV03 csv only,
own column names, no collector pixels). `airquality.methods` 0.11.0 reads them with `read_statpop_ha()` onto
the STATPOP grid (LV03 shifted by the false origins of LV95, the BFS convention for the hectare grid; cells
with 0 inhabitants emptied), so nothing in `R/` reads them separately (methods decision 18). Decided:
`expo_years <- c(1990, 2000, 2010:year_last)`, not `1990:year_last`: there are no population rasters in
between, and `align_to_reference()` only computes years with one, so the longer form would read about 47
pollutant rasters for nothing. No interpolation of the population for 1991–1999 and 2001–2009 (a new method,
not wanted for now). O3 peak season 1990: first `NA` (coefficients from 1992 on, no extrapolation;
`derive_o3_peakseason()` reports years before the first fit as a message, later gaps still warn); the same
day the user lowered `expo_o3_nmin_sites` from 7 to 4, so 1990 (4 sites) and 1991 (5) get their own offset
(114, 117; 1992–1994: 107–110) in the common model. Considered and not taken: offsets of 1990/1991 at the
fixed slope of the years with ≥ 7 sites (same offsets, nothing else changes). The common slope moves from
−0.6621 to −0.6642, the O3 peak season of every year by at most 0.1 % (2010 canton −0.003 µg/m3; also the
health outcomes of O3). PM2.5 2000
derived from PM10 (`expo_years_pm25_from_pm10 <- intersect(expo_years, 2000:2014)`, ratios from 2000 on).
Health outcomes 2000 included (mortality from 1996); premature deaths only, because the population by age
(life tables) starts in 2010. Prototype, numbers and checks: `notes/findings_exposition.md`.

**13. The register of the data sources: a CSV with a clear schema, read by key** (user decisions 2026-10-23).
`data/meta/ressources.csv` documents every source and gives the code its endpoints. It had grown two columns
for the description, two for the provider, a `DOWNLOAD_URL` that was sometimes a folder completed by
`DATASET_NAME` (itself a file name, an R object, a collection id or an R expression), and the code asked for
numbers (`filter_ressources(r, 27)`) without checking that one row matched. Now one column per meaning
(schema in `data/meta/README.md`: `ID`, `KEY`, `TITLE`, `DESCRIPTION`, `PROVIDER`, `ACCESS`, `SOURCE_TYPE`,
`ENDPOINT`, `LANDING_PAGE`, `FORMAT`, `LICENSE`, `UNIT`, `TEMPORAL`, `USED_BY`); `ENDPOINT` is exactly what the
code uses, so `prepare_ressources()` only checks the columns; `filter_ressources(r, "emikat")` stops unless
exactly one row has the key. The geo.admin collection ids stay constants in `R/exposition.R`;
`tests/testthat/test-ressources.R` checks that code and register agree (keys, collections, files in
`data/meta/`, `airquality.data` objects, `USED_BY`; `USED_BY` is empty exactly for `reference` rows). Only
stable facts are written by hand: years, resolution per year and version change with every data release and
are derived by the pipeline (step 1b), never written back into the input. Rejected: a YAML register (more
expressive, but not a table on GitHub); later possibly a Frictionless Data Package for the 14 output files.
Migration 2026-10-23 byte-identical on frozen inputs (12 outputs, 720 figures); content completion follows.
**Inventory** (`R/sources.R`, target `report_sources_inventory`): per key the years available, the cell size per
year condensed to ranges and the version, derived from targets that exist anyway – the geo.admin asset states,
the data read (`emis_emikat_raw`, `emis_rsd_vehicles`, `outcomes_population`, `outcomes_mortality`, `mon_aq_raw`,
`mon_ndep_raw`), the cell sizes of the maps (`read_pollutant_maps()` returns `grids`; no `gsd` in the STAC
metadata) and of `expo_pop_cells`/`expo_eco_ndep`, the `modified` of opendata.swiss and the version of
`airquality.data`. Logged to `data/log/sources_inventory.csv` only when the inventory differs from the last logged
run, with a message naming the keys added, changed or removed (also a rerun of the target for another reason, e.g.
a changed function, appends nothing); shown with the register on "Allgemeines" (`sources_table()`).
First inventory: NO2 200 m 1990–2019, 20 m 2020–2025; PM10, O3 200 m → 100 m from 2020; PM2.5 100 m from 2015;
STATPOP 1990, 2000, 2010–2025 at 100 m; critical loads 1 km 1990–2020 (6 model years); emissions 1990–2025 plus
the projections 2030, 2040, 2050; RSD 2002–2021, 2023, 2025.
**Content** (2026-10-23): titles, descriptions, units and temporal resolution in German (shown on the website),
providers without urls (the Statistisches Amt is now the Amt für Statistik und Daten), landing pages to opendata.swiss
or the files on GitHub. Licenses from the metadata of the sources: the three opendata.swiss packages and the
geolion municipality boundaries (opendata.swiss package `gemeindegrenzen3`) `terms_open`; the geo.admin collections
of BFS and the critical loads link `terms_by`, the four BAFU map collections only say `proprietary` without link,
recorded as the general geo.admin terms (free use, attribution required); `airquality.data` and this repository GPL-3.
The rows 25 (death rate) and 26 (cohort life tables of the BFS) are removed, no code read them (user decision); the
type `reference` stays for documentation-only sources, none at the moment. Removed ids are not reused.
