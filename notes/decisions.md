# Project decision log

Architecture decisions with their reasons and numbers; `CLAUDE.md` holds a one-line summary of each.
Read this file before changing anything these decisions cover.

## The one recurring lesson

**Make every step visible and recomputable.** Scripts that communicate through global variables,
append to their own outputs and derive their work list from earlier results (`get_years()`) are hard
to debug and silently carry old errors forward: the double counting of exclave cells (see
`findings_exposition.md`) survived for years because every run only appended new years.

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

**8. Inputs are checked where they enter** (2026-09-19). `check_columns()` (R/helpers.R, a wrapper
around `airquality.methods::check_names()` since 2026-09-21) and the
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
