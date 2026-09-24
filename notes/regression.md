# Regression workflow

How refactorings are checked against the old code on frozen inputs.

Online inputs can change between runs, so a comparison with the committed outputs mixes data and
code changes. Instead, old and new code run on the **same frozen inputs**.

**Pipeline (since phase 2b, 2026-09-24)**:

1. before refactoring: `source("tests/regression/run_pipeline.R"); run_pipeline("reference")`
2. after refactoring: `run_pipeline("candidate")`
3. `source("tests/regression/compare_outputs.R")`;
   `compare_outputs("tests/regression/results/pipeline/reference/output", ".../candidate/output")`

`run_pipeline()` runs `targets::tar_make()` in the current session (`callr_function = NULL`) without the
report targets; `testthat::with_mocked_bindings()` replaces the network readers of `airquality.methods`
(`read_opendataswiss()`, `read_geolion_wfs()`) by a record/replay version (`tests/regression/inputs/*.rds`,
keyed by `rlang::hash()` of the arguments, `refresh = TRUE` downloads again). The outputs and logs go to
`tests/regression/results/pipeline/<label>/{output,log}/` (environment variables `AIRQUALITY_OUTPUT_DIR`,
`AIRQUALITY_LOG_DIR` of `settings.R`) and the store to `.../_targets`, so `data/` and `_targets/` are never
touched; a second run with the same label reuses its store (only the `tar_cue("always")` targets run).
Inputs and results are gitignored. `airquality.data` datasets need no frozen downloads, but both runs must
use the same installed version of that package. The rasters of data.geo.admin.ch are not frozen (cached
downloads or streamed GeoTIFFs), so run reference and candidate shortly after each other. The hash of the
arguments depends on the string encoding: a call from a pipeline file can miss a recording made by an old
script with the same arguments and download again (happened for the RSD data on 2026-09-24; same data).
Settings that depend on the date (`year_last`, `emis_year_max`) make a reference valid for the current
year only.

**Before phase 2b** the topic scripts were checked the same way with `run_topic()` (removed with the
scripts in `5220a3e`; the migration to the pipeline was checked against its outputs of the old scripts,
11 of 12 files byte-identical, the health outcomes within 4.5e-11).
`compare_outputs()` works for all 14 files without configuration: byte identity, header (contract),
rows only in one file (key = non-numeric columns plus integer-valued ones such as `year`), values
that became `NA`, and the largest relative deviation.

**Figures** (since 2026-09-21, plots/report rework): `tests/regression/run_plots.R`.
`run_plots("reference", global = TRUE)` before, `run_plots("candidate")` after, then
`compare_plots("reference", "candidate")`. It sources `report/plots/_plot_setup.R` and the plot scripts of
all five topics (`topics =` to restrict) on the output CSVs in `data/output/` (keep them unchanged
in between), replays the municipality map like `run_pipeline()` and saves every row of the plot tibbles as
a PNG (ragg, 7 × 5 in, 96 dpi) to `tests/regression/results/plots/<label>/`; the comparison reports byte
identity, then pixel identity. Without `global = TRUE` the scripts run in their own environment, so a
function that still reads a setting from the global environment fails. Two runs of the same code are
byte-identical (304 figures, about 2 min per run). Row order matters: jittered points (with seed) and
overlapping bars or slices are drawn in the row order of the data.

`compare_plots_content()` compares two runs by content (every figure must have a byte-identical one in
the other run, file names ignored), for changes that rename plots. The page check without rendering is
`tests/regression/check_pages.R` (`check_pages()`).
