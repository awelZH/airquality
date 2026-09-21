# Regression workflow (phase 2a)

How refactorings are checked against the old code on frozen inputs.

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
`tests/regression/results/<topic>/<label>/`, so `inst/extdata/output/` is never touched. Settings
are evaluated from `scripts/_settings.R` and `scripts/_setup.R`, in this order (only the listed
assignments; no package loading, no `airquality.data` update). A new topic needs an entry in
`topics` (script, settings, `attach` for packages an old script expects to be attached; further
readers in `network_readers`). Inputs and results are gitignored. Topics whose inputs are
`airquality.data` datasets (monitoring, trends) need no frozen downloads, but both runs must use the
same installed version of that package. Topic `exposition` (since 2026-09-21): the municipality map
is frozen, the rasters are not (cached downloads or streamed GeoTIFFs from data.geo.admin.ch), so
run reference and candidate shortly after each other; its log file
`exposition_derivation_coefficients.csv` carries the run time and always differs.
Settings that depend on the date (`year_last`, `emis_year_max`) make a reference valid for the
current year only.

`compare_outputs()` works for all 14 files without configuration: byte identity, header (contract),
rows only in one file (key = non-numeric columns plus integer-valued ones such as `year`), values
that became `NA`, and the largest relative deviation.

**Figures** (since 2026-09-21, plots/report rework): `tests/regression/run_plots.R`.
`run_plots("reference", global = TRUE)` before, `run_plots("candidate")` after, then
`compare_plots("reference", "candidate")`. It sources `scripts/_plot_setup.R` and the plot scripts of
emissions, monitoring and exposition on the output CSVs in `inst/extdata/output/` (keep them unchanged
in between), replays the municipality map like `run_topic()` and saves every row of the plot tibbles as
a PNG (ragg, 7 × 5 in, 96 dpi) to `tests/regression/results/plots/<label>/`; the comparison reports byte
identity, then pixel identity. Without `global = TRUE` the scripts run in their own environment, so a
function that still reads a setting from the global environment fails. Two runs of the same code are
byte-identical (295 figures, about 2 min per run). Row order matters: jittered points (with seed) and
overlapping bars or slices are drawn in the row order of the data.
