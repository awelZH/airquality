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
