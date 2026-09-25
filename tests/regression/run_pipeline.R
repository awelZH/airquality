# Run the targets pipeline on frozen inputs and write its outputs to a separate directory (phase 2b; before,
# run_topic.R ran the topic scripts the same way).
#
# The network readers of airquality.methods (incl. the opendata.swiss metadata) replay the inputs of
# tests/regression/inputs/ (recorded on first use, gitignored; refresh = TRUE downloads again), the outputs and
# logs go to tests/regression/results/pipeline/<label>/{output,log}/ (environment variables of settings.R)
# and the targets store to .../_targets, so data/output/, data/log/ and _targets/ are never touched. The report
# is not built.
# tar_make() runs in this session (callr_function = NULL), so the mocked readers apply.
#
# compare with the outputs of the old scripts, e.g.
#   source("tests/regression/compare_outputs.R")
#   compare_outputs("tests/regression/results/emissions/reference", "tests/regression/results/pipeline/candidate/output")
#
# run from the project root in a fresh R session, e.g.
#   Rscript -e 'source("tests/regression/run_pipeline.R"); run_pipeline("candidate")'

regression_dir <- "tests/regression"

# wrap a reader so that its result is stored on first use and replayed afterwards (key: hash of the arguments)
frozen <- function(fun, name, input_dir, refresh) {
  force(fun)
  function(...) {
    file <- file.path(input_dir, paste0(name, "_", rlang::hash(list(...)), ".rds"))
    if (file.exists(file) && !refresh) return(readRDS(file))
    cli::cli_inform("Downloading input for {.fn {name}} -> {.file {file}}")
    result <- fun(...)
    saveRDS(result, file)
    result
  }
}

run_pipeline <- function(label, refresh = FALSE) {
  result_dir <- file.path(regression_dir, "results", "pipeline", label)
  input_dir <- file.path(regression_dir, "inputs")
  dir.create(input_dir, showWarnings = FALSE, recursive = TRUE)
  withr::local_envvar(
    AIRQUALITY_OUTPUT_DIR = file.path(result_dir, "output"),
    AIRQUALITY_LOG_DIR = file.path(result_dir, "log")
  )

  ns <- asNamespace("airquality.methods")
  network_readers <- c("read_opendataswiss", "get_opendataswiss_resources", "read_geolion_wfs")

  testthat::with_mocked_bindings(
    targets::tar_make(
      names = !tidyselect::starts_with("report_"),
      store = file.path(result_dir, "_targets"),
      callr_function = NULL,
      reporter = "balanced"
    ),
    !!!purrr::map(rlang::set_names(network_readers), \(name) frozen(ns[[name]], name, input_dir, refresh)),
    .package = "airquality.methods"
  )

  cli::cli_alert_success("pipeline: outputs written to {.file {file.path(result_dir, 'output')}}")
  invisible(result_dir)
}
