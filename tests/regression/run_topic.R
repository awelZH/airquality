# Run one topic script of the analysis on frozen inputs and write its outputs to a separate directory.
#
# Used in phase 2a to check that refactoring a topic does not change its results:
# 1. before refactoring: run_topic("emissions", "reference") downloads the online inputs once
#    (tests/regression/inputs/, gitignored) and writes the outputs of the current code
# 2. after refactoring:  run_topic("emissions", "candidate") replays the same inputs
# 3. compare: source("tests/regression/compare_outputs.R");
#    compare_outputs("tests/regression/results/emissions/reference", "tests/regression/results/emissions/candidate")
#
# The script itself runs unchanged: the network readers of airquality.methods are replaced by a
# record/replay version and write_local_csv() is redirected to tests/regression/results/<topic>/<label>/,
# so inst/extdata/output/ is never touched. Packages the old script needs attached are listed in `attach`.
# Only the settings the topic needs are taken from
# scripts/_setup.R (package loading and the airquality.data update are skipped).
#
# run from the project root in a fresh R session, e.g.
#   Rscript -e 'source("tests/regression/run_topic.R"); run_topic("emissions", "reference")'

regression_dir <- "tests/regression"

topics <- list(
  emissions = list(
    script = "scripts/_compile_emission_data.R",
    setup = c("ressources", "year_offset", "year_last", "emis_year_max", "emis_subsector_min_share", "emis_subsectors_max")
  ),
  monitoring = list(
    script = "scripts/_compile_monitoring_data.R",
    setup = c("ressources", "mon_cantons"),
    attach = "dplyr" # the old script called mutate() and left_join() without prefix
  )
)

# evaluate the top-level assignments `name <- ...` of scripts/_setup.R for the given names, in file order
eval_setup <- function(names, env, file = "scripts/_setup.R") {
  assigned <- character()
  for (e in parse(file, encoding = "UTF-8")) {
    if (rlang::is_call(e, "<-") && rlang::is_symbol(e[[2]]) && as.character(e[[2]]) %in% names) {
      eval(e, env)
      assigned <- c(assigned, as.character(e[[2]]))
    }
  }
  missing <- setdiff(names, assigned)
  if (length(missing) > 0) cli::cli_abort("Not assigned in {.file {file}}: {.val {missing}}.")
  invisible(env)
}

# wrap a reader so that its result is stored on first use and replayed afterwards
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

run_topic <- function(topic, label, refresh = FALSE) {
  spec <- topics[[topic]]
  if (is.null(spec)) cli::cli_abort("Unknown topic {.val {topic}}; known: {.val {names(topics)}}.")

  input_dir <- file.path(regression_dir, "inputs")
  output_dir <- file.path(regression_dir, "results", topic, label)
  dir.create(input_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  library(airquality.methods) # the old code calls some of its exports without prefix
  for (package in spec$attach) library(package, character.only = TRUE)
  devtools::load_all(quiet = TRUE)

  ns <- asNamespace("airquality.methods")
  write_local_csv <- ns$write_local_csv
  network_readers <- c("read_opendataswiss", "read_geolion_wfs")

  env <- new.env(parent = globalenv())
  testthat::with_mocked_bindings(
    {
      eval_setup(spec$setup, env)
      source(spec$script, local = env, encoding = "UTF-8")
    },
    !!!purrr::map(rlang::set_names(network_readers), \(name) frozen(ns[[name]], name, input_dir, refresh)),
    write_local_csv = function(data, file, ...) write_local_csv(data, file.path(output_dir, basename(file)), ...),
    .package = "airquality.methods"
  )

  cli::cli_alert_success("{topic}: outputs written to {.file {output_dir}}")
  invisible(output_dir)
}
