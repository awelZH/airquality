# Build the report plots of the given topics and save every plot as a PNG, to check that refactoring the
# plot code does not change the figures.
#
# 1. before refactoring: run_plots("reference", global = TRUE) (old code that reads globals)
# 2. after refactoring:  run_plots("candidate")
# 3. compare:            compare_plots("reference", "candidate")
#
# The plot scripts run unchanged on the output CSVs in data/output/ (not touched in between);
# the municipality map from geolion is replayed from tests/regression/inputs/ like in run_topic.R.
# Figures go to tests/regression/results/plots/<label>/ (gitignored), one file per row of the plot
# tibbles, named <type>_<source>_<pollutant>_<year>.png and drawn with ragg at a fixed size.
#
# run from the project root in a fresh R session, e.g.
#   Rscript -e 'source("tests/regression/run_plots.R"); run_plots("reference")'

source("tests/regression/run_topic.R")

plot_topics <- c("emissions", "monitoring", "exposition", "outcomes", "trends")

run_plots <- function(label, topics = plot_topics, global = FALSE, refresh = FALSE, width = 7, height = 5, res = 96) {
  input_dir <- file.path(regression_dir, "inputs")
  output_dir <- file.path(regression_dir, "results", "plots", label)
  unlink(output_dir, recursive = TRUE)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  ns <- asNamespace("airquality.methods")
  # global = TRUE for code whose functions look up settings (pointsize, theme_ts, ...) in the global
  # environment, as the plot functions did before their rework; otherwise hidden globals fail here
  env <- if (global) globalenv() else new.env(parent = globalenv())
  testthat::with_mocked_bindings(
    {
      source("scripts/_plot_setup.R", local = env, encoding = "UTF-8")
      for (topic in topics) source(paste0("scripts/_plot_", topic, ".R"), local = env, encoding = "UTF-8")
    },
    read_geolion_wfs = frozen(ns$read_geolion_wfs, "read_geolion_wfs", input_dir, refresh),
    .package = "airquality.methods"
  )

  # one row per plot: a plot catalog (plot, parameter, year, figure) or, from before the catalog, a tibble
  # (type, source, pollutant, year, plot)
  plots <- purrr::map(rlang::set_names(topics), \(topic) get(paste0("plots_", topic), envir = env)) |> purrr::list_rbind(names_to = "topic")
  if (!"figure" %in% names(plots)) plots <- dplyr::rename(plots, figure = plot, plot = source, parameter = pollutant)
  files <- file.path(output_dir, paste0(gsub("[^A-Za-z0-9.-]+", "-", paste(plots$topic, plots$plot, plots$parameter, plots$year, sep = "_")), ".png"))
  if (anyDuplicated(files)) cli::cli_abort("Plot file names are not unique: {.file {unique(files[duplicated(files)])}}")

  purrr::walk2(plots$figure, files, \(plot, file) {
    ragg::agg_png(file, width = width, height = height, units = "in", res = res)
    print(plot)
    grDevices::dev.off()
  })

  cli::cli_alert_success("{length(files)} plot{?s} written to {.file {output_dir}}")
  invisible(files)
}

# compare two runs by content, ignoring the file names (for changes that rename plots): every figure of one
# run must have a byte-identical figure in the other, and the numbers must agree
compare_plots_content <- function(label_a = "reference", label_b = "candidate") {
  dirs <- file.path(regression_dir, "results", "plots", c(label_a, label_b))
  md5 <- purrr::map(dirs, \(dir) tools::md5sum(list.files(dir, pattern = "\\.png$", full.names = TRUE)))
  only <- list(md5[[1]][!md5[[1]] %in% md5[[2]]], md5[[2]][!md5[[2]] %in% md5[[1]]])

  cli::cli_inform(c(
    "{length(md5[[1]])} vs. {length(md5[[2]])} plots, {sum(md5[[1]] %in% md5[[2]])} of {label_a} with a byte-identical figure in {label_b}",
    if (length(only[[1]]) > 0) c(x = "no match in {label_b}: {.file {basename(names(only[[1]]))}}"),
    if (length(only[[2]]) > 0) c(x = "no match in {label_a}: {.file {basename(names(only[[2]]))}}")
  ))
  invisible(list(only_a = basename(names(only[[1]])), only_b = basename(names(only[[2]]))))
}

# compare two runs file by file: byte identity first, then pixel identity for files that differ
compare_plots <- function(label_a = "reference", label_b = "candidate") {
  dirs <- file.path(regression_dir, "results", "plots", c(label_a, label_b))
  files <- purrr::map(dirs, \(dir) sort(list.files(dir, pattern = "\\.png$")))

  only <- list(setdiff(files[[1]], files[[2]]), setdiff(files[[2]], files[[1]]))
  common <- intersect(files[[1]], files[[2]])
  md5 <- purrr::map(dirs, \(dir) unname(tools::md5sum(file.path(dir, common))))
  differ <- common[md5[[1]] != md5[[2]]]
  pixel_differ <- purrr::keep(differ, \(file) {
    a <- png::readPNG(file.path(dirs[[1]], file))
    b <- png::readPNG(file.path(dirs[[2]], file))
    !identical(dim(a), dim(b)) || any(a != b)
  })

  cli::cli_inform(c(
    "{length(common)} common plot{?s}, {length(common) - length(differ)} byte-identical, {length(differ) - length(pixel_differ)} only pixel-identical",
    if (length(pixel_differ) > 0) c(x = "differ: {.file {pixel_differ}}"),
    if (length(only[[1]]) > 0) c(x = "only in {label_a}: {.file {only[[1]]}}"),
    if (length(only[[2]]) > 0) c(x = "only in {label_b}: {.file {only[[2]]}}")
  ))
  invisible(list(differ = pixel_differ, only_a = only[[1]], only_b = only[[2]]))
}
