# Compare exposition outputs of the regression variants (tests/regression/results/<variant>/) and of the
# production run (inst/extdata/output/) with the frozen baseline (tests/regression/baseline/).
#
# Writes one summary table per file to tests/regression/results/comparison_<file>.csv and prints it.
#
# run from the project root: source("tests/regression/compare_exposition.R")

baseline_dir <- "tests/regression/baseline"
candidates <- c(
  list.dirs("tests/regression/results", recursive = FALSE) |> rlang::set_names(basename),
  output = "inst/extdata/output"
)

read_output <- function(dir, file) {
  readr::read_delim(file.path(dir, file), delim = ";", show_col_types = FALSE,
                    locale = readr::locale(encoding = "UTF-8"))
}

# relative deviation of the value columns, per key combination, summarised per parameter
compare_table <- function(base, new, keys, values, by = "parameter") {
  joined <- dplyr::full_join(base, new, by = keys, suffix = c(".base", ".new"))

  coverage <-
    joined |>
    dplyr::summarise(
      rows_base = sum(!is.na(.data[[paste0(values[1], ".base")]])),
      rows_new = sum(!is.na(.data[[paste0(values[1], ".new")]])),
      rows_both = sum(!is.na(.data[[paste0(values[1], ".base")]]) & !is.na(.data[[paste0(values[1], ".new")]])),
      .by = dplyr::all_of(by)
    )

  deviations <-
    purrr::map(values, \(value) {
      joined |>
        dplyr::transmute(
          dplyr::across(dplyr::all_of(by)),
          value = value,
          rel = (.data[[paste0(value, ".new")]] - .data[[paste0(value, ".base")]]) / .data[[paste0(value, ".base")]]
        )
    }) |>
    purrr::list_rbind() |>
    dplyr::filter(is.finite(rel)) |>
    dplyr::summarise(
      median_rel = stats::median(rel),
      max_abs_rel = max(abs(rel)),
      .by = dplyr::all_of(c(by, "value"))
    )

  dplyr::left_join(deviations, coverage, by = by)
}

# the cumulative distributions are compared as curves: largest vertical distance per year and parameter
compare_distribution <- function(base, new, keys, class, cum_rel) {
  common <- dplyr::inner_join(dplyr::distinct(base, dplyr::pick(dplyr::all_of(keys))),
                              dplyr::distinct(new, dplyr::pick(dplyr::all_of(keys))), by = keys)
  base <- dplyr::semi_join(base, common, by = keys)
  new <- dplyr::semi_join(new, common, by = keys)

  dplyr::full_join(base, new, by = c(keys, class), suffix = c(".base", ".new")) |>
    dplyr::arrange(dplyr::pick(dplyr::all_of(c(keys, class)))) |>
    # a class missing in one file means no increase there: carry the curve forward, start at 0
    tidyr::fill(dplyr::all_of(paste0(cum_rel, c(".base", ".new"))), .direction = "down", .by = dplyr::all_of(keys)) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(paste0(cum_rel, c(".base", ".new"))), \(x) dplyr::coalesce(x, 0))
    ) |>
    dplyr::summarise(
      max_curve_distance = max(abs(.data[[paste0(cum_rel, ".new")]] - .data[[paste0(cum_rel, ".base")]])),
      .by = dplyr::all_of(keys)
    )
}

files <- list(
  data_exposition_weighted_means_canton.csv = \(base, new) compare_table(
    base, new,
    keys = c("year", "parameter"),
    values = c("population_weighted_mean", "population_weighted_mean_base", "population",
               "concentration_min", "concentration_max", "concentration_median")
  ),
  data_exposition_weighted_means_municipalities.csv = \(base, new) compare_table(
    base, new,
    keys = c("year", "parameter", "bfsnr"),
    values = c("population_weighted_mean", "population")
  ),
  data_exposition_distribution_pollutants.csv = \(base, new) {
    curve <- compare_distribution(base, new, c("year", "parameter"), "concentration", "population_cum_rel") |>
      dplyr::summarise(max_curve_distance = max(max_curve_distance, na.rm = TRUE), .by = "parameter")
    totals <- compare_table(
      dplyr::summarise(base, population = sum(population), .by = c(year, parameter)),
      dplyr::summarise(new, population = sum(population), .by = c(year, parameter)),
      keys = c("year", "parameter"), values = "population"
    )
    dplyr::left_join(totals, curve, by = "parameter")
  },
  data_exposition_distribution_ndep.csv = \(base, new) {
    curve <- compare_distribution(base, new, "year", "ndep_exmax", "n_ecosys_cum_rel") |>
      dplyr::summarise(max_curve_distance = max(max_curve_distance, na.rm = TRUE)) |>
      dplyr::mutate(parameter = "ndep_exmax")
    totals <- compare_table(
      dplyr::summarise(base, n_ecosys = sum(n_ecosys), .by = year) |> dplyr::mutate(parameter = "ndep_exmax"),
      dplyr::summarise(new, n_ecosys = sum(n_ecosys), .by = year) |> dplyr::mutate(parameter = "ndep_exmax"),
      keys = c("year", "parameter"), values = "n_ecosys"
    )
    dplyr::left_join(totals, curve, by = "parameter")
  }
)

comparison <-
  purrr::imap(files, \(compare, file) {
    base <- read_output(baseline_dir, file)
    purrr::imap(candidates, \(dir, variant) {
      if (!file.exists(file.path(dir, file))) return(NULL)
      new <- read_output(dir, file)
      compare(base, new) |>
        dplyr::mutate(
          variant = variant,
          same_columns = identical(names(base), names(new)),
          .before = 1
        )
    }) |>
      purrr::list_rbind()
  })

purrr::iwalk(comparison, \(table, file) {
  readr::write_delim(table, file.path("tests/regression/results", paste0("comparison_", file)), delim = ";")
  cli::cli_h2(file)
  cli::cli_text("columns identical to baseline: {.val {unique(table$variant[table$same_columns])}}")
  cli::cli_text("max. absolute relative deviation in % (rows compared: base / new / both):")
  table |>
    dplyr::mutate(
      max_abs_rel_pct = signif(100 * max_abs_rel, 2),
      rows = paste(rows_base, rows_new, rows_both, sep = "/")
    ) |>
    dplyr::select(variant, parameter, value, max_abs_rel_pct, rows) |>
    tidyr::pivot_wider(names_from = variant, values_from = c(max_abs_rel_pct, rows)) |>
    print(n = Inf, width = Inf)
  if ("max_curve_distance" %in% names(table)) {
    cli::cli_text("max. distance of cumulative distribution curves:")
    table |>
      dplyr::distinct(variant, parameter, max_curve_distance) |>
      tidyr::pivot_wider(names_from = variant, values_from = max_curve_distance) |>
      print(n = Inf, width = Inf)
  }
})
