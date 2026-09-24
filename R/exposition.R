# Population and ecosystem exposition: from geo.admin raster data to the cell
# table, and from the cell table to the aggregated outputs.
#
# The unit of work is the *cell table*: one row per 100 m STATPOP cell and year,
# with the inhabitants, the municipality the cell centre lies in and one column
# per pollutant. Canton and municipality are two aggregation levels of the same
# cell table, so both are always computed from identical cells.


# ---- constants -----------------------------------------------------------------

#' geo.admin collections used for the population exposition
#'
#' @keywords internal
exposition_collections <- c(
  statpop = "ch.bfs.statistik-bevoelkerung_haushalte",
  no2 = "ch.bafu.luftreinhaltung-stickstoffdioxid",
  pm10 = "ch.bafu.luftreinhaltung-feinstaub_pm10",
  pm2_5 = "ch.bafu.luftreinhaltung-feinstaub_pm2_5",
  o3_max_98p_m1 = "ch.bafu.luftreinhaltung-ozon"
)

#' Parameter names of the cell-table pollutant columns, as used in all outputs
#'
#' @keywords internal
exposition_parameters <- c(
  no2 = "NO2",
  pm10 = "PM10",
  pm2_5 = "PM2.5",
  o3_max_98p_m1 = "O3_max_98p_m1",
  o3_peakseason_mean_d1_max_mean_h8gl = "O3_peakseason_mean_d1_max_mean_h8gl"
)

#' geo.admin collection of the critical load exceedance for nitrogen
#'
#' @keywords internal
ndep_collection <- "ch.bafu.luftreinhaltung-stickstoff_kritischer_eintrag"


# ---- read ------------------------------------------------------------------------

#' Read inhabitant and pollutant rasters and align them onto the STATPOP grid
#'
#' Pollutants are averaged (GDAL `average`) onto the 100 m grid of the STATPOP
#' raster of the same year; STATPOP itself is never resampled.
#'
#' @param years Years to read.
#' @param boundary `sf` object; its bounding box limits the download.
#' @param correct_noloc Subtract the STATPOP collector pixels (inhabitants that
#'   cannot be located), see [airquality.methods::read_statpop_ha()].
#'
#' @return Output of [airquality.methods::align_to_reference()]: one row per
#'   year with a `stars` object holding `BBTOT` and the pollutants.
#'
#' @keywords internal
read_exposition_rasters <- function(years, boundary, correct_noloc = TRUE) {
  specs <- airquality.methods::geo_admin_specs()
  specs[[exposition_collections[["statpop"]]]]$args$correct_noloc <- correct_noloc

  airquality.methods::read_geo_admin(
    unname(exposition_collections),
    years = years,
    bbox = boundary,
    specs = specs
  ) |>
    airquality.methods::align_to_reference(reference = exposition_collections[["statpop"]])
}

#' Build the cell table of the exposition from the rasters
#'
#' Reads the rasters ([read_exposition_rasters()]), turns them into one row per inhabited cell and year,
#' assigns each cell its municipality and gives the STATPOP collector pixel inhabitants back to their
#' municipality (decisions 3–5). One step, because the aligned rasters are partly streamed from the web
#' (GDAL `/vsicurl/`) and cannot be stored in between.
#'
#' @inheritParams read_exposition_rasters
#' @param map_municipalities Municipality polygons (without foreign enclaves).
#'
#' @return The cell table: `x`, `y`, `year`, `population`, `bfsnr`, `gemeindename` and one column per
#'   pollutant.
#'
#' @keywords internal
build_exposition_cells <- function(years, map_municipalities, correct_noloc = TRUE) {
  rasters <- read_exposition_rasters(years, map_municipalities, correct_noloc = correct_noloc)

  cells <-
    rasters |>
    rasters_to_cells() |>
    airquality.methods::assign_municipalities(map_municipalities)

  airquality.methods::redistribute_noloc(cells, airquality.methods::noloc_from_aligned(rasters), map_municipalities)
}


#' Read the critical load exceedance for nitrogen, restricted to the canton
#'
#' @param map_municipalities Municipality polygons; cells whose centre lies in
#'   none of them are dropped.
#' @param years Years to read (`NULL` = all available).
#'
#' @return Tibble with `x`, `y`, `year`, `ndep_exmax` (kgN/ha/a above the
#'   critical load).
#'
#' @keywords internal
read_ndep_exceedance <- function(map_municipalities, years = NULL) {
  data <- airquality.methods::read_geo_admin(ndep_collection, years = years, bbox = map_municipalities)

  purrr::map2(data$stars, data$year, \(raster, year) {
    tibble::as_tibble(raster) |>
      dplyr::mutate(year = as.numeric(year))
  }) |>
    purrr::list_rbind() |>
    dplyr::rename(ndep_exmax = "n_deposition_exceedance") |>
    dplyr::filter(!is.na(.data$ndep_exmax)) |>
    airquality.methods::assign_municipalities(map_municipalities) |>
    dplyr::filter(!is.na(.data$bfsnr)) |>
    dplyr::select("x", "y", "year", "ndep_exmax")
}


# ---- cell table ------------------------------------------------------------------

#' Convert aligned rasters into the cell table
#'
#' @param aligned Output of [read_exposition_rasters()].
#'
#' @return Tibble with one row per inhabited cell and year: `x`, `y` (cell
#'   centres), `year`, `population` and one column per pollutant (`NA` where a
#'   pollutant is not available for that year).
#'
#' @keywords internal
rasters_to_cells <- function(aligned) {
  cells <- purrr::map2(aligned$stars, aligned$year, \(raster, year) {
    tibble::as_tibble(raster) |>
      dplyr::mutate(year = as.numeric(year))
  }) |>
    purrr::list_rbind() |>
    dplyr::rename(population = "BBTOT") |>
    dplyr::filter(!is.na(.data$population))

  missing <- setdiff(names(exposition_collections)[-1], names(cells))
  cells[missing] <- NA_real_

  dplyr::select(cells, "x", "y", "year", "population", dplyr::all_of(names(exposition_collections)[-1]))
}

# municipality assignment and collector pixels: drop_foreign_enclaves(), assign_municipalities(),
# noloc_from_aligned(), redistribute_noloc() in airquality.methods

# ---- output --------------------------------------------------------------------

#' Round inhabitant counts to whole persons for output
#'
#' @param data Aggregated output table.
#'
#' @return `data` with `population` and `population_cum` rounded.
#'
#' @keywords internal
round_population <- function(data) {
  dplyr::mutate(data, dplyr::across(dplyr::any_of(c("population", "population_cum")), round))
}


# ---- derived parameters ---------------------------------------------------------

#' Fit the O3 peak-season model from monitoring data
#'
#' Robust linear regression of the O3 peak-season metric on NO2 with a common
#' slope and one offset per year, `O3_peakseason ~ NO2 + factor(year) - 1`,
#' using only years with at least `nmin_sites` sites measuring both.
#'
#' @param monitoring Monitoring data (`airquality.data::data_monitoring_aq_y1`).
#' @param nmin_sites Minimum number of sites per year.
#'
#' @return Tibble with `year`, `offset`, `slope`.
#'
#' @keywords internal
fit_o3_peakseason_model <- function(monitoring, nmin_sites = 7) {
  data <- monitoring |>
    dplyr::filter(.data$parameter %in% c("O3_peakseason_mean_d1_max_mean_h8gl", "NO2")) |>
    dplyr::select("year", "site", "masl", "parameter", "concentration") |>
    tidyr::pivot_wider(names_from = "parameter", values_from = "concentration") |>
    tidyr::drop_na() |>
    dplyr::filter(dplyr::n() >= nmin_sites, .by = "year")

  coefs <- fit_rlm_per_year(data, "O3_peakseason_mean_d1_max_mean_h8gl", covariate = "NO2")

  tibble::tibble(year = extract_year(names(coefs)), offset = unname(coefs)) |>
    dplyr::filter(!is.na(.data$year)) |>
    dplyr::mutate(slope = unname(coefs[["NO2"]]))
}

#' Robust regression with one offset per year and an optional common slope
#'
#' Equivalent to `MASS::rlm(y ~ covariate + factor(year) - 1)`, but with an
#' explicit design matrix so that a single year works as well.
#'
#' @param data Data with `year`, `response` and `covariate` columns.
#' @param response Name of the response column.
#' @param covariate Name of the covariate column, or `NULL`.
#'
#' @return Named coefficient vector: `covariate` and `year<YYYY>`.
#'
#' @keywords internal
fit_rlm_per_year <- function(data, response, covariate = NULL) {
  years <- sort(unique(data$year))
  if (length(years) == 0) {
    cli::cli_abort("No year has enough data to fit {.field {response}}.")
  }

  offsets <- purrr::map(years, \(yr) as.numeric(data$year == yr))
  x <- do.call(cbind, c(if (!is.null(covariate)) list(data[[covariate]]), offsets))
  colnames(x) <- c(covariate, paste0("year", years))

  stats::coefficients(MASS::rlm(x = x, y = data[[response]]))
}

#' Derive the O3 peak-season concentration from NO2 cell by cell
#'
#' @param cells Cell table with `year` and `no2`.
#' @param coefs Output of [fit_o3_peakseason_model()].
#'
#' @return `cells` with `o3_peakseason_mean_d1_max_mean_h8gl` added.
#'
#' @keywords internal
derive_o3_peakseason <- function(cells, coefs) {
  missing <- setdiff(unique(cells$year[!is.na(cells$no2)]), coefs$year)
  if (length(missing) > 0) {
    cli::cli_warn(c(
      "!" = "No O3 peak-season coefficients for {.val {missing}}.",
      "i" = "The O3 peak-season concentration is set to NA for these years."
    ))
  }

  cells |>
    dplyr::left_join(coefs, by = dplyr::join_by("year"), relationship = "many-to-one") |>
    dplyr::mutate(o3_peakseason_mean_d1_max_mean_h8gl = .data$no2 * .data$slope + .data$offset) |>
    dplyr::select(!c("offset", "slope"))
}

#' Estimate the yearly PM2.5:PM10 ratio at NABEL sites
#'
#' Robust regression `ratio ~ factor(year) - 1`, i.e. a robust mean ratio per
#' year.
#'
#' @param monitoring Monitoring data (`airquality.data::data_monitoring_aq_y1`).
#' @param source Monitoring network used.
#' @param exclude_sites Sites not representative for the canton.
#' @param min_year First year used.
#'
#' @return Tibble with `year`, `ratio`.
#'
#' @keywords internal
fit_pm_ratio <- function(monitoring,
                         source = "NABEL (BAFU & Empa)",
                         exclude_sites = "Bern-Bollwerk",
                         min_year = 2000) {
  data <- monitoring |>
    dplyr::filter(
      .data$source == !!source,
      .data$parameter %in% c("PM2.5", "PM10"),
      .data$year >= min_year,
      !.data$site %in% exclude_sites
    ) |>
    dplyr::select("year", "site", "parameter", "concentration") |>
    tidyr::pivot_wider(names_from = "parameter", values_from = "concentration") |>
    dplyr::mutate(ratio = .data$PM2.5 / .data$PM10) |>
    dplyr::filter(!is.na(.data$ratio))

  coefs <- fit_rlm_per_year(data, "ratio")

  tibble::tibble(year = extract_year(names(coefs)), ratio = unname(coefs)) |>
    dplyr::filter(!is.na(.data$year))
}

#' Derive PM2.5 from PM10 for years without PM2.5 rasters
#'
#' @param cells Cell table with `year`, `pm10`, `pm2_5`.
#' @param ratios Output of [fit_pm_ratio()].
#' @param years Years in which missing PM2.5 is derived.
#'
#' @return `cells` with `pm2_5` filled where it was missing in `years`.
#'
#' @keywords internal
derive_pm25_from_pm10 <- function(cells, ratios, years = 2010:2014) {
  target <- intersect(unique(cells$year), years)
  missing <- setdiff(target, ratios$year)
  if (length(missing) > 0) {
    cli::cli_warn("No PM2.5:PM10 ratio for {.val {missing}}; PM2.5 stays NA there.")
  }

  cells |>
    dplyr::left_join(ratios, by = dplyr::join_by("year"), relationship = "many-to-one") |>
    dplyr::mutate(
      pm2_5 = dplyr::if_else(
        is.na(.data$pm2_5) & .data$year %in% years,
        .data$pm10 * .data$ratio,
        .data$pm2_5
      )
    ) |>
    dplyr::select(!"ratio")
}


#' Coefficients of the derived parameters in long format, for the run log
#'
#' The derivation models are refitted on every run with the current monitoring
#' data, so the values of earlier years can shift slightly between runs. The
#' log makes such shifts traceable.
#'
#' @param coefs_o3 Output of [fit_o3_peakseason_model()].
#' @param ratios_pm Output of [fit_pm_ratio()].
#' @param run Identifier of the run, by default the current time.
#'
#' @return Tibble with `run`, `parameter`, `year`, `term`, `value`.
#'
#' @keywords internal
tidy_derivation_coefficients <- function(coefs_o3, ratios_pm, run = format(Sys.time(), "%Y-%m-%d %H:%M:%S")) {
  o3 <- coefs_o3 |>
    tidyr::pivot_longer(c("offset", "slope"), names_to = "term", values_to = "value") |>
    dplyr::mutate(parameter = "O3_peakseason_mean_d1_max_mean_h8gl")
  pm <- ratios_pm |>
    dplyr::transmute(.data$year, term = "ratio_pm25_pm10", value = .data$ratio, parameter = "PM2.5")

  dplyr::bind_rows(o3, pm) |>
    dplyr::mutate(run = run) |>
    dplyr::select("run", "parameter", "year", "term", "value") |>
    dplyr::arrange(.data$parameter, .data$term, .data$year)
}


# ---- long format and base scenario ----------------------------------------------

#' Pivot the cell table to one row per cell, year and parameter
#'
#' @param cells Cell table.
#'
#' @return Tibble with `parameter`, `pollutant`, `metric`, `concentration`;
#'   cells without a value for a parameter are dropped.
#'
#' @keywords internal
cells_to_long <- function(cells) {
  cells |>
    tidyr::pivot_longer(
      dplyr::any_of(names(exposition_parameters)),
      names_to = "parameter",
      values_to = "concentration",
      values_drop_na = TRUE
    ) |>
    dplyr::mutate(
      parameter = unname(exposition_parameters[.data$parameter]),
      pollutant = airquality.methods::shortpollutant(.data$parameter),
      metric = airquality.methods::longmetric(.data$parameter),
      .before = "concentration"
    )
}

#' Attach the base-scenario concentration
#'
#' The base scenario combines the inhabitants of each year with the
#' concentrations of `base_year` in the same cell. All STATPOP years share one
#' grid, so this is a join on the cell coordinates.
#'
#' @param long Output of [cells_to_long()].
#' @param base_year Reference year.
#'
#' @return `long` with `concentration_base` (`NA` in the base year itself).
#'
#' @keywords internal
add_base_scenario <- function(long, base_year) {
  base <- long |>
    dplyr::filter(.data$year == base_year) |>
    dplyr::select("x", "y", "parameter", concentration_base = "concentration")

  long |>
    dplyr::left_join(base, by = dplyr::join_by("x", "y", "parameter"), relationship = "many-to-one") |>
    dplyr::mutate(concentration_base = dplyr::if_else(.data$year == base_year, NA, .data$concentration_base))
}


# ---- aggregation -----------------------------------------------------------------

#' Population-weighted mean of a concentration
#'
#' @param concentration,population Numeric vectors of the same length; missing values are ignored.
#'
#' @return Numeric of length 1.
#'
#' @keywords internal
calc_population_weighted_mean <- function(concentration, population) {sum(concentration * population, na.rm = TRUE) / sum(population, na.rm = TRUE)}


#' Population-weighted mean concentration per canton or municipality
#'
#' `level = "canton"` uses every cell inside the canton, including lakes that
#' belong to no municipality; `level = "municipality"` uses cells with a
#' municipality, one row per `bfsnr`.
#'
#' @param data Output of [cells_to_long()] (optionally [add_base_scenario()]).
#' @param level `"canton"` or `"municipality"`.
#' @param concentration Concentration column to average (data-masked).
#'
#' @return Tibble with the established output columns.
#'
#' @keywords internal
aggregate_population_weighted_mean <- function(data,
                                               level = c("canton", "municipality"),
                                               concentration = concentration) {
  level <- rlang::arg_match(level)
  by <- c("year", "pollutant", "metric", "parameter")
  if (level == "municipality") {
    data <- dplyr::filter(data, !is.na(.data$gemeindename))
    by <- c(by, "bfsnr", "gemeindename")
  } else {
    data <- dplyr::filter(data, !is.na(.data$bfsnr))
  }

  data |>
    dplyr::mutate(.conc = {{ concentration }}) |>
    dplyr::filter(!is.na(.data$.conc), !is.na(.data$population)) |>
    dplyr::summarise(
      population_weighted_mean = calc_population_weighted_mean(.data$.conc, .data$population),
      population = sum(.data$population),
      concentration_min = min(.data$.conc),
      concentration_max = max(.data$.conc),
      concentration_mean = mean(.data$.conc),
      concentration_median = stats::median(.data$.conc),
      .by = dplyr::all_of(by)
    ) |>
    dplyr::arrange(dplyr::pick(dplyr::all_of(by))) |>
    dplyr::mutate(unit = "μg/m3", source = "BAFU & BFS")
}

#' Canton weighted means with the base scenario alongside
#'
#' @param long Output of [add_base_scenario()].
#' @param base_year Reference year.
#'
#' @return Tibble in the shape of `data_exposition_weighted_means_canton.csv`.
#'
#' @keywords internal
combine_canton_means <- function(long, base_year) {
  keys <- c("year", "pollutant", "metric", "parameter")

  base <- long |>
    dplyr::filter(.data$year != base_year) |>
    aggregate_population_weighted_mean(level = "canton", concentration = concentration_base) |>
    dplyr::select(dplyr::all_of(keys), population_weighted_mean_base = "population_weighted_mean") |>
    dplyr::mutate(base_year = base_year)

  aggregate_population_weighted_mean(long, level = "canton") |>
    dplyr::left_join(base, by = keys, relationship = "one-to-one") |>
    dplyr::relocate("population_weighted_mean_base", "base_year", .after = "parameter") |>
    dplyr::arrange(.data$year, .data$pollutant)
}

#' Inhabitants per concentration class in the canton
#'
#' Class width depends on the parameter, see [bin_fun()]. Cells without
#' inhabitants are dropped.
#'
#' @param data Output of [cells_to_long()].
#'
#' @return Tibble in the shape of `data_exposition_distribution_pollutants.csv`.
#'
#' @keywords internal
aggregate_population_exposition_distrib <- function(data) {
  keys <- c("year", "pollutant", "metric", "parameter")

  data |>
    dplyr::filter(!is.na(.data$bfsnr), .data$population > 0) |>
    dplyr::mutate(
      concentration = bin_fun(dplyr::first(.data$parameter))(.data$concentration),
      .by = "parameter"
    ) |>
    dplyr::summarise(
      population = sum(.data$population),
      .by = dplyr::all_of(c(keys, "concentration"))
    ) |>
    dplyr::arrange(.data$year, .data$pollutant, .data$metric, .data$concentration) |>
    dplyr::mutate(
      population_cum = cumsum(.data$population),
      population_cum_rel = .data$population_cum / sum(.data$population),
      .by = dplyr::all_of(keys)
    ) |>
    dplyr::mutate(source = "BAFU & BFS")
}

#' Sensitive ecosystems per class of critical load exceedance
#'
#' Classes of 1 kgN/ha/a, labelled by their centre.
#'
#' @param data Tibble with `year` and `ndep_exmax`, e.g. from
#'   [read_ndep_exceedance()].
#'
#' @return Tibble in the shape of `data_exposition_distribution_ndep.csv`.
#'
#' @keywords internal
aggregate_ndep_exposition_distrib <- function(data) {
  data |>
    dplyr::filter(!is.na(.data$ndep_exmax)) |>
    dplyr::mutate(ndep_exmax = floor(.data$ndep_exmax) + 0.5) |>
    dplyr::summarise(n_ecosys = dplyr::n(), .by = c("year", "ndep_exmax")) |>
    dplyr::arrange(.data$year, .data$ndep_exmax) |>
    dplyr::mutate(
      n_ecosys_cum = cumsum(.data$n_ecosys),
      n_ecosys_cum_rel = .data$n_ecosys_cum / sum(.data$n_ecosys),
      source = "BAFU",
      .by = "year"
    )
}
