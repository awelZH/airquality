# Air quality monitoring in the Canton of Zurich (Ostluft and NABEL): yearly pollutant values and
# nitrogen deposition in sensitive ecosystems, from the pre-compiled datasets of airquality.data.
#
# pollutants: data_monitoring_aq_y1 -> prepare_monitoring_airquality()
# nitrogen deposition: site_meta_ndep -> prepare_ndep_site_meta()
#                      data_monitoring_ndep_y1 -> prepare_ndep_parameters() (one row per parameter)
#                      -> aggregate_ndep() (total deposition per site and year)
# pollutant maps (BAFU rasters): read_pollutant_maps() -> canton maps (aggregate_map(), derive_o3_peakseason_map(),
#                                derive_pm25_map())
#                                and map values at the sites (extract_at_sites(), add_o3_peakseason_sites())
#                                -> map_validation_data() -> fit_map_validation() (model verification)
# Input checks (check_columns()) stop with a clear message if a dataset changed. The classes of the nitrogen
# deposition (ecosystems, site classes, estimated part, source category) come from airquality.methods.


# ---- pollutants ----------------------------------------------------------------------------

#' Select the yearly pollutant values of the monitoring sites in the given cantons
#'
#' @param data Yearly monitoring data (`airquality.data::data_monitoring_aq_y1`) with a column
#'   `canton`; sites without canton (e.g. NABEL sites outside the region) are dropped.
#' @param cantons Cantons whose sites are kept.
#'
#' @return `data` without the column `canton`.
#'
#' @keywords internal
prepare_monitoring_airquality <- function(data, cantons) {
  check_columns(data, c("site", "canton"), "monitoring data (airquality.data::data_monitoring_aq_y1)")

  data |>
    dplyr::filter(canton %in% cantons) |>
    dplyr::select(-canton)
}


# ---- nitrogen deposition: preparation and aggregation ------------------------------------------

#' Classify the nitrogen deposition sites
#'
#' @param site_meta Site metadata (`airquality.data::site_meta_ndep`).
#'
#' @return One row per site and ecosystem with `site`, `ecosys` (recoded), `x`, `y`, `masl`,
#'   `siteclass`, `emissionclass`, `gve_5km`, `nh3_emission_1km`, `cln` (critical load).
#'
#' @keywords internal
prepare_ndep_site_meta <- function(site_meta) {
  check_columns(site_meta, c("site", "ecosys", "x", "y", "masl", "gve_5km", "n_fertilization_5km",
                             "nh3_emission_1km", "cln"), "nitrogen deposition site metadata (airquality.data::site_meta_ndep)")

  site_meta |>
    dplyr::mutate(
      siteclass = airquality.methods::classify_ostluft_siteclass(gve_5km, n_fertilization_5km),
      emissionclass = airquality.methods::classify_nh3_emission(nh3_emission_1km),
      ecosys = airquality.methods::recode_ecosystems(ecosys)
    ) |>
    dplyr::select(site, ecosys, x, y, masl, siteclass, emissionclass, gve_5km, nh3_emission_1km, cln)
}


#' Nitrogen deposition per site, year and parameter, with the site metadata
#'
#' @param data Yearly nitrogen deposition per parameter (`airquality.data::data_monitoring_ndep_y1`).
#' @param site_meta Output of [prepare_ndep_site_meta()].
#' @param cantons Cantons whose sites are kept.
#'
#' @return One row per site, ecosystem, year and parameter (`data_ndep_pars_monitoring_y1.csv`).
#'
#' @keywords internal
prepare_ndep_parameters <- function(data, site_meta, cantons) {
  check_columns(data, c("year", "site", "canton", "ecosys", "parameter", "value", "unit", "part_est", "sampling",
                        "datasource", "source"), "nitrogen deposition data (airquality.data::data_monitoring_ndep_y1)")

  data |>
    dplyr::mutate(
      source_cat = airquality.methods::derive_source_category(parameter),
      ecosys = airquality.methods::recode_ecosystems(ecosys)
    ) |>
    dplyr::left_join(site_meta, by = dplyr::join_by(site, ecosys)) |>
    dplyr::mutate(
      metric = factor("Jahressumme"),
      pollutant = factor("Ndep")
    ) |>
    dplyr::rename(deposition = value) |>
    dplyr::select(year, site, canton, x, y, masl, ecosys, cln, siteclass, emissionclass, pollutant, metric, parameter,
                  deposition, unit, part_est, source_cat, sampling, datasource, source) |>
    dplyr::filter(canton %in% cantons)
}


#' List the data sources of a group once, sorted
#'
#' Entries may already list several sources ("FUB, Ostluft"), so they are split first. Without
#' this, the same set of sources appeared in several spellings and with duplicates, depending on
#' the row order of the input.
#'
#' @param datasource Character vector of data sources, comma-separated.
#'
#' @return One comma-separated string, or `NA` if there is no source.
#'
#' @keywords internal
combine_sources <- function(datasource) {
  sources <- unlist(stringr::str_split(datasource, ","))
  sources <- stringr::str_trim(sources)
  sources <- unique(sources[!is.na(sources) & sources != ""])
  if (length(sources) == 0) {
    return(NA_character_)
  }
  paste(sort(sources, method = "radix"), collapse = ",")
}


#' Total nitrogen deposition per site and year, with its estimated part
#'
#' Sums the deposition over all parameters; the estimated (modelled instead of measured) part is
#' the sum of `deposition * part_est`. The data sources of a site and year are listed once and
#' sorted ([combine_sources()]). Rows are sorted by the grouping columns, so the result does not
#' depend on the row order of the input.
#'
#' @param data Output of [prepare_ndep_parameters()].
#' @param additional_groups Further columns to keep (constant per site and year).
#'
#' @return One row per site and year (`data_ndep_monitoring_y1.csv`) with `estimated`,
#'   `deposition`, `estimated_class` and `frac_estimated_class`.
#'
#' @keywords internal
aggregate_ndep <- function(data, additional_groups = NULL) {
  groups <- c("site", "year", "ecosys", "siteclass", "emissionclass", "canton", "cln", "unit", additional_groups)
  groups_source <- c(groups, "datasource", "source")

  data |>
    dplyr::mutate(datasource = combine_sources(datasource), .by = dplyr::all_of(groups)) |>
    dplyr::summarise(
      estimated = sum(deposition * part_est),
      deposition = sum(deposition),
      .by = dplyr::all_of(groups_source)
    ) |>
    dplyr::arrange(dplyr::pick(dplyr::all_of(groups_source))) |>
    dplyr::mutate(
      estimated_class = airquality.methods::classify_estimated(estimated),
      frac_estimated_class = airquality.methods::classify_frac_estimated(estimated / deposition)
    )
}


# ---- pollutant maps: canton maps and model verification ------------------------------------------

#' Read the BAFU pollutant maps for the canton map and the model verification
#'
#' For each collection and each available year: the map averaged onto `cellsize` and masked to the
#' canton ([aggregate_map()]), and the values at the monitoring sites taken from the native raster
#' ([extract_at_sites()]). Reads one raster at a time, because the rasters of the newer years have
#' 20 m cells (about 7 million per canton extent), and streams them from the web; hence one step.
#'
#' A year that cannot be read is skipped with a warning, so one broken file does not stop the other maps.
#'
#' @param collections geo.admin collections of the pollutant maps (`exposition_collections` without
#'   STATPOP).
#' @param years Wanted years; years without a map are skipped.
#' @param boundary `sf` polygon of the canton; its bounding box limits the download.
#' @param sites Monitoring sites: `site`, `x`, `y` (LV95).
#' @param cellsize Cell size of the maps in metres.
#' @param read,get_assets Readers of the rasters and of the assets of a collection (replaced in the
#'   tests).
#'
#' @return List with `maps` (output of [aggregate_map()], one row per parameter and year) and `sites`
#'   (output of [extract_at_sites()]).
#'
#' @keywords internal
read_pollutant_maps <- function(collections, years, boundary, sites, cellsize = 100,
                                read = airquality.methods::read_geo_admin,
                                get_assets = airquality.methods::get_geo_admin_assets) {
  grid <- airquality.methods::make_reference_grid(boundary, cellsize = cellsize, crs = sf::st_crs(boundary))

  maps <- purrr::map(unname(collections), \(collection) {
    available <- intersect(years, get_assets(collection)$year)
    purrr::map(sort(available), \(year) {
      tryCatch(
        {
          raster <- read(collection, years = year, bbox = boundary)
          list(map = aggregate_map(raster, grid, boundary), sites = extract_at_sites(raster, sites))
        },
        error = function(err) {
          cli::cli_warn("Skipping the map of {.val {collection}} {year}: it cannot be read.", parent = err)
          NULL
        }
      )
    })
  }) |>
    purrr::list_flatten() |>
    purrr::compact()

  list(
    maps = purrr::list_rbind(purrr::map(maps, "map")),
    sites = purrr::list_rbind(purrr::map(maps, "sites"))
  )
}

#' Average a pollutant raster onto a grid and mask it to a boundary
#'
#' @param raster One row of [airquality.methods::read_geo_admin()] (`collection`, `label`, `year`,
#'   `stars`).
#' @param grid Target grid ([airquality.methods::make_reference_grid()]); values are averaged (GDAL
#'   `average`).
#' @param boundary `sf` polygon; cells whose centre lies outside become `NA`.
#'
#' @return Tibble with `parameter`, `year`, `derived` (`FALSE`) and `stars` (a `stars` object with the
#'   attribute `concentration`).
#'
#' @keywords internal
aggregate_map <- function(raster, grid, boundary) {
  map <- airquality.methods::align_to_grid(raster, grid)$stars[[1]][boundary]
  names(map) <- "concentration"

  tibble::tibble(parameter = unname(exposition_parameters[raster$label]), year = as.numeric(raster$year), derived = FALSE,
                 stars = list(map))
}

#' Values of a pollutant raster at the monitoring sites
#'
#' Each site gets the value of the native raster cell it lies in, so the fine gradients near roads of
#' the 20 m maps are kept. Coordinates given as north/east (`x < y`; in LV95 east is always larger) are
#' swapped for the lookup: `airquality.data` 0.1.3 has them swapped for the NABEL sites Dübendorf-EMPA and
#' Zürich-Kaserne.
#'
#' @inheritParams aggregate_map
#' @param sites Monitoring sites: `site`, `x`, `y` (LV95).
#'
#' @return Tibble with `site`, `x`, `y` (as given), `year`, `parameter`, `concentration_map` (`NA` outside
#'   the raster).
#'
#' @keywords internal
extract_at_sites <- function(raster, sites) {
  map <- raster$stars[[1]]
  swapped <- sites$x < sites$y
  lookup <- tibble::tibble(east = dplyr::if_else(swapped, sites$y, sites$x), north = dplyr::if_else(swapped, sites$x, sites$y))
  points <- sf::st_as_sf(lookup, coords = c("east", "north"), crs = sf::st_crs(map))
  values <- stars::st_extract(map, points)

  tibble::tibble(
    site = sites$site, x = sites$x, y = sites$y, year = as.numeric(raster$year),
    parameter = unname(exposition_parameters[raster$label]),
    concentration_map = values[[raster$label]]
  )
}

#' Derive O3 peak-season maps from the NO2 maps
#'
#' Applies the model of the exposition (decision 6, [fit_o3_peakseason_model()]) cell by cell; only years
#' with coefficients get a map.
#'
#' @param maps Output of [aggregate_map()] for several parameters and years.
#' @param coefs Output of [fit_o3_peakseason_model()].
#'
#' @return The derived maps only, in the form of `maps` (`derived` = `TRUE`).
#'
#' @keywords internal
derive_o3_peakseason_map <- function(maps, coefs) {
  maps |>
    dplyr::filter(parameter == "NO2") |>
    dplyr::inner_join(coefs, by = dplyr::join_by(year), relationship = "one-to-one") |>
    dplyr::mutate(
      parameter = "O3_peakseason_mean_d1_max_mean_h8gl",
      derived = TRUE,
      stars = purrr::pmap(list(stars, offset, slope), \(map, offset, slope) map * slope + offset)
    ) |>
    dplyr::select(parameter, year, derived, stars)
}

#' Derive PM2.5 maps from the PM10 maps for years without a PM2.5 map
#'
#' PM10 map times the yearly PM2.5:PM10 ratio of the NABEL sites ([fit_pm_ratio()]), as in the exposition
#' (decision 6); only years in `years` with a PM10 map, a ratio and no PM2.5 map get a map.
#'
#' @inheritParams derive_o3_peakseason_map
#' @param ratios Output of [fit_pm_ratio()].
#' @param years Years in which PM2.5 is derived.
#'
#' @return The derived maps only, in the form of `maps` (`derived` = `TRUE`).
#'
#' @keywords internal
derive_pm25_map <- function(maps, ratios, years) {
  maps |>
    dplyr::filter(parameter == "PM10", year %in% years, !year %in% maps$year[maps$parameter == "PM2.5"]) |>
    dplyr::inner_join(ratios, by = dplyr::join_by(year), relationship = "one-to-one") |>
    dplyr::mutate(
      parameter = "PM2.5",
      derived = TRUE,
      stars = purrr::map2(stars, ratio, \(map, ratio) map * ratio)
    ) |>
    dplyr::select(parameter, year, derived, stars)
}

#' Add the O3 peak season at the monitoring sites, derived from the NO2 map values
#'
#' @param sites_map Output of [extract_at_sites()] for several parameters and years.
#' @param coefs Output of [fit_o3_peakseason_model()]; years without coefficients get no value.
#'
#' @return `sites_map` with the rows of `O3_peakseason_mean_d1_max_mean_h8gl` added.
#'
#' @keywords internal
add_o3_peakseason_sites <- function(sites_map, coefs) {
  o3 <- sites_map |>
    dplyr::filter(parameter == "NO2", year %in% coefs$year) |>
    dplyr::rename(no2 = concentration_map) |>
    derive_o3_peakseason(coefs) |>
    dplyr::mutate(parameter = "O3_peakseason_mean_d1_max_mean_h8gl", concentration_map = o3_peakseason_mean_d1_max_mean_h8gl) |>
    dplyr::select(dplyr::all_of(names(sites_map)))

  dplyr::bind_rows(sites_map, o3)
}

#' Derive PM2.5 at the sites from the PM10 map values, in the years without a PM2.5 map
#'
#' The site counterpart of [derive_pm25_map()]: PM10 value at the site times the PM2.5:PM10 ratio of the
#' year. The ratio comes from monitoring data, partly of the same sites, so the verification of these years
#' is in-sample.
#'
#' @param sites_map Output of [extract_at_sites()] for several parameters and years.
#' @param ratios Output of [fit_pm_ratio()]; years without a ratio get no value.
#'
#' @return `sites_map` with the derived `PM2.5` rows added.
#'
#' @keywords internal
add_pm25_sites <- function(sites_map, ratios) {
  pm25 <- sites_map |>
    dplyr::filter(parameter == "PM10", !year %in% sites_map$year[sites_map$parameter == "PM2.5"]) |>
    dplyr::inner_join(ratios, by = dplyr::join_by(year), relationship = "many-to-one") |>
    dplyr::mutate(parameter = "PM2.5", concentration_map = concentration_map * ratio) |>
    dplyr::select(dplyr::all_of(names(sites_map)))

  dplyr::bind_rows(sites_map, pm25)
}

#' Pair the measured yearly values with the map values at the same site
#'
#' @param sites_map Map values at the sites ([extract_at_sites()], [add_o3_peakseason_sites()]).
#' @param monitoring Monitoring data of the canton (`data_airquality_monitoring_y1.csv`); sites of all
#'   site classes are kept.
#' @param parameters Parameters to keep.
#'
#' @return One row per site, year and parameter with both values: `year`, `site`, `siteclass`, `traffic`
#'   (traffic influence of the site class: "verkehrsbelastet" or "Hintergrund"), `pollutant`, `metric`,
#'   `parameter`, `concentration` (measured), `concentration_map`.
#'
#' @keywords internal
map_validation_data <- function(sites_map, monitoring, parameters) {
  check_columns(monitoring, c("year", "site", "x", "y", "siteclass", "pollutant", "metric", "parameter", "concentration"),
                "monitoring data (data_airquality_monitoring_y1)")

  monitoring |>
    dplyr::mutate(
      dplyr::across(c(site, siteclass, pollutant, metric, parameter), as.character),
      traffic = dplyr::if_else(grepl("verkehrsbelastet", siteclass), "verkehrsbelastet", "Hintergrund")
    ) |>
    dplyr::filter(parameter %in% parameters) |>
    dplyr::select(year, site, x, y, siteclass, traffic, pollutant, metric, parameter, concentration) |>
    dplyr::inner_join(sites_map, by = dplyr::join_by(site, x, y, year, parameter)) |>
    dplyr::filter(!is.na(concentration), !is.na(concentration_map)) |>
    dplyr::select(!c(x, y))
}

#' Robust regression of the map values on the measured values, per group
#'
#' `MASS::rlm(concentration_map ~ concentration)` per parameter and traffic influence, over all years by
#' default. The optional prediction band uses the robust scale `s` of the fit and the covariance of the
#' coefficients: fit +/- t(n - 2) * sqrt(s^2 + se^2); it assumes a constant spread over the range.
#'
#' @param data Output of [map_validation_data()].
#' @param by Grouping columns, e.g. with `"year"` for one fit per year.
#' @param min_n Groups with fewer pairs get no fit.
#' @param band Add the prediction band?
#' @param level Level of the prediction band.
#' @param n_grid Number of points of the band, spread evenly over the measured range of the group.
#'
#' @return Tibble with one row per group: the `by` columns, `n`, `intercept`, `slope`, `scale` (robust
#'   scale of the residuals), the measured range `from`, `to` and, with `band`, the list column `band`
#'   (tibble with `concentration`, `fit`, `lower`, `upper`).
#'
#' @keywords internal
fit_map_validation <- function(data, by = c("parameter", "traffic"), min_n = 3, band = FALSE, level = 0.95, n_grid = 50) {
  # the shape of the result, also when no group has enough pairs
  empty <- tibble::tibble(dplyr::slice(dplyr::distinct(data, dplyr::pick(dplyr::all_of(by))), 0), n = integer(),
                          intercept = double(), slope = double(), scale = double(), from = double(), to = double())
  if (band) empty$band <- list()

  data |>
    dplyr::arrange(dplyr::pick(dplyr::all_of(by))) |>
    dplyr::filter(dplyr::n() >= min_n, .by = dplyr::all_of(by)) |>
    dplyr::group_split(dplyr::pick(dplyr::all_of(by))) |>
    purrr::map(\(data) {
      fit <- MASS::rlm(concentration_map ~ concentration, data = data, maxit = 50)
      coefs <- stats::coef(fit)
      result <- tibble::tibble(dplyr::distinct(data, dplyr::pick(dplyr::all_of(by))), n = nrow(data), intercept = unname(coefs[1]),
                               slope = unname(coefs[2]), scale = fit$s, from = min(data$concentration), to = max(data$concentration))
      if (!band) return(result)

      grid <- tibble::tibble(concentration = seq(result$from, result$to, length.out = n_grid))
      x <- cbind(1, grid$concentration)
      se <- sqrt(rowSums((x %*% stats::vcov(fit)) * x))
      half <- stats::qt((1 + level) / 2, df = nrow(data) - 2) * sqrt(fit$s^2 + se^2)
      dplyr::mutate(result, band = list(dplyr::mutate(grid, fit = drop(x %*% coefs), lower = fit - half, upper = fit + half)))
    }) |>
    purrr::list_rbind(ptype = empty)
}

#' Keep the years of a parameter with enough pairs of measured and map values
#'
#' @param data Output of [map_validation_data()].
#' @param min_n Minimum number of pairs of a parameter in a year.
#'
#' @return `data` without the years of a parameter with fewer than `min_n` pairs.
#'
#' @keywords internal
keep_years_with_pairs <- function(data, min_n) {
  dplyr::filter(data, dplyr::n() >= min_n, .by = c(parameter, year))
}
