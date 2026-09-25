# Unit tests for R/monitoring.R. All inputs are synthetic; no network.

# ---- pollutants ----------------------------------------------------------------------

test_that("prepare_monitoring_airquality() keeps the sites of the given cantons and drops `canton`", {
  data <- tibble::tibble(
    year = 2020, site = c("A", "B", "C", "D"), canton = c("ZH", "AG", NA, "ZH"),
    parameter = "NO2", concentration = 1:4
  )

  result <- prepare_monitoring_airquality(data, cantons = "ZH")
  expect_equal(result$site, c("A", "D"))
  expect_false("canton" %in% names(result))

  expect_equal(prepare_monitoring_airquality(data, cantons = c("ZH", "AG"))$site, c("A", "B", "D"))
})

test_that("prepare_monitoring_airquality() stops if a column is missing", {
  data <- tibble::tibble(year = 2020, site = "A")

  expect_error(prepare_monitoring_airquality(data, "ZH"), "canton", class = "airquality_input_error")
})

# The classes of the nitrogen deposition (recode_ecosystems(), classify_*(), derive_source_category()) are
# tested in airquality.methods.

# ---- nitrogen deposition: preparation and aggregation -------------------------------------

make_site_meta <- function() {
  tibble::tibble(
    site = c("S1", "S2"), ecosys = c("Mischwald", "Siedlungen"), x = c(1, 2), y = c(3, 4), masl = c(400, 500),
    gve_5km = c(12000, 1000), n_fertilization_5km = c(10, 10), nh3_emission_1km = c(40, 5), cln = c(15, NA),
    other = "dropped"
  )
}

make_ndep <- function() {
  tibble::tibble(
    year = 2020, site = c("S1", "S1", "S1", "S2", "S3"), canton = c("ZH", "ZH", "ZH", "ZH", "AG"),
    ecosys = c("Laubwald", "Laubwald", "Laubwald", "Siedlungen", "Wald"),
    parameter = c("NO2-N_ddep", "NH4-N_bdep", "NH3-N_ddep", "NH4-N_bdep", "NH4-N_bdep"),
    value = c(2, 10, 4, 5, 1), unit = "kg/ha/a", part_est = c(1, 0, 0.5, 0, 0),
    sampling = "x", datasource = c("FUB", "Ostluft", "FUB", "Ostluft", "Ostluft"), source = "Ostluft"
  )
}

test_that("prepare_ndep_site_meta() classifies the sites and keeps the needed columns", {
  result <- prepare_ndep_site_meta(make_site_meta())

  expect_named(result, c("site", "ecosys", "x", "y", "masl", "siteclass", "emissionclass", "gve_5km",
                         "nh3_emission_1km", "cln"))
  expect_equal(as.character(result$siteclass), c("hoch", "tief"))
  expect_equal(as.character(result$emissionclass), c("hoch", "tief"))
  expect_equal(as.character(result$ecosys), c("Wald", "kein empf. Ökosys."))
})

test_that("prepare_ndep_parameters() adds the site metadata and keeps the given cantons", {
  site_meta <- prepare_ndep_site_meta(make_site_meta())

  result <- prepare_ndep_parameters(make_ndep(), site_meta, cantons = "ZH")

  expect_named(result, c("year", "site", "canton", "x", "y", "masl", "ecosys", "cln", "siteclass", "emissionclass",
                         "pollutant", "metric", "parameter", "deposition", "unit", "part_est", "source_cat",
                         "sampling", "datasource", "source"))
  expect_equal(result$site, c("S1", "S1", "S1", "S2"))
  expect_equal(result$cln, c(15, 15, 15, NA))
  expect_equal(as.character(result$siteclass), c("hoch", "hoch", "hoch", "tief"))
  expect_equal(result$source_cat, c("aus NOx-Quellen", "aus NH3-Quellen", "aus NH3-Quellen", "aus NH3-Quellen"))
  expect_equal(as.character(unique(result$pollutant)), "Ndep")
  expect_equal(as.character(unique(result$metric)), "Jahressumme")
  expect_equal(result$deposition, c(2, 10, 4, 5))
})

test_that("prepare_ndep_parameters() stops if the deposition data or the site metadata lack columns", {
  site_meta <- prepare_ndep_site_meta(make_site_meta())

  expect_error(prepare_ndep_parameters(dplyr::select(make_ndep(), -part_est), site_meta, "ZH"),
               "part_est", class = "airquality_input_error")
  expect_error(prepare_ndep_site_meta(dplyr::select(make_site_meta(), -cln)), "cln", class = "airquality_input_error")
})

test_that("combine_sources() lists every data source once, sorted", {
  expect_equal(combine_sources(c("Ostluft", "FUB")), "FUB,Ostluft")
  expect_equal(combine_sources(c("FUB", "Ostluft")), "FUB,Ostluft")
  expect_equal(combine_sources(c("Ostluft", "Ostluft")), "Ostluft")
})

test_that("combine_sources() splits entries that already list several sources", {
  # the input data contains combined entries such as "FUB, Ostluft"
  expect_equal(combine_sources(c("FUB, Ostluft", "Ostluft")), "FUB,Ostluft")
  expect_equal(combine_sources(c("FUB, Ostluft", "FUB, Ostluft")), "FUB,Ostluft")
})

test_that("combine_sources() ignores missing and empty entries", {
  expect_equal(combine_sources(c(NA, "FUB")), "FUB")
  expect_equal(combine_sources(c("FUB", "")), "FUB")
  expect_equal(combine_sources(c(NA_character_, NA_character_)), NA_character_)
})

test_that("aggregate_ndep() sums the deposition per site and year and the estimated part", {
  pars <- prepare_ndep_parameters(make_ndep(), prepare_ndep_site_meta(make_site_meta()), cantons = "ZH")

  result <- aggregate_ndep(pars, additional_groups = c("x", "y", "masl", "pollutant", "metric"))

  expect_named(result, c("site", "year", "ecosys", "siteclass", "emissionclass", "canton", "cln", "unit", "x", "y",
                         "masl", "pollutant", "metric", "datasource", "source", "estimated", "deposition",
                         "estimated_class", "frac_estimated_class"))
  expect_equal(result$site, c("S1", "S2"))
  s1 <- dplyr::filter(result, site == "S1")
  expect_equal(s1$deposition, 16)
  expect_equal(s1$estimated, 2 * 1 + 4 * 0.5)
  expect_equal(s1$datasource, "FUB,Ostluft")
  expect_equal(dplyr::filter(result, site == "S2")$datasource, "Ostluft")
  expect_equal(as.character(s1$estimated_class), "<5 kg-N")
  expect_equal(as.character(s1$frac_estimated_class), "<33%")
})

test_that("aggregate_ndep() does not depend on the row order of the input", {
  pars <- prepare_ndep_parameters(make_ndep(), prepare_ndep_site_meta(make_site_meta()), cantons = "ZH")
  groups <- c("x", "y", "masl", "pollutant", "metric")

  result <- aggregate_ndep(pars, additional_groups = groups)
  shuffled <- aggregate_ndep(pars[c(4, 3, 1, 2), ], additional_groups = groups)

  expect_equal(result$site, c("S1", "S2"))
  expect_equal(shuffled, result)
})

# ---- pollutant maps: canton maps and model verification ----------------------------------

# a 400 x 400 m raster of 20 m cells, as one row of airquality.methods::read_geo_admin(); the value rises
# from west to east by 1 per 20 m cell (1 ... 20)
make_raster_row <- function(year = 2020, label = "no2", collection = "ch.bafu.luftreinhaltung-stickstoffdioxid",
                            origin = c(0, 0)) {
  bbox <- c(xmin = origin[1], ymin = origin[2], xmax = origin[1] + 400, ymax = origin[2] + 400)
  raster <- stars::st_as_stars(sf::st_bbox(bbox, crs = sf::st_crs(2056)), dx = 20, values = 0)
  raster[[1]][] <- rep(1:20, times = 20)
  names(raster) <- label
  tibble::tibble(collection = collection, label = label, year = as.integer(year), stars = list(raster))
}

# the western half of the raster
make_boundary <- function() {
  sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(200, 0), c(200, 400), c(0, 400), c(0, 0)))),
                                  crs = 2056))
}

test_that("aggregate_map() averages onto the cell size and masks cells outside the boundary", {
  grid <- airquality.methods::make_reference_grid(c(xmin = 0, ymin = 0, xmax = 400, ymax = 400), cellsize = 100)

  result <- suppressMessages(aggregate_map(make_raster_row(), grid, make_boundary()))

  expect_named(result, c("parameter", "year", "derived", "stars"))
  expect_equal(result$parameter, "NO2")
  expect_equal(result$year, 2020)
  expect_false(result$derived)
  map <- result$stars[[1]]
  expect_named(map, "concentration")
  expect_equal(unname(stars::st_res(map)), c(100, 100))
  # 100 m cells average five 20 m cells: 3 and 8 in the western half, NA (outside) in the eastern half
  values <- map[["concentration"]]
  expect_equal(sort(unique(stats::na.omit(as.vector(values)))), c(3, 8))
  expect_equal(sum(!is.na(values)), 8)
})

test_that("read_pollutant_maps() reads the available years and skips a year that cannot be read, with a warning", {
  get_assets <- function(collection) tibble::tibble(year = c(2018L, 2019L, 2020L))
  read <- function(collection, years, bbox) {
    if (years == 2019) cli::cli_abort("{.val luftreinhaltung-stickstoffdioxid_2019_2056.tif} is not in EPSG:2056.")
    make_raster_row(years)
  }
  sites <- tibble::tibble(site = "A", x = 10, y = 10)
  boundary <- make_boundary()

  expect_warning(
    result <- suppressMessages(read_pollutant_maps("ch.bafu.luftreinhaltung-stickstoffdioxid", years = 2019:2021,
                                                   boundary = boundary, sites = sites, read = read, get_assets = get_assets)),
    "2019"
  )

  expect_equal(result$maps$year, 2020)
  expect_equal(result$sites$year, 2020)
  expect_equal(result$sites$concentration_map, 1)
})

test_that("extract_at_sites() takes the value of the native cell of each site", {
  sites <- tibble::tibble(site = c("A", "B", "C"), x = c(10, 390, 1000), y = c(10, 200, 200))

  result <- extract_at_sites(make_raster_row(), sites)

  expect_named(result, c("site", "x", "y", "year", "parameter", "concentration_map"))
  expect_equal(result$site, c("A", "B", "C"))
  expect_equal(result$parameter, rep("NO2", 3))
  expect_equal(result$year, rep(2020, 3))
  # A lies in the first 20 m column, B in the last, C outside the raster
  expect_equal(result$concentration_map, c(1, 20, NA))
})

test_that("extract_at_sites() swaps LV95 coordinates given as north/east, but returns them as given", {
  raster <- make_raster_row(origin = c(2600000, 1200000))
  sites <- tibble::tibble(site = c("A", "B"), x = c(2600010, 1200010), y = c(1200010, 2600390))

  result <- extract_at_sites(raster, sites)

  expect_equal(result$x, sites$x)
  expect_equal(result$y, sites$y)
  # A in the first column, B (swapped) in the last
  expect_equal(result$concentration_map, c(1, 20))
})

test_that("derive_o3_peakseason_map() derives the O3 peak season from the NO2 maps of years with coefficients", {
  grid <- airquality.methods::make_reference_grid(c(xmin = 0, ymin = 0, xmax = 400, ymax = 400), cellsize = 100)
  maps <- suppressMessages(dplyr::bind_rows(
    aggregate_map(make_raster_row(2019), grid, make_boundary()),
    aggregate_map(make_raster_row(2020), grid, make_boundary()),
    aggregate_map(make_raster_row(2020, "pm10", "ch.bafu.luftreinhaltung-feinstaub_pm10"), grid, make_boundary())
  ))
  coefs <- tibble::tibble(year = 2020, offset = 100, slope = -2)

  result <- derive_o3_peakseason_map(maps, coefs)

  expect_equal(result$parameter, "O3_peakseason_mean_d1_max_mean_h8gl")
  expect_true(result$derived)
  expect_equal(result$year, 2020)
  expect_equal(sort(unique(stats::na.omit(as.vector(result$stars[[1]][["concentration"]])))), c(100 - 2 * 8, 100 - 2 * 3))
})

test_that("add_o3_peakseason_sites() adds the O3 peak season at the sites of years with coefficients", {
  sites_map <- tibble::tibble(
    site = c("A", "A", "B", "A"), x = 1, y = 1, year = c(2019, 2020, 2020, 2020),
    parameter = c("NO2", "NO2", "NO2", "PM10"), concentration_map = c(10, 20, 30, 40)
  )
  coefs <- tibble::tibble(year = 2020, offset = 100, slope = -2)

  result <- add_o3_peakseason_sites(sites_map, coefs)

  expect_equal(nrow(result), 6)
  o3 <- dplyr::filter(result, parameter == "O3_peakseason_mean_d1_max_mean_h8gl")
  expect_equal(o3$site, c("A", "B"))
  expect_equal(o3$concentration_map, c(60, 40))
  expect_named(result, names(sites_map))
})

make_monitoring_sites <- function() {
  tibble::tibble(
    year = c(2020, 2020, 2020, 2021, 2020), site = c("A", "B", "C", "A", "A"), x = 1, y = 1,
    siteclass = "städtisch - Hintergrund", pollutant = c("NO2", "NO2", "NO2", "NO2", "PM10"),
    metric = "Jahresmittel", parameter = pollutant, unit = "µg/m3", concentration = c(20, 25, 30, 22, 15),
    source = "Ostluft", masl = 400
  )
}

test_that("map_validation_data() pairs the measured values with the map values of the same site and year", {
  sites_map <- tibble::tibble(
    site = c("A", "B", "C", "A"), x = 1, y = 1, year = 2020, parameter = c("NO2", "NO2", "NO2", "PM10"),
    concentration_map = c(18, NA, 31, 14)
  )

  monitoring <- make_monitoring_sites() |>
    dplyr::mutate(siteclass = dplyr::if_else(site == "C", "klein-/vorstädtisch - verkehrsbelastet", siteclass))

  result <- map_validation_data(sites_map, monitoring, parameters = "NO2")

  expect_named(result, c("year", "site", "siteclass", "traffic", "pollutant", "metric", "parameter", "concentration",
                         "concentration_map"))
  # B has no map value, A 2021 no map, PM10 not wanted
  expect_equal(result$site, c("A", "C"))
  expect_equal(result$traffic, c("Hintergrund", "verkehrsbelastet"))
  expect_equal(result$concentration, c(20, 30))
  expect_equal(result$concentration_map, c(18, 31))
})

test_that("map_validation_data() stops if a column of the monitoring data is missing", {
  sites_map <- tibble::tibble(site = "A", x = 1, y = 1, year = 2020, parameter = "NO2", concentration_map = 1)

  expect_error(map_validation_data(sites_map, dplyr::select(make_monitoring_sites(), -siteclass), "NO2"),
               "siteclass", class = "airquality_input_error")
})


test_that("fit_map_validation() fits a robust line per parameter and traffic influence over the measured range", {
  data <- tibble::tibble(
    parameter = rep(c("NO2", "NO2", "PM10"), each = 11),
    traffic = rep(c("verkehrsbelastet", "Hintergrund", "Hintergrund"), each = 11),
    concentration = c(0:10, 0:10, 2:12),
    concentration_map = c(2 + 0.5 * (0:10), 3 + 0.8 * (0:10), 1 + 1 * (2:12)) + rep(c(0.1, -0.1), length.out = 33)
  )
  data$concentration_map[11] <- 100 # outlier of NO2 at traffic sites

  result <- fit_map_validation(data)

  expect_named(result, c("parameter", "traffic", "n", "intercept", "slope", "scale", "from", "to"))
  expect_equal(result$parameter, c("NO2", "NO2", "PM10"))
  expect_equal(result$traffic, c("Hintergrund", "verkehrsbelastet", "Hintergrund"))
  expect_equal(result$n, c(11, 11, 11))
  expect_equal(result$intercept, c(3, 2, 1), tolerance = 0.05)
  expect_equal(result$slope, c(0.8, 0.5, 1), tolerance = 0.05)
  expect_equal(result$from, c(0, 0, 2))
  expect_equal(result$to, c(10, 10, 12))
})

test_that("derive_pm25_map() derives PM2.5 from the PM10 maps of the given years without a PM2.5 map", {
  grid <- airquality.methods::make_reference_grid(c(xmin = 0, ymin = 0, xmax = 400, ymax = 400), cellsize = 100)
  pm10 <- \(year) aggregate_map(make_raster_row(year, "pm10", "ch.bafu.luftreinhaltung-feinstaub_pm10"), grid, make_boundary())
  maps <- suppressMessages(dplyr::bind_rows(
    pm10(2013), pm10(2014), pm10(2015),
    aggregate_map(make_raster_row(2015, "pm2_5", "ch.bafu.luftreinhaltung-feinstaub_pm2_5"), grid, make_boundary())
  ))
  ratios <- tibble::tibble(year = c(2014, 2015), ratio = c(0.5, 0.7))

  result <- derive_pm25_map(maps, ratios, years = 2000:2015)

  # 2013 has no ratio, 2015 has a PM2.5 map
  expect_equal(result$parameter, "PM2.5")
  expect_true(result$derived)
  expect_equal(result$year, 2014)
  expect_equal(sort(unique(stats::na.omit(as.vector(result$stars[[1]][["concentration"]])))), c(3, 8) * 0.5)
})
