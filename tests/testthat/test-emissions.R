# Unit tests for R/emissions.R. All inputs are synthetic; no network.

# ---- emission inventory (EMIKAT) ---------------------------------------------

make_emikat <- function() {
  # raw columns as delivered by opendata.swiss (German names, several inventory versions)
  tibble::tibble(
    jahr = c(2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015),
    substanz = c("NOx", "BC", "NOx", "NOx", "NOx", "NOx", "NOx", "NOx"),
    hauptgruppe = "Verkehr",
    untergruppe = c("Strassenverkehr", "Strassenverkehr", "Strassenverkehr", "Strassenverkehr",
                    "Rheinschifffahrt", "Strassenverkehr", "Strassenverkehr", "Strassenverkehr"),
    gemeinde = c("A", "A", "B", "C", "A", "A", "A", "A"),
    emission = c(1, 0.5, 2, 4, 3, 0, NA, 9),
    bfsnr = 1,
    kanton = c("ZH", "ZH", "ZH", "AG", "ZH", "ZH", "ZH", "ZH"),
    einheit = "t/a",
    einheit_lang = "Tonnen pro Jahr",
    stand = c("sub_25", "sub_25", "sub_25", "sub_25", "sub_25", "sub_25", "sub_25", "sub_19"),
    source = "Ostluft & BAFU"
  )
}

test_that("prepare_emissions() renames the columns and keeps the latest inventory version", {
  result <- prepare_emissions(make_emikat())

  expect_contains(names(result), c("year", "pollutant", "sector", "subsector", "canton", "municipality", "unit", "emission"))
  expect_false("stand" %in% names(result))
  expect_false(9 %in% result$emission) # only in the older version sub_19
})

test_that("prepare_emissions() keeps the canton, drops zero and excluded subsectors, renames BC", {
  result <- prepare_emissions(make_emikat())

  expect_equal(result$emission, c(1, 0.5, 2))
  expect_all_true(result$canton == "ZH")
  expect_false("Rheinschifffahrt" %in% result$subsector)
  expect_setequal(result$pollutant, c("NOx", "eBC"))
})

test_that("prepare_emissions() takes the canton and the excluded subsectors as arguments", {
  result <- prepare_emissions(make_emikat(), canton = "AG")
  expect_equal(result$emission, 4)

  result <- prepare_emissions(make_emikat(), exclude_subsectors = character())
  expect_contains(result$subsector, "Rheinschifffahrt")
})

test_that("prepare_emissions() drops the projections beyond year_max", {
  data <- dplyr::bind_rows(make_emikat(), dplyr::mutate(make_emikat(), jahr = 2030, emission = emission * 10))

  expect_setequal(prepare_emissions(data)$year, c(2015, 2030))
  result <- prepare_emissions(data, year_max = 2026)
  expect_equal(result$year, c(2015, 2015, 2015))
})

make_prepared_emissions <- function() {
  tibble::tibble(
    year = c(2015, 2015, 2015, 2015, 2016, 2016),
    pollutant = "NOx",
    sector = c("Verkehr", "Verkehr", "Verkehr", "Haushalte", "Verkehr", "Verkehr"),
    subsector = c("Strassenverkehr", "Offroad", "Bahn", "Feuerungen", "Strassenverkehr", "Bahn"),
    municipality = c("A", "A", "A", "A", "B", "B"),
    unit = "t/a",
    emission = c(10, 1, 2, 5, 8, NA),
    source = "Ostluft & BAFU"
  )
}

test_that("aggregate_emissions() sums emissions per year, pollutant, sector and new subsector", {
  subsector_new <- tibble::tibble(subsector = c("Offroad", "Bahn"), subsector_new = c("verschiedene", "verschiedene"))

  result <- aggregate_emissions(make_prepared_emissions(), subsector_new)

  expect_named(result, c("year", "pollutant", "metric", "unit", "sector", "subsector_new", "emission", "source"))
  verkehr_2015 <- dplyr::filter(result, year == 2015, sector == "Verkehr")
  expect_setequal(verkehr_2015$subsector_new, c("Strassenverkehr", "verschiedene"))
  expect_equal(verkehr_2015$emission[verkehr_2015$subsector_new == "verschiedene"], 3)
  expect_all_true(result$metric == "Jahresmenge")
  expect_all_true(result$source == "Ostluft & BAFU")
})

test_that("aggregate_emissions() keeps the total emission and drops empty groups", {
  data <- make_prepared_emissions()
  subsector_new <- tibble::tibble(subsector = "Offroad", subsector_new = "verschiedene")

  result <- aggregate_emissions(data, subsector_new)

  expect_equal(sum(result$emission), sum(data$emission, na.rm = TRUE))
  # Bahn 2016 has only NA and is dropped, no group without emission is added
  expect_false(any(result$year == 2016 & result$subsector_new == "Bahn"))
  expect_all_true(result$emission > 0)
})

make_aggregated_emissions <- function() {
  tibble::tibble(
    year = c(2015, 2015, 2015, 2016, 2016, 2015),
    pollutant = c("NOx", "NOx", "NOx", "NOx", "NOx", "PM10"),
    metric = "Jahresmenge",
    unit = "t/a",
    sector = c("Verkehr", "Verkehr", "Haushalte", "Verkehr", "Verkehr", "Verkehr"),
    subsector_new = c("Strasse", "Bahn", "Feuerungen", "Strasse", "Bahn", "Bahn"),
    emission = c(10, 1, 5, 2, 3, 20),
    source = "Ostluft & BAFU"
  )
}

test_that("add_emission_colours() orders subsectors by their total emission within each sector", {
  result <- add_emission_colours(make_aggregated_emissions())

  expect_named(result, c("sector", "subsector_new", "rootcol", "order", "col", "year", "pollutant", "metric",
                         "unit", "emission", "source"))
  order <- dplyr::distinct(result, sector, subsector_new, order) |> dplyr::arrange(order)
  # sectors alphabetically; within Verkehr: Bahn (1 + 3 + 20) before Strasse (10 + 2)
  expect_equal(order$subsector_new, c("Feuerungen", "Bahn", "Strasse"))
  expect_equal(order$order, 1:3)
})

test_that("add_emission_colours() gives every sector its own colour ramp", {
  result <- add_emission_colours(make_aggregated_emissions())
  colours <- dplyr::distinct(result, sector, subsector_new, rootcol, order, col) |> dplyr::arrange(order)

  expect_equal(unique(colours$rootcol[colours$sector == "Verkehr"]), "Gray")
  expect_equal(unique(colours$rootcol[colours$sector == "Haushalte"]), "Green")
  expect_equal(colours$col[colours$sector == "Verkehr"], airquality.methods::pal_emissions(2, "Gray"))
  expect_equal(colours$col[colours$sector == "Haushalte"], airquality.methods::pal_emissions(1, "Green"))
})

test_that("add_emission_colours() sorts the rows by year, pollutant, sector and decreasing emission", {
  result <- add_emission_colours(make_aggregated_emissions())

  expect_equal(result$emission, c(5, 10, 1, 20, 3, 2))
})

test_that("add_emission_colours() stops on a sector without colour", {
  data <- dplyr::mutate(make_aggregated_emissions(), sector = "Unbekannt")

  expect_error(add_emission_colours(data), "Unbekannt")
})

# ---- remote sensing (RSD) ------------------------------------------------------

test_that("calc_vsp() follows Jiménez", {
  # 20 m/s, 1 m/s2, flat: 20 * (1.1 * 1 + 0 + 0.132) + 0.000302 * 20^3
  expect_equal(calc_vsp(20, 1, 0), 20 * 1.232 + 0.000302 * 8000)
  # a slope adds g * slope per unit speed
  expect_equal(calc_vsp(10, 0, 0.01) - calc_vsp(10, 0, 0), 10 * 9.81 * 0.01)
})

test_that("calc_rsd_nox_emission() converts mixing ratios into g NOx per kg fuel", {
  # without CO and HC and without NO2: 30 * (NO/CO2) * 860 / 12, as NO2 equivalent (46 / 30)
  expect_equal(calc_rsd_nox_emission(NO = 0.01, p = 0, CO2 = 10, CO = 0, HC = 0), 0.001 * 860 * 46 / 12)
  # a NO2 fraction p increases NOx by 1 / (1 - p)
  expect_equal(
    calc_rsd_nox_emission(NO = 0.01, p = 0.5, CO2 = 10, CO = 0, HC = 0),
    2 * calc_rsd_nox_emission(NO = 0.01, p = 0, CO2 = 10, CO = 0, HC = 0)
  )
})

make_rsd_filters <- function() {
  tibble::tribble(
    ~parameter, ~min, ~max,
    "nmin", 2, NA,
    "vehicleyears", 2000, NA,
    "velocityrange", 5, 60,
    "accelerationrange", -2, 4,
    "vsprange", 1, 35,
    "weightmax", NA, 3500
  )
}

make_rsd_meta <- function() {
  tibble::tribble(
    ~vehicle_type, ~vehicle_fuel_type, ~vehicle_euronorm, ~parameter, ~value, ~source, ~remark,
    "passenger car", "diesel", "Euro5", "fraction_no2_hbefa", 0.5, "HBEFA", NA,
    "passenger car", "diesel", "Euro5", "nox_emission_threshold_g_per_kg_fuel", 5, "TU Graz", NA,
    "passenger car", "diesel", "Euro6b", "fraction_no2_hbefa", 0.2, "HBEFA", NA,
    "passenger car", "diesel", "Euro6b", "nox_emission_threshold_g_per_kg_fuel", 2, "TU Graz", NA,
    "passenger car", "diesel", "2010", "nox_emission_threshold_g_per_kg_fuel", 5, "TU Graz", NA,
    "passenger car", "diesel", "2020", "nox_emission_threshold_g_per_kg_fuel", 2, "TU Graz", NA
  )
}

# one vehicle measurement = one id with one row per measured parameter
make_rsd_vehicle <- function(id, euronorm = "Euro5", model_year = 2010, velocity = 36, acceleration = 1,
                             weight = 1500, NO = 100, date = as.Date("2020-06-01")) {
  tibble::tibble(
    id = id,
    date_measured = date,
    site_roadgrade = 0,
    vehicle_type = "passenger car",
    vehicle_fuel_type = "diesel",
    vehicle_euronorm = euronorm,
    vehicle_unloaded_weight = weight,
    vehicle_model_year = model_year,
    parameter = c("velocity", "acceleration", "NO", "CO2", "CO", "HC"),
    value = c(velocity, acceleration, NO, 10, 0, 0),
    unit = c("km/h", "km/h/s", "ppm", "%", "%", "ppm"),
    source = "Kanton Zürich/AWEL"
  )
}

test_that("prepare_rsd() returns one row per vehicle with its NOx emission", {
  data <- dplyr::bind_rows(make_rsd_vehicle(1), make_rsd_vehicle(2, euronorm = "Euro6b", model_year = 2020, NO = 50))

  result <- prepare_rsd(data, make_rsd_meta(), make_rsd_filters(), model_year_max = 2026)

  expect_equal(result$id, c(1, 2))
  expect_equal(result$vehicle_specific_power, rep(calc_vsp(10, 1 / 3.6, 0), 2))
  expect_equal(result$fraction_no2_hbefa, c(0.5, 0.2))
  expect_equal(
    result$nox_emission,
    calc_rsd_nox_emission(NO = c(100, 50) / 10^4, p = c(0.5, 0.2), CO2 = 10, CO = 0, HC = 0)
  )
  expect_s3_class(result$vehicle_type, "factor")
  expect_equal(levels(result$vehicle_fuel_type), c("gasoline", "diesel"))
})

test_that("prepare_rsd() applies the filter criteria", {
  data <- dplyr::bind_rows(
    make_rsd_vehicle(1),
    make_rsd_vehicle(2, model_year = 1999),    # older than vehicleyears
    make_rsd_vehicle(3, model_year = 2027),    # newer than model_year_max
    make_rsd_vehicle(4, velocity = 70),        # too fast
    make_rsd_vehicle(5, acceleration = 5),     # accelerates too much
    make_rsd_vehicle(6, acceleration = -1),    # negative vehicle specific power
    make_rsd_vehicle(7, weight = 4000),        # too heavy
    make_rsd_vehicle(8, NO = NA)               # NO missing
  )

  result <- prepare_rsd(data, make_rsd_meta(), make_rsd_filters(), model_year_max = 2026)

  expect_equal(result$id, 1)
})

test_that("prepare_rsd() merges Euro5a and Euro5b into Euro5", {
  data <- dplyr::bind_rows(make_rsd_vehicle(1, euronorm = "Euro5a"), make_rsd_vehicle(2, euronorm = "Euro5b"))

  result <- prepare_rsd(data, make_rsd_meta(), make_rsd_filters(), model_year_max = 2026)

  expect_equal(result$vehicle_euronorm, c("Euro5", "Euro5"))
  expect_equal(result$fraction_no2_hbefa, c(0.5, 0.5))
})

make_prepared_rsd <- function() {
  tibble::tibble(
    id = 1:5,
    date_measured = as.Date(c("2019-06-01", "2019-06-01", "2020-06-01", "2020-06-01", "2020-06-01")),
    vehicle_type = factor("passenger car", levels = c("passenger car", "light duty vehicle")),
    vehicle_fuel_type = factor(c("diesel", "diesel", "diesel", "gasoline", "gasoline"), levels = c("gasoline", "diesel")),
    vehicle_euronorm = c("Euro5", "Euro5", "Euro6b", "Euro6b", "Euro6b"),
    vehicle_model_year = c(2010, 2010, 2020, 2020, 2020),
    nox_emission = c(4, 6, 1, 0.5, 1.5),
    source = "Kanton Zürich/AWEL"
  )
}

test_that("aggregate_rsd_nox() averages NOx per group and adds the thresholds", {
  result <- aggregate_rsd_nox(make_prepared_rsd(), make_rsd_meta(), make_rsd_filters(),
                              groups = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm"))

  expect_named(result, c("pollutant", "metric", "vehicle_type", "vehicle_fuel_type", "vehicle_euronorm", "emission",
                         "unit", "n", "standarderror", "nox_emission_threshold_g_per_kg_fuel", "source"))
  euro5 <- dplyr::filter(result, vehicle_type == "passenger car", vehicle_euronorm == "Euro5", vehicle_fuel_type == "diesel")
  expect_equal(euro5$emission, 5)
  expect_equal(euro5$n, 2)
  expect_equal(euro5$standarderror, sd(c(4, 6)) / sqrt(2))
  expect_equal(euro5$nox_emission_threshold_g_per_kg_fuel, 5)
  expect_all_true(result$pollutant == "NOx" & result$metric == "Mittelwert" & result$unit == "g/kg fuel")
  expect_all_true(result$source == "Kanton Zürich/AWEL")
})

test_that("aggregate_rsd_nox() reports groups with fewer than nmin vehicles as NA", {
  result <- aggregate_rsd_nox(make_prepared_rsd(), make_rsd_meta(), make_rsd_filters(),
                              groups = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm"))

  # one diesel Euro6b only; gasoline Euro5 does not occur and is completed with n = 0
  cars <- dplyr::filter(result, vehicle_type == "passenger car")
  diesel_euro6b <- dplyr::filter(cars, vehicle_euronorm == "Euro6b", vehicle_fuel_type == "diesel")
  expect_equal(diesel_euro6b$n, 1)
  expect_true(is.na(diesel_euro6b$emission))
  gasoline_euro5 <- dplyr::filter(cars, vehicle_euronorm == "Euro5", vehicle_fuel_type == "gasoline")
  expect_equal(gasoline_euro5$n, 0)
  # vehicle types are factors: light duty vehicles are completed, too, although none was measured
  expect_setequal(result$vehicle_type, c("passenger car", "light duty vehicle"))
  expect_all_true(result$n[result$vehicle_type == "light duty vehicle"] == 0)
})

test_that("aggregate_rsd_nox() uses the model-year thresholds per vehicle model year", {
  result <- aggregate_rsd_nox(make_prepared_rsd(), make_rsd_meta(), make_rsd_filters(),
                              groups = c("vehicle_model_year", "vehicle_type", "vehicle_fuel_type"))

  expect_named(result, c("pollutant", "metric", "vehicle_model_year", "vehicle_type", "vehicle_fuel_type", "emission",
                         "unit", "n", "standarderror", "nox_emission_threshold_g_per_kg_fuel", "source"))
  diesel <- dplyr::filter(result, vehicle_type == "passenger car", vehicle_fuel_type == "diesel")
  expect_equal(diesel$nox_emission_threshold_g_per_kg_fuel[diesel$vehicle_model_year == 2010], 5)
  expect_equal(diesel$nox_emission_threshold_g_per_kg_fuel[diesel$vehicle_model_year == 2020], 2)
})

test_that("aggregate_rsd_nox() per year of measurement adds all fuel types together, without thresholds", {
  result <- aggregate_rsd_nox(make_prepared_rsd(), make_rsd_meta(), make_rsd_filters(),
                              groups = c("year", "vehicle_fuel_type"))

  expect_named(result, c("pollutant", "metric", "year", "vehicle_fuel_type", "emission", "unit", "n",
                         "standarderror", "source"))
  expect_setequal(result$vehicle_fuel_type, c("gasoline", "diesel", "all"))
  all_2020 <- dplyr::filter(result, year == 2020, vehicle_fuel_type == "all")
  expect_equal(all_2020$n, 3)
  expect_equal(all_2020$emission, 1)
})
