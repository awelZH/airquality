# Unit tests for R/plot_emissions.R. All inputs are synthetic; no network. The grouped legend itself is
# tested in airquality.methods.

# ---- plot_emission_inventory() ------------------------------------------------------------

make_emission_plot_data <- function() {
  tibble::tibble(
    year = rep(2000:2001, each = 3),
    pollutant = "NOx", metric = "Jahresmenge", unit = "t/a",
    sector = rep(c("Verkehr", "Verkehr", "Land- und Forstw."), 2),
    subsector_new = rep(c("Strassenverkehr", "verschiedene", "verschiedene"), 2),
    order = rep(c(2, 3, 1), 2),
    col = rep(c("#1A1A1A", "#7F7F7F", "#3C096C"), 2),
    emission = c(10, 2, 3, 9, 2, 3)
  )
}

test_that("plot_emission_inventory() shows sectors as legend blocks without pasting them to the subsectors", {
  plot <- plot_emission_inventory(make_emission_plot_data())

  texts <- legend_texts(plot)
  expect_contains(texts, c("Verkehr", "Land- und Forstw.", "Strassenverkehr"))
  expect_equal(sum(texts == "verschiedene"), 2)
  expect_false(any(grepl(" / ", texts)))
})

test_that("plot_emission_inventory() can move sectors, e.g. agriculture last", {
  plot <- plot_emission_inventory(make_emission_plot_data(), sectors_last = "Land- und Forstw.")

  expect_equal(levels(plot$data$key)[3], "Land- und Forstw.::verschiedene")
})

# ---- RSD plots ---------------------------------------------------------------------

make_rsd_per_norm <- function() {
  tidyr::expand_grid(
    vehicle_type = c("passenger car", "light duty vehicle"),
    vehicle_fuel_type = c("gasoline", "diesel"),
    vehicle_euronorm = c("Euro5", "Euro6", "Euro6c")
  ) |>
    dplyr::mutate(emission = seq_along(vehicle_type), standarderror = 0.1, nox_emission_threshold_g_per_kg_fuel = 5) |>
    dplyr::filter(!(vehicle_type == "light duty vehicle" & vehicle_euronorm == "Euro5" & vehicle_fuel_type == "diesel"))
}

test_that("plot_rsd_per_norm() drops Euro6c, fills missing combinations and translates the labels", {
  plot <- plot_rsd_per_norm(make_rsd_per_norm())

  expect_s3_class(plot, "ggplot")
  expect_false("Euro6c" %in% plot$data$vehicle_euronorm)
  expect_equal(nrow(plot$data), 2 * 2 * 2)
  expect_equal(levels(plot$data$vehicle_type), c("Personenwagen", "leichte Nutzfahrzeuge"))
  expect_equal(levels(plot$data$vehicle_fuel_type), c("Benzin", "Diesel"))
  expect_true(is.na(dplyr::filter(plot$data, vehicle_type == "leichte Nutzfahrzeuge", vehicle_fuel_type == "Diesel",
                                   vehicle_euronorm == "Euro5")$emission))
})

test_that("plot_rsd_per_yearmodel() fills missing model years and translates the labels", {
  data <- tidyr::expand_grid(
    vehicle_type = c("passenger car", "light duty vehicle"), vehicle_fuel_type = c("gasoline", "diesel"),
    vehicle_model_year = 2010:2011
  ) |>
    dplyr::mutate(emission = 1, standarderror = 0.1, nox_emission_threshold_g_per_kg_fuel = 5) |>
    dplyr::slice(-1)

  plot <- plot_rsd_per_yearmodel(data)

  expect_equal(nrow(plot$data), 8)
  expect_equal(levels(plot$data$vehicle_type), c("Personenwagen", "leichte Nutzfahrzeuge"))
  expect_equal(sum(is.na(plot$data$emission)), 1)
})

test_that("plot_rsd_per_yearmeas() translates the fuel types, 'all' first", {
  data <- tidyr::expand_grid(year = 2010:2015, vehicle_fuel_type = c("gasoline", "all", "diesel")) |>
    dplyr::mutate(emission = seq_along(year))

  plot <- plot_rsd_per_yearmeas(data)

  expect_equal(levels(plot$data$vehicle_fuel_type), c("Benzin & Diesel", "Benzin", "Diesel"))
  expect_match(as.character(plot$labels$title), "Abgasmessungen von Stickoxiden im realen Fahrbetrieb", fixed = TRUE)
})
