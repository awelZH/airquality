# Unit tests for R/plot_monitoring.R. All inputs are synthetic; no network.

siteclasses <- c("städtisch - verkehrsbelastet", "städtisch - Hintergrund", "klein-/vorstädtisch - Hintergrund",
                 "ländlich - Hintergrund")

# ---- preparing the monitoring data --------------------------------------------------

test_that("prepare_plot_airquality() keeps the given years, parameters and site classes", {
  data <- tibble::tibble(
    year = c(2019, 2020, 2020, 2020, 2020),
    parameter = c("NO2", "NO2", "PM10", "NO2", "NO2"),
    siteclass = c("ländlich - Hintergrund", "städtisch - Hintergrund", "städtisch - Hintergrund",
                  "ländlich - verkehrsbelastet", NA),
    concentration = 1:5
  )

  result <- prepare_plot_airquality(data, years = 2020, parameters = "NO2", siteclass_levels = siteclasses)

  expect_equal(result$concentration, 2L)
  expect_s3_class(result$siteclass, "factor")
  expect_equal(levels(result$siteclass), siteclasses)
})

make_ndep <- function() {
  tibble::tibble(
    year = 2020, site = c("A", "A", "B", "C", "D"),
    ecosys = c("Wald", "Hochmoor", "Siedlungen", NA, "Wald"),
    cln = c(10, 5, NA, 10, NA),
    pollutant = "Ndep", metric = "Jahressumme", unit = "kg/ha/a",
    deposition = c(20, 10, 30, 40, 50)
  )
}

test_that("prepare_plot_ndep() drops sites without a sensitive ecosystem or critical load", {
  expect_equal(prepare_plot_ndep(make_ndep())$deposition, c(20, 10))
})

test_that("prepare_plot_ndep_components() sums the parameters per source category", {
  data <- make_ndep() |>
    dplyr::slice(c(1, 1, 1, 2)) |>
    dplyr::mutate(source_cat = c("aus NH3-Quellen", "aus NH3-Quellen", "aus NOx-Quellen", "aus NOx-Quellen"),
                  deposition = c(1, 2, 4, 8))

  result <- prepare_plot_ndep_components(data)

  expect_equal(nrow(result), 3)
  expect_equal(dplyr::filter(result, ecosys == "Wald", component == "aus NH3-Quellen")$deposition, 3)
  expect_equal(levels(result$component), c("aus NOx-Quellen", "aus NH3-Quellen"))
  expect_equal(levels(result$ecosys), c("Wald", "Trockenrasen", "Flachmoor", "Hochmoor"))
})

# ---- thresholds ---------------------------------------------------------------------

test_that("combine_thresholds() adds the LRV and WHO values of pollutant and metric", {
  data <- tibble::tibble(
    year = 2020, site = "A", pollutant = c("NO2", "PM10", "eBC"), metric = "Jahresmittel",
    parameter = pollutant, unit = "µg/m3", concentration = c(20, 10, 1), siteclass = "x", source = "s"
  )

  result <- combine_thresholds(data, make_threshold_values())

  expect_equal(result$`LRV Grenzwert`, c(30, 20, NA))
  expect_equal(result$`WHO Richtwert`, c(10, 15, NA))
  expect_equal(result$concentration, c(20, 10, 1))
})

test_that("timeseriespars() takes the thresholds from the given threshold values", {
  thresholds <- make_threshold_values()

  no2 <- timeseriespars("NO2", thresholds)
  expect_equal(no2$ylim, c(0, 70))
  expect_equal(no2$thresh$value, c(30, 10))
  expect_equal(no2$thresh$labels, c("LRV Grenzwert", "WHO Richtwert"))

  expect_equal(timeseriespars("O3_max_98p_m1", thresholds)$thresh$value, 100)
  expect_true(is.na(timeseriespars("eBC", thresholds)$thresh$value))
})

test_that("threshold_comparison_data() relates concentrations and depositions to their thresholds", {
  airquality <- tibble::tibble(
    year = c(2019, 2020, 2020), site = "A", pollutant = "NO2", metric = "Jahresmittel", parameter = "NO2",
    unit = "µg/m3", concentration = c(60, 15, 45), source = "s",
    siteclass = factor(c("städtisch - Hintergrund", "städtisch - Hintergrund", "städtisch - verkehrsbelastet"), levels = siteclasses)
  )
  ndep <- tibble::tibble(year = c(2019, 2020), pollutant = "Ndep", metric = "Jahressumme", deposition = c(40, 30), cln = 10)

  result <- threshold_comparison_data(airquality, ndep, make_threshold_values(), years = 2020)

  lrv <- "relativ zu Immissionsgrenzwerten bzw. kritischen Eintragsraten:"
  who <- "relativ zu Richtwerten der Weltgesundheitsorganisation:"
  expect_equal(dplyr::filter(result, reference == lrv, siteclass == "städtisch - Hintergrund")$value, 0.5)
  expect_equal(dplyr::filter(result, reference == who, siteclass == "städtisch - verkehrsbelastet")$value, 4.5)
  expect_equal(dplyr::filter(result, siteclass == "empf. Ökosystem")$value, 3)
  expect_equal(dplyr::filter(result, siteclass == "empf. Ökosystem")$reference, lrv)
  expect_setequal(as.character(result$x), c("NO2 Jahresmittel", "Stickstoffeintrag in empf. Ökosysteme "))
  expect_equal(nrow(result), 5)
})

# ---- plots -------------------------------------------------------------------------

test_that("plot_pars_monitoring_timeseries() gives one plot per parameter, with its thresholds", {
  data <- tibble::tibble(
    year = 2020:2021, site = "A", pollutant = "NO2", metric = "Jahresmittel", parameter = "NO2", unit = "µg/m3",
    concentration = c(20, 25), siteclass = factor("städtisch - Hintergrund", levels = siteclasses)
  )

  plots <- plot_pars_monitoring_timeseries(data, "NO2", threshold_values = make_threshold_values(),
                                           colour_scale = ggplot2::scale_color_discrete())

  expect_named(plots, "NO2")
  layers <- layer_data_all(plots$NO2)
  expect_equal(layers[[2]]$yintercept, c(30, 10))
})

test_that("the nitrogen deposition plots are built from the given data", {
  data <- prepare_plot_ndep(make_ndep()) |>
    dplyr::mutate(estimated_class = "<5 kg-N")

  sites <- plot_ndep_sites(data, colour_scale = ggplot2::scale_color_discrete(),
                           fill_scale = ggplot2::scale_fill_discrete(), shape_scale = ggplot2::scale_shape_discrete())
  vs_cln <- plot_ndep_sites_vs_cln(data, colour_scale = ggplot2::scale_color_discrete())

  expect_equal(sort(sites$data$deposition), c(10, 20))
  expect_equal(layer_data_all(vs_cln)[[1]]$yintercept[1], 1)
  expect_equal(vs_cln$data$deposition / vs_cln$data$cln, c(2, 2))
})
