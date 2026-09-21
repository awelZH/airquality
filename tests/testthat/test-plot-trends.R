# Unit tests for R/plot_trends.R. All inputs are synthetic; no network.

# ---- data --------------------------------------------------------------------------

test_that("emission_trends_relative() sums the subsectors and relates every year to the reference year", {
  withr::local_package("airquality.methods") # prepare_emission_trends() (WIP) calls longpollutant() without prefix
  emissions <- tibble::tibble(
    year = rep(2014:2016, each = 2), pollutant = "NOx", subsector_new = rep(c("a", "b"), 3),
    emission = c(10, 10, 5, 5, 4, NA)
  )

  result <- emission_trends_relative(emissions, reference_year = 2015)

  expect_equal(result$year, 2014:2016)
  expect_equal(result$value, c(2, 1, 0.4))
  expect_equal(unique(result$reference_year), 2015)
  expect_equal(unique(result$pollutant), airquality.methods::longpollutant("NOx"))
  expect_equal(unique(result$site), "Kanton Zürich")
})

make_trends_agg <- function() {
  tibble::tibble(
    year = 2020, pollutant = c("Stickstoffdioxid", "Ozon", "Ozon", "Stickstoffdioxid"),
    parameter = c("NO2", "O3_max_98p_m1", "O3_max_98p_m1", "NO2"),
    type = c("Trend", "Messwerte", "emission", "andere"), reference_year = 2015, site = "Kanton Zürich", value = 1:4
  )
}

test_that("trend_data_overview() keeps emission and medians, labels O3 with its metric", {
  result <- trend_data_overview(make_trends_agg())

  expect_equal(as.character(result$type), c("Median Trend", "Median Messwerte", "Emission"))
  expect_equal(levels(result$type), c("Emission", "Median Trend", "Median Messwerte"))
  expect_equal(result$pollutant[2], paste0("Ozon, ", airquality.methods::longmetric("O3_max_98p_m1")))
  expect_equal(result$pollutant[1], "Stickstoffdioxid")
})

test_that("trend_data_detailed() adds the trends per site up to the given year", {
  trends <- tibble::tibble(
    year = c(2020, 2020, 2020, 2030), site = c("A", "B", "C", "A"), pollutant = "Stickstoffdioxid", parameter = "NO2",
    type = c("Trend", "Trend", "gemessen", "Trend"), class = c("relative Immission", "Immission", "relative Immission", "relative Immission"),
    reference_year = 2015, value = 1:4
  )

  result <- trend_data_detailed(make_trends_agg(), trends, year_max = 2025)

  expect_equal(as.character(result$type), c("Emission", "Trend pro Standort"))
  expect_equal(result$site, c("Kanton Zürich", "A"))
  expect_equal(levels(result$type), c("Emission", "Trend pro Standort", "Median Messwerte"))
})

# ---- plots -------------------------------------------------------------------------

test_that("plot_emission_trends_relative() shows the change vs. the reference year", {
  data <- tibble::tibble(year = 2014:2016, pollutant = "Stickoxide", reference_year = 2015, value = c(2, 1, 0.4))

  plot <- plot_emission_trends_relative(data, reference_year = 2015)
  layers <- layer_data_all(plot)

  expect_equal(layers[[2]]$xintercept, 2015)
  expect_equal(layers[[3]]$y, c(1, 0, -0.6))
  expect_equal(plot$labels$subtitle, "Veränderung gegenüber dem Jahr 2015")
})

test_that("plot_timeseries_trend_relative() draws one reference year line per pollutant", {
  data <- tibble::tibble(
    year = rep(2014:2016, 2), pollutant = rep(c("A", "B"), each = 3), reference_year = rep(c(2015, 2014), each = 3),
    site = "Kanton Zürich", type = factor("Emission", levels = c("Emission", "Median Trend", "Median Messwerte")), value = 1
  )

  layers <- layer_data_all(plot_timeseries_trend_relative(data))

  expect_equal(sort(layers[[2]]$xintercept), c(2014, 2015))
  expect_equal(unique(layers[[3]]$y), 0)
})

test_that("plot_trends_per_pollutant() gives one plot per pollutant with the legend on the right", {
  data <- tibble::tibble(
    year = rep(2014:2016, 2), pollutant = rep(c("A", "B"), each = 3), reference_year = 2015,
    site = "Kanton Zürich", type = factor("Emission", levels = c("Emission", "Median Trend", "Median Messwerte")), value = 1:6
  )

  plots <- plot_trends_per_pollutant(plot_timeseries_trend_relative(data))

  expect_named(plots, c("A", "B"))
  expect_equal(unique(plots$B$data$pollutant), "B")
  expect_equal(layer_data_all(plots$B)[[3]]$y, c(3, 4, 5))
  expect_equal(plots$A$theme$legend.position, "right")
})
