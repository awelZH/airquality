# Unit tests for R/plot_exposition.R. All inputs are synthetic; no network.

# ---- population over thresholds ------------------------------------------------------

make_distribution <- function() {
  tibble::tibble(
    year = 2020,
    pollutant = c("NO2", "NO2", "NO2", "O3", "O3", "O3", "O3"),
    metric = "Jahresmittel",
    parameter = c("NO2", "NO2", "NO2", "O3_max_98p_m1", "O3_max_98p_m1", "O3_peakseason_mean_d1_max_mean_h8gl",
                  "O3_peakseason_mean_d1_max_mean_h8gl"),
    concentration = c(5, 20, 40, 90, 110, 50, 70),
    population = c(100, 200, 50, 300, 20, 250, 70)
  )
}

make_weighted_means <- function() {
  tibble::tibble(
    year = 2020, pollutant = c("NO2", "O3", "O3"),
    parameter = c("NO2", "O3_max_98p_m1", "O3_peakseason_mean_d1_max_mean_h8gl"),
    population = c(350, 320, 320)
  )
}

test_that("population_over_thresholds() counts inhabitants over LRV, additionally over WHO, and below", {
  result <- population_over_thresholds(make_distribution(), make_weighted_means(), make_threshold_values())

  no2 <- dplyr::filter(result, pollutant == airquality.methods::longpollutant("NO2"))
  expect_equal(no2$population[no2$reference == "über LRV-Grenzwert"], 50)
  expect_equal(no2$population[no2$reference == "über WHO-Richtwert"], 200)
  expect_equal(no2$population[no2$reference == "unter Grenz-/Richtwert"], 100)
  expect_equal(levels(result$reference), c("unter Grenz-/Richtwert", "über WHO-Richtwert", "über LRV-Grenzwert"))
})

test_that("population_over_thresholds() uses the LRV of the O3 peak and the WHO value of the peak season", {
  result <- population_over_thresholds(make_distribution(), make_weighted_means(), make_threshold_values())

  o3 <- dplyr::filter(result, pollutant == airquality.methods::longpollutant("O3"))
  expect_equal(o3$population[o3$reference == "über LRV-Grenzwert"], 20)
  expect_equal(o3$population[o3$reference == "über WHO-Richtwert"], 70 - 20)
  expect_equal(o3$population[o3$reference == "unter Grenz-/Richtwert"], 320 - 70)
})

test_that("population_over_thresholds() sets small negative remainders to 0", {
  weighted_means <- dplyr::mutate(make_weighted_means(), population = population - 400)

  result <- population_over_thresholds(make_distribution(), weighted_means, make_threshold_values())

  expect_true(all(result$population >= 0))
})

test_that("the plots of the population over thresholds use the last years for the shares", {
  data <- dplyr::bind_rows(
    population_over_thresholds(make_distribution(), make_weighted_means(), make_threshold_values()),
    population_over_thresholds(dplyr::mutate(make_distribution(), year = 2019), dplyr::mutate(make_weighted_means(), year = 2019),
                               make_threshold_values())
  )
  colours <- c("über LRV-Grenzwert" = "red3", "über WHO-Richtwert" = "gray30", "unter Grenz-/Richtwert" = "gray60")

  timeseries <- plot_population_over_thresholds(data, colours)
  share <- plot_population_over_thresholds_share(data, n_years = 1, colours = colours)

  expect_s3_class(timeseries, "ggplot")
  expect_equal(sum(share$data$population), sum(data$population[data$year == 2020]))
  expect_equal(share$labels$subtitle, "Anteil Personen im Kanton Zürich in den Jahren 2020 bis 2020")
})

# ---- distributions -------------------------------------------------------------------

test_that("plot_exposition_histograms() and plot_exposition_cumulative() give one plot per year, with the thresholds", {
  data <- tibble::tibble(
    year = rep(2019:2020, each = 3), pollutant = "NO2", metric = "Jahresmittel", parameter = "NO2",
    concentration = rep(c(5, 15, 35), 2), population = 10, population_cum_rel = rep(c(1, 2, 3) / 3, 2)
  )

  axes <- list(NO2 = list(barwidth = 0.9, xbreaks = seq(0, 55, 5)))
  hist <- plot_exposition_histograms(data, "NO2", make_threshold_values(), axes = axes)
  cumul <- plot_exposition_cumulative(data, "NO2", make_threshold_values(), axes = axes)

  expect_named(hist, c("2019", "2020"))
  expect_named(cumul, c("alle", "2019", "2020"))
  expect_equal(layer_data_all(hist[["2020"]])[[2]]$xintercept, c(30, 10))
  expect_equal(sort(unique(layer_data_all(cumul$alle)[[2]]$group)), 1:2)
})

test_that("the nitrogen distribution plots give one plot per year plus all years", {
  data <- tibble::tibble(year = rep(2019:2020, each = 2), ndep_exmax = c(1, 5, 1, 5), n_ecosys = 1,
                         n_ecosys_cum_rel = c(0.5, 1, 0.5, 1))
  threshold <- list(value = 0, color = "red3", labels = "kritische Eintragsrate CLN", labelsize = 4, linetype = 1, linesize = 1)
  axes <- list(Ndep = list(barwidth = 0.9, xbreaks = seq(-5, 45, 5)))

  expect_named(plot_ndep_exposition_histograms(data, threshold, axes = axes), c("2019", "2020"))
  expect_named(plot_ndep_exposition_cumulative(data, threshold, axes = axes), c("alle", "2019", "2020"))
})

# ---- population-weighted means -------------------------------------------------------

test_that("plot_weighted_mean_timeseries() shows the actual mean and the reduction vs. the base year", {
  data <- tibble::tibble(
    year = 2014:2016, pollutant = "NO2", parameter = "NO2", base_year = 2015,
    population_weighted_mean = c(30, 25, 20), population_weighted_mean_base = 25
  )

  plots <- plot_weighted_mean_timeseries(data, "NO2")

  expect_named(plots, "NO2")
  expect_setequal(unique(plots$NO2$data$scenario), c("tatsächliche Belastung", "vermindert vs. 2015"))
  expect_equal(plots$NO2$data$population_weighted_mean[plots$NO2$data$scenario == "vermindert vs. 2015"], c(0, 0, -5))
})

test_that("plot_weighted_mean_maps() gives one map per year with the canton mean in the subtitle", {
  square <- function(x) sf::st_polygon(list(rbind(c(x, 0), c(x + 1, 0), c(x + 1, 1), c(x, 1), c(x, 0))))
  data <- sf::st_sf(
    year = rep(2019:2020, each = 2), pollutant = "NO2", parameter = "NO2", population_weighted_mean = c(10, 20, 12, 22),
    geometry = sf::st_sfc(square(0), square(1), square(0), square(1), crs = 2056)
  )
  canton <- tibble::tibble(year = 2019:2020, parameter = "NO2", population_weighted_mean = c(15.04, 17.06))

  maps <- plot_weighted_mean_maps(data, canton, "NO2", crs = 2056)

  expect_named(maps, c("2019", "2020"))
  expect_match(as.character(maps[["2020"]]$labels$subtitle), "gesamt = 17.1", fixed = TRUE)
})

test_that("table_population_over_thresholds() gives one row per pollutant and year, cumulated over WHO", {
  data <- tibble::tibble(
    year = rep(c(2019, 2020), each = 3), pollutant = "Stickstoffdioxid",
    reference = factor(rep(c("unter Grenz-/Richtwert", "über WHO-Richtwert", "über LRV-Grenzwert"), 2),
                       levels = c("unter Grenz-/Richtwert", "über WHO-Richtwert", "über LRV-Grenzwert")),
    population = c(1000, 200000, 30, 2000, 100000, 0)
  )

  result <- table_population_over_thresholds(data)

  expect_named(result, c("Jahr", "Schadstoff", "< Grenz-/Richtwert", "> WHO-Richtwert", "> LRV-Grenzwert"))
  expect_equal(result$Jahr, c(2020, 2019))
  expect_equal(trimws(result$`> WHO-Richtwert`), c("100'000", "200'030"))
  expect_equal(trimws(result$`< Grenz-/Richtwert`), c("2'000", "1'000"))
})
