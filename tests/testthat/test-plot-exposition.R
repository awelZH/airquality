# Unit tests for R/plot_exposition.R. All inputs are synthetic; no network.

# ---- population over thresholds ------------------------------------------------------

make_distribution <- function() {
  tibble::tibble(
    year = 2020,
    pollutant = c("NO2", "NO2", "NO2", "O3", "O3", "O3", "O3"),
    metric = c("Jahresmittel", "Jahresmittel", "Jahresmittel", "typische Spitzenbelastung", "typische Spitzenbelastung",
               "mittlere Sommertagbelastung", "mittlere Sommertagbelastung"),
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

test_that("population_over_thresholds() counts each O3 metric against its own threshold only", {
  result <- population_over_thresholds(make_distribution(), make_weighted_means(), make_threshold_values())

  peak <- dplyr::filter(result, parameter == "O3_max_98p_m1")
  expect_equal(as.character(peak$reference), c("über LRV-Grenzwert", "unter Grenz-/Richtwert"))
  expect_equal(peak$population, c(20, 320 - 20))

  season <- dplyr::filter(result, parameter == "O3_peakseason_mean_d1_max_mean_h8gl")
  expect_equal(as.character(season$reference), c("über WHO-Richtwert", "unter Grenz-/Richtwert"))
  expect_equal(season$population, c(70, 320 - 70))
})

test_that("population_over_thresholds() names the metric only for pollutants with several metrics", {
  result <- population_over_thresholds(make_distribution(), make_weighted_means(), make_threshold_values())

  labels <- dplyr::distinct(result, parameter, pollutant)
  expect_equal(labels$pollutant[labels$parameter == "NO2"], airquality.methods::longpollutant("NO2"))
  expect_equal(labels$pollutant[labels$parameter == "O3_max_98p_m1"],
               paste0(airquality.methods::longpollutant("O3"), " (typische Spitzenbelastung)"))
})

test_that("population_over_thresholds() sets small negative remainders to 0", {
  weighted_means <- dplyr::mutate(make_weighted_means(), population = population - 400)

  result <- population_over_thresholds(make_distribution(), weighted_means, make_threshold_values())

  expect_true(all(result$population >= 0))
})

test_that("the doughnut plot of the population over thresholds uses the last years for the shares", {
  data <- dplyr::bind_rows(
    population_over_thresholds(make_distribution(), make_weighted_means(), make_threshold_values()),
    population_over_thresholds(dplyr::mutate(make_distribution(), year = 2019), dplyr::mutate(make_weighted_means(), year = 2019),
                               make_threshold_values())
  )
  colours <- c("über LRV-Grenzwert" = "red3", "über WHO-Richtwert" = "gray30", "unter Grenz-/Richtwert" = "gray60")

  share <- plot_population_over_thresholds_share(data, n_years = 1, colours = colours)

  expect_equal(sum(share$data$population), sum(data$population[data$year == 2020]))
  expect_equal(share$labels$subtitle, "Anteil Personen im Kanton Zürich in den Jahren 2020 bis 2020")
})

test_that("the doughnut plot works with a theme whose caption is a ggtext textbox", {
  data <- population_over_thresholds(make_distribution(), make_weighted_means(), make_threshold_values())
  colours <- c("über LRV-Grenzwert" = "red3", "über WHO-Richtwert" = "gray30", "unter Grenz-/Richtwert" = "gray60")
  theme <- ggplot2::theme_minimal() + ggplot2::theme(plot.caption = ggtext::element_textbox_simple())

  expect_no_error(withr::with_pdf(NULL, ggplot2::ggplotGrob(
    plot_population_over_thresholds_share(data, n_years = 1, colours = colours, theme = theme)
  )))
})

test_that("plot_population_over_thresholds() gives one plot per parameter, without panels, named in the title", {
  data <- dplyr::bind_rows(
    population_over_thresholds(make_distribution(), make_weighted_means(), make_threshold_values()),
    population_over_thresholds(dplyr::mutate(make_distribution(), year = 2019), dplyr::mutate(make_weighted_means(), year = 2019),
                               make_threshold_values())
  )
  colours <- c("über LRV-Grenzwert" = "red3", "über WHO-Richtwert" = "gray30", "unter Grenz-/Richtwert" = "gray60")

  plots <- plot_population_over_thresholds(data, c("NO2", "O3_max_98p_m1"), colours)

  expect_named(plots, c("NO2", "O3_max_98p_m1"))
  expect_equal(unique(plots$O3_max_98p_m1$data$parameter), "O3_max_98p_m1")
  expect_equal(sum(plots$NO2$data$population), sum(data$population[data$parameter == "NO2"]))
  expect_s3_class(plots$NO2$facet, "FacetNull")
  expect_match(as.character(plots$NO2$labels$title), airquality.methods::longpollutant("NO2"), fixed = TRUE)
  expect_match(as.character(plots$O3_max_98p_m1$labels$title), "typische Spitzenbelastung", fixed = TRUE)
  expect_equal(plots$NO2$labels$subtitle, "Anzahl Personen, Wohnbevölkerung im Kanton Zürich")
  expect_equal(plots$NO2$theme$legend.position, "right")
  expect_setequal(legend_texts(plots$NO2), names(colours))
  expect_setequal(legend_texts(plots$O3_max_98p_m1), c("über LRV-Grenzwert", "unter Grenz-/Richtwert")) # no WHO guideline
})

# ---- page section per parameter ------------------------------------------------------

test_that("print_exposition_parameter() prints the text, the distributions and a tabset canton / municipalities", {
  plot <- ggplot2::ggplot()
  catalog <- dplyr::bind_rows(
    airquality.methods::plot_catalog(list(NO2 = list(`2020` = plot)), "distribution_histogram", names_to = c("parameter", "year")),
    airquality.methods::plot_catalog(list(NO2 = list(alle = plot, `2020` = plot)), "distribution_cumulative", names_to = c("parameter", "year")),
    airquality.methods::plot_catalog(list(NO2 = plot), "population_over_thresh", names_to = "parameter"),
    airquality.methods::plot_catalog(list(NO2 = plot), "population_weighted_mean", names_to = "parameter"),
    airquality.methods::plot_catalog(list(NO2 = list(`2020` = plot)), "population_weighted_mean_map", names_to = c("parameter", "year"))
  )

  output <- withr::with_pdf(NULL, utils::capture.output(print_exposition_parameter(catalog, "NO2", text = "Erläuterung")))

  headings <- c("#### Belastungsverteilung", "#### Entwicklung luftschadstoffbelastete Bevölkerung", "#### mittlere Bevölkerungsbelastung")
  expect_contains(output, c("Erläuterung", headings, "##### alle Jahre", "##### Kanton", "##### Gemeinden"))
  expect_lt(which(output == "Erläuterung"), which(output == headings[1]))
  expect_lt(which(output == headings[1]), grep("year-slider", output)[1])
  expect_lt(which(output == "##### alle Jahre"), which(output == headings[2]))
  expect_lt(which(output == headings[2]), which(output == headings[3]))
  expect_lt(which(output == headings[3]), which(output == "##### Kanton"))
  expect_lt(which(output == "##### Kanton"), which(output == "##### Gemeinden"))
})

# ---- ecosystems over the critical load -----------------------------------------------

test_that("ecosystems_over_critical_load() counts the ecosystems with and without exceedance per year", {
  distribution <- tibble::tibble(
    year = rep(c(2015, 2020), each = 3), ndep_exmax = c(-0.5, 0.5, 10.5, -1.5, 0.5, 20.5), n_ecosys = c(2, 5, 7, 1, 6, 8)
  )

  result <- ecosystems_over_critical_load(distribution)

  expect_named(result, c("year", "reference", "n_ecosys"))
  expect_equal(levels(result$reference), c("unter krit. Eintragsrate", "über krit. Eintragsrate"))
  over <- dplyr::filter(result, reference == "über krit. Eintragsrate")
  below <- dplyr::filter(result, reference == "unter krit. Eintragsrate")
  expect_equal(over$n_ecosys[over$year == 2015], 12)
  expect_equal(below$n_ecosys[below$year == 2020], 1)
  expect_equal(sum(result$n_ecosys), sum(distribution$n_ecosys))
})

test_that("ecosystems_over_critical_load() keeps a year without ecosystems below the critical load", {
  distribution <- tibble::tibble(year = 2020, ndep_exmax = c(0.5, 3.5), n_ecosys = c(4, 6))

  result <- ecosystems_over_critical_load(distribution)

  expect_equal(result$n_ecosys[result$reference == "unter krit. Eintragsrate"], 0)
  expect_equal(result$n_ecosys[result$reference == "über krit. Eintragsrate"], 10)
})

test_that("plot_ecosystems_over_critical_load() stacks the ecosystems like the population plots", {
  data <- ecosystems_over_critical_load(
    tibble::tibble(year = rep(c(2015, 2020), each = 2), ndep_exmax = c(-0.5, 2.5, -0.5, 2.5), n_ecosys = c(1, 9, 2, 8))
  )
  colours <- c("über krit. Eintragsrate" = "red3", "unter krit. Eintragsrate" = "gray60")

  plot <- plot_ecosystems_over_critical_load(data, colours)

  expect_s3_class(plot, "ggplot")
  expect_s3_class(plot$facet, "FacetNull")
  expect_equal(plot$labels$subtitle, "Anzahl empfindlicher Ökosysteme im Kanton Zürich")
  expect_setequal(legend_texts(plot), names(colours))
  bars <- layer_data_all(plot)[[1]]
  expect_equal(as.vector(tapply(bars$ymax, bars$x, max)), c(10, 10)) # stacked: the top of the bars is the total per year
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

test_that("table_population_over_thresholds() gives one row per O3 metric, with a dash where it has no threshold", {
  data <- population_over_thresholds(make_distribution(), make_weighted_means(), make_threshold_values())

  result <- table_population_over_thresholds(data)

  o3 <- airquality.methods::longpollutant("O3")
  peak <- dplyr::filter(result, Schadstoff == paste0(o3, " (typische Spitzenbelastung)"))
  season <- dplyr::filter(result, Schadstoff == paste0(o3, " (mittlere Sommertagbelastung)"))
  expect_equal(nrow(result), 3)
  expect_equal(trimws(c(peak$`> LRV-Grenzwert`, peak$`> WHO-Richtwert`, peak$`< Grenz-/Richtwert`)), c("20", "–", "300"))
  expect_equal(trimws(c(season$`> LRV-Grenzwert`, season$`> WHO-Richtwert`, season$`< Grenz-/Richtwert`)), c("–", "70", "250"))
})

test_that("the cumulative plot of all years has a two-column legend", {
  data <- tibble::tibble(year = rep(2019:2020, each = 2), ndep_exmax = c(1, 5, 1, 5), n_ecosys_cum_rel = c(0.5, 1, 0.5, 1))
  threshold <- list(value = 0, color = "red3", labels = "CLN", labelsize = 4, linetype = 1, linesize = 1)

  plot <- ggplot_exposition_cumulative_years(data, "ndep_exmax", "n_ecosys_cum_rel", xbreaks = seq(0, 5, 1),
                                             threshold = threshold, xlabel = NULL, title = "t", subtitle = "s", caption = "c")

  expect_equal(plot$guides$guides[[1]]$params$ncol, 2)
})

test_that("a cumulative plot of one year draws the other years in the background, without a colour legend", {
  data <- tibble::tibble(year = rep(2019:2021, each = 2), concentration = rep(c(1, 5), 3),
                         population_cum_rel = rep(c(0.5, 1), 3))
  threshold <- list(value = NA)

  plot <- ggplot_exposition_cumulative(dplyr::filter(data, year == 2021), "concentration", "population_cum_rel",
                                       threshold = threshold, background = data)
  layers <- layer_data_all(plot)

  # first layer: all years in grey, second layer: the year itself in the usual colour
  expect_equal(length(unique(layers[[1]]$group)), 3)
  expect_equal(unique(layers[[1]]$colour), "gray80")
  expect_lt(unique(layers[[1]]$alpha), 1)
  expect_equal(unique(layers[[2]]$colour), "#50586C")
  expect_null(plot$labels$colour)
})

test_that("the thresholds of a distribution are named in the legend, not written into the panel", {
  data <- tibble::tibble(
    year = 2020, pollutant = "NO2", metric = "Jahresmittel", parameter = "NO2",
    concentration = c(5, 15, 35), population = 10, population_cum_rel = c(1, 2, 3) / 3
  )
  axes <- list(NO2 = list(barwidth = 0.9, xbreaks = seq(0, 55, 5)))

  hist <- plot_exposition_histograms(data, "NO2", make_threshold_values(), axes = axes)[["2020"]]
  cumul <- plot_exposition_cumulative(data, "NO2", make_threshold_values(), axes = axes)[["2020"]]

  expect_contains(legend_texts(hist), c("LRV Grenzwert", "WHO Richtwert"))
  expect_contains(legend_texts(cumul), c("LRV Grenzwert", "WHO Richtwert"))
  expect_false(any(purrr::map_lgl(hist$layers, \(layer) inherits(layer$geom, "GeomText"))))
})

test_that("a distribution with only the threshold legend shows no legend title", {
  data <- tibble::tibble(
    year = 2020, pollutant = "NO2", metric = "Jahresmittel", parameter = "NO2",
    concentration = c(5, 15, 35), population = 10, population_cum_rel = c(1, 2, 3) / 3
  )
  axes <- list(NO2 = list(barwidth = 0.9, xbreaks = seq(0, 55, 5)))

  hist <- plot_exposition_histograms(data, "NO2", make_threshold_values(), axes = axes)[["2020"]]

  expect_contains(legend_texts(hist), "LRV Grenzwert")
  expect_false("Referenz" %in% legend_texts(hist))
})

test_that("the plot of all years shows the thresholds without a legend title", {
  data <- tibble::tibble(year = rep(2019:2020, each = 2), ndep_exmax = c(1, 5, 1, 5), n_ecosys_cum_rel = c(0.5, 1, 0.5, 1))
  threshold <- list(value = 0, color = "red3", labels = "krit. Eintragsrate", labelsize = 4, linetype = 1, linesize = 1)

  plot <- ggplot_exposition_cumulative_years(data, "ndep_exmax", "n_ecosys_cum_rel", xbreaks = seq(0, 5, 1),
                                             threshold = threshold, xlabel = NULL, title = "t", subtitle = "s", caption = "c")

  expect_contains(legend_texts(plot), c("krit. Eintragsrate", "Jahr")) # the year legend keeps its title
  expect_false("Referenz" %in% legend_texts(plot))
})
