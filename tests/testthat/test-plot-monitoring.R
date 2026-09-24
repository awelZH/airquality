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

test_that("timeseries_threshold() takes the thresholds of a parameter from the given threshold values", {
  thresholds <- make_threshold_values()

  no2 <- timeseries_threshold("NO2", thresholds)
  expect_equal(no2$value, c(30, 10))
  expect_equal(no2$labels, c("LRV Grenzwert", "WHO Richtwert"))

  expect_equal(timeseries_threshold("O3_max_98p_m1", thresholds)$value, 100)
  expect_true(is.na(timeseries_threshold("eBC", thresholds)$value))
  expect_error(timeseries_threshold("SO2", thresholds), class = "airquality_plot_error")
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

test_that("plot_monitoring_timeseries() gives one plot per parameter, with its thresholds", {
  data <- tibble::tibble(
    year = 2020:2021, site = "A", pollutant = "NO2", metric = "Jahresmittel", parameter = "NO2", unit = "µg/m3",
    concentration = c(20, 25), siteclass = factor("städtisch - Hintergrund", levels = siteclasses)
  )

  plots <- plot_monitoring_timeseries(data, "NO2", axes = list(NO2 = list(ylim = c(0, 70), ybreaks = seq(0, 70, 10))), threshold_values = make_threshold_values(),
                                           colour_scale = ggplot2::scale_color_discrete())

  expect_named(plots, "NO2")
  layers <- layer_data_all(plots$NO2)
  expect_equal(layers[[1]]$yintercept, c(30, 10)) # the thresholds are drawn first, behind the points
  expect_equal(sort(layers[[2]]$y), c(20, 25))
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

test_that("jittered points move only across the value axis, never along it", {
  data <- prepare_plot_ndep(make_ndep()) |>
    dplyr::mutate(estimated_class = "<5 kg-N")

  sites <- plot_ndep_sites(data, colour_scale = ggplot2::scale_color_discrete(),
                           fill_scale = ggplot2::scale_fill_discrete(), shape_scale = ggplot2::scale_shape_discrete())
  vs_cln <- plot_ndep_sites_vs_cln(data, colour_scale = ggplot2::scale_color_discrete())
  expect_setequal(layer_data_all(sites)[[1]]$y, c(20, 10))
  expect_equal(layer_data_all(vs_cln)[[2]]$y, c(2, 2))

  comparison <- tibble::tibble(
    x = factor(c("NO2 Jahresmittel", "NO2 Jahresmittel", "PM10 Jahresmittel")), value = c(0.5, 1.5, 0.75),
    siteclass = "städtisch - Hintergrund", reference = "relativ"
  )
  plot <- plot_threshold_comparison(comparison, threshold_styles = tibble::tibble(col = "red3", lty = 1, lsz = 1), years = 2020,
                                    limits = tibble::tibble(reference = "relativ", value = 2),
                                    categories = threshold_comparison_categories(comparison))
  points <- purrr::detect(layer_data_all(plot), \(layer) "shape" %in% names(layer))
  expect_setequal(points$y, c(0.5, 1.5, 0.75))
})

test_that("year_windows() gives the moving windows of the given width, labelled by their range", {
  windows <- year_windows(c(2018, 2019, 2020, 2021), width = 3)

  expect_named(windows, c("2018–2020", "2019–2021"))
  expect_equal(windows[["2019–2021"]], 2019:2021)
})

test_that("year_windows() needs at least one full window", {
  expect_error(year_windows(2019:2020, width = 3), class = "airquality_plot_error")
})

test_that("plot_threshold_comparison() keeps the categories of each panel and the given maximum", {
  levels <- c("Stickstoffeintrag", "PM10 Jahresmittel", "NO2 Jahresmittel")
  # the window holds NO2 only; the nitrogen deposition belongs to panel A, PM10 to both
  data <- tibble::tibble(
    x = factor("NO2 Jahresmittel", levels = levels), value = 0.5, siteclass = "städtisch - Hintergrund",
    reference = "A:"
  )
  categories <- tibble::tibble(
    reference = c("A:", "A:", "A:", "B:", "B:"),
    x = factor(c("Stickstoffeintrag", "PM10 Jahresmittel", "NO2 Jahresmittel", "PM10 Jahresmittel", "NO2 Jahresmittel"), levels = levels)
  )
  limits <- tibble::tibble(reference = c("A:", "B:"), value = c(4, 2))

  plot <- plot_threshold_comparison(data, threshold_styles = tibble::tibble(col = "red3", lty = 1, lsz = 1),
                                    years = 2020, limits = limits, categories = categories)
  build <- withr::with_pdf(NULL, ggplot2::ggplot_build(plot))
  panels <- purrr::map(build$layout$panel_params, \(panel) panel$y$get_labels())

  expect_equal(panels[[1]], levels) # all categories of the panel, in the order of the levels
  expect_false("Stickstoffeintrag" %in% panels[[2]])
  expect_gte(max(build$layout$panel_params[[1]]$x.range), 4)
  expect_false("NA" %in% legend_texts(plot)) # the filled categories must not reach the legend
  expect_match(plot$labels$caption, "^Referenzwerte nach heutigem Stand") # the thresholds are the current ones
})

test_that("the thresholds of a time series are named in the legend, not written into the panel", {
  data <- tibble::tibble(
    year = 2020:2021, site = "A", pollutant = "NO2", metric = "Jahresmittel", parameter = "NO2", unit = "µg/m3",
    concentration = c(20, 25), siteclass = factor("städtisch - Hintergrund", levels = siteclasses)
  )

  plot <- plot_monitoring_timeseries(data, "NO2", axes = list(NO2 = list(ylim = c(0, 70), ybreaks = seq(0, 70, 10))),
                                     threshold_values = make_threshold_values(),
                                     colour_scale = ggplot2::scale_color_discrete())[["NO2"]]

  expect_contains(legend_texts(plot), c("LRV Grenzwert", "WHO Richtwert"))
  expect_false(any(purrr::map_lgl(plot$layers, \(layer) inherits(layer$geom, "GeomText"))))
})

test_that("the threshold legend of a time series carries no title", {
  data <- tibble::tibble(
    year = 2020:2021, site = "A", pollutant = "NO2", metric = "Jahresmittel", parameter = "NO2", unit = "µg/m3",
    concentration = c(20, 25), siteclass = factor("städtisch - Hintergrund", levels = siteclasses)
  )

  plot <- plot_monitoring_timeseries(data, "NO2", axes = list(NO2 = list(ylim = c(0, 70), ybreaks = seq(0, 70, 10))),
                                     threshold_values = make_threshold_values(),
                                     colour_scale = ggplot2::scale_color_discrete())[["NO2"]]

  expect_contains(legend_texts(plot), c("LRV Grenzwert", "WHO Richtwert"))
  expect_false("Referenz" %in% legend_texts(plot)) # the labels speak for themselves
})

test_that("the nitrogen plots name the critical load in the legend and in the caption", {
  data <- prepare_plot_ndep(make_ndep()) |>
    dplyr::mutate(estimated_class = "<5 kg-N", component = factor("aus NH3-Quellen"))

  bars <- plot_ndep_bars(data)
  vs_cln <- plot_ndep_sites_vs_cln(data)

  expect_contains(legend_texts(bars), "krit. Eintragsrate")
  expect_contains(legend_texts(vs_cln), "krit. Eintragsrate")
  expect_false("Referenz" %in% c(legend_texts(bars), legend_texts(vs_cln))) # the label speaks for itself
  expect_match(vs_cln$labels$caption, "^krit. Eintragsraten nach heutigem Stand")
})

test_that("plot_ndep_sites_vs_cln() shows the exceedance in absolute values as well", {
  data <- prepare_plot_ndep(make_ndep()) |>
    dplyr::mutate(year = 2020, estimated_class = "<5 kg-N")

  relative <- plot_ndep_sites_vs_cln(data)
  absolute <- plot_ndep_sites_vs_cln(data, relative = FALSE)

  # deposition 20 with a critical load of 10, and 10 with 5
  expect_equal(layer_data_all(relative)[[2]]$y, c(2, 2))
  expect_equal(layer_data_all(absolute)[[2]]$y, c(10, 5))
  expect_equal(layer_data_all(relative)[[1]]$yintercept[1], 1)
  expect_equal(layer_data_all(absolute)[[1]]$yintercept[1], 0)
})
