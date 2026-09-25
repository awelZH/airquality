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
  expect_equal(plots$NO2$labels$caption, "Referenzwerte nach heutigem Stand, Daten: Ostluft & NABEL (BAFU & Empa)")
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

# ---- pollutant maps and model verification ------------------------------------------------

make_pollutant_maps <- function() {
  map <- stars::st_as_stars(sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 400, ymax = 400), crs = sf::st_crs(2056)),
                            dx = 100, values = 0)
  map[[1]][] <- 1:16
  names(map) <- "concentration"
  tibble::tibble(parameter = c("NO2", "NO2", "PM10"), year = c(2019, 2020, 2020), derived = c(TRUE, FALSE, FALSE),
                 stars = list(map, map * 2, map))
}

make_square <- function() {
  sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(400, 0), c(400, 400), c(0, 400), c(0, 0)))),
                                  crs = 2056))
}

test_that("plot_pollutant_maps() gives one centred map per year with the raster and the boundary", {
  maps <- plot_pollutant_maps(make_pollutant_maps(), "NO2", boundary = make_square(), crs = 2056,
                              caption = "BAFU", caption_derived = "abgeleitet")

  expect_named(maps, c("2019", "2020"))
  expect_s3_class(maps[["2020"]], "ggplot")
  expect_match(as.character(maps[["2020"]]$labels$subtitle), "2020", fixed = TRUE)
  expect_equal(nrow(layer_data_all(maps[["2020"]])[[1]]), 16)
  expect_s3_class(maps[["2020"]]$layers[[2]]$geom, "GeomSf")
  expect_equal(maps[["2020"]]$theme$plot.title$hjust, 0.5)
  expect_equal(maps[["2020"]]$theme$plot.caption$hjust, 0.5)
  # the caption of the derived year
  expect_equal(maps[["2019"]]$labels$caption, "abgeleitet")
  expect_equal(maps[["2020"]]$labels$caption, "BAFU")
})

test_that("plot_ndep_exceedance_maps() gives one map per model year in the wanted years", {
  data <- tidyr::expand_grid(year = c(1990, 2000, 2020), x = c(500, 1500), y = c(500, 1500)) |>
    dplyr::mutate(ndep_exmax = seq_along(x))

  maps <- plot_ndep_exceedance_maps(data, years = 1995:2025, boundary = make_square(), crs = 2056)

  expect_named(maps, c("2000", "2020"))
  expect_equal(nrow(layer_data_all(maps[["2020"]])[[1]]), 4)
  expect_match(as.character(maps[["2020"]]$labels$subtitle), "2020", fixed = TRUE)
  expect_equal(maps[["2020"]]$theme$plot.title$hjust, 0.5)
})

make_validation <- function() {
  tibble::tibble(
    year = rep(c(2019, 2020), each = 5), site = rep(letters[1:5], 2),
    siteclass = rep(c("städtisch - verkehrsbelastet", "ländlich - Hintergrund"), 5),
    traffic = rep(c("verkehrsbelastet", "Hintergrund"), 5),
    pollutant = "NO2", metric = "Jahresmittel", parameter = "NO2",
    concentration = c(10, 20, 30, 40, 50, 12, 22, 32, 42, 52),
    concentration_map = c(12, 19, 31, 38, 52, 13, 21, 33, 40, 50)
  )
}

test_that("plot_map_validation() draws one panel per traffic influence with its own robust line", {
  data <- make_validation()
  fit <- fit_map_validation(data)

  plot <- plot_map_validation(data, fit, "NO2")

  geoms <- purrr::map_chr(unname(plot$layers), \(layer) class(layer$geom)[1])
  expect_equal(geoms, c("GeomAbline", "GeomPoint", "GeomSegment"))
  expect_contains(legend_texts(plot), c("1:1-Linie", "robuste Regression", "Jahr"))
  expect_null(plot$mapping$shape)
  expect_null(plot$layers[[2]]$mapping$shape)
  expect_match(as.character(plot$labels$title), "Stickstoffdioxid")
  expect_match(as.character(plot$labels$subtitle), "2019–2020", fixed = TRUE)
  expect_false(grepl("1:1|in-sample", plot$labels$caption))

  built <- withr::with_pdf(NULL, ggplot2::ggplot_build(plot))
  # two panels, "verkehrsbelastet" first, each regression line over the measured range of its sites
  expect_equal(as.character(built$layout$layout$traffic), c("verkehrsbelastet", "Hintergrund"))
  # a y axis in both panels
  expect_equal(plot$facet$params$draw_axes$y, TRUE)
  segments <- built$data[[3]]
  expect_equal(nrow(segments), 2)
  expect_equal(segments$x[order(segments$PANEL)], c(10, 12))
  expect_equal(segments$xend[order(segments$PANEL)], c(50, 52))
  # the year as a continuous colour scale
  expect_s3_class(plot$scales$get_scales("colour"), "ScaleContinuous")
  # the same range on both axes and in both panels
  expect_equal(built$layout$panel_params[[1]]$x.range, built$layout$panel_params[[1]]$y.range)
  expect_equal(built$layout$panel_params[[1]]$x.range, built$layout$panel_params[[2]]$x.range)
})

test_that("print_map_tabset() puts the map slider and the verification into two tabs", {
  plot <- ggplot2::ggplot() + ggplot2::ggtitle("x")
  catalog <- dplyr::bind_rows(
    airquality.methods::plot_catalog(list(NO2 = list(`2019` = plot, `2020` = plot)), "map", names_to = c("parameter", "year")),
    airquality.methods::plot_catalog(list(NO2 = plot), "map_validation", names_to = "parameter")
  )

  output <- withr::with_pdf(NULL, utils::capture.output(print_map_tabset(catalog, "NO2")))

  expect_contains(output, c("::: {.panel-tabset}", "##### Karte", "##### Modellverifikation"))
  expect_match(output[grep("year-slider", output)[1]], 'data-start="2020"', fixed = TRUE)
  expect_lt(grep("##### Karte", output), grep("year-slider", output)[1])
  expect_lt(grep("year-slider", output)[1], grep("##### Modellverifikation", output))
})

test_that("print_timeseries_map() prints the time series, a heading and the map tabset one level deeper", {
  plot <- ggplot2::ggplot() + ggplot2::ggtitle("x")
  catalog <- dplyr::bind_rows(
    airquality.methods::plot_catalog(list(O3 = plot), "timeseries_siteclass", names_to = "parameter"),
    airquality.methods::plot_catalog(list(O3 = list(`2020` = plot)), "map", names_to = c("parameter", "year")),
    airquality.methods::plot_catalog(list(O3 = plot), "map_validation", names_to = "parameter")
  )

  output <- withr::with_pdf(NULL, utils::capture.output(print_timeseries_map(catalog, "O3")))

  expect_contains(output, c("###### Belastungskarte", "::: {.panel-tabset}", "###### Karte", "###### Modellverifikation"))
  expect_false(any(grepl("^##### ", output))) # no heading of the level of the surrounding tabs
  expect_lt(grep("###### Belastungskarte", output), grep("panel-tabset", output)[1])
})

test_that("plot_map_validation() flags a derived map as in-sample", {
  data <- make_validation() |> dplyr::mutate(parameter = "O3_peakseason_mean_d1_max_mean_h8gl", pollutant = "O3")
  fit <- fit_map_validation(data)

  plot <- plot_map_validation(data, fit, "O3_peakseason_mean_d1_max_mean_h8gl", in_sample = TRUE)

  expect_match(plot$labels$caption, "^in-sample")
})
