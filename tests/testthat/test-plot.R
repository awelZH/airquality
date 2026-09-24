# Unit tests for R/plot.R (building blocks shared by all report pages). All inputs are synthetic; no
# network.

titled <- function(title) ggplot2::ggplot() + ggplot2::ggtitle(title)

# ---- plot_catalog() -----------------------------------------------------------------

test_that("plot_catalog() gives one row for a single plot, without parameter and year", {
  result <- plot_catalog(titled("a"), "rsd_norm")

  expect_named(result, c("plot", "parameter", "year", "figure"))
  expect_equal(result$plot, "rsd_norm")
  expect_true(is.na(result$parameter))
  expect_true(is.na(result$year))
  expect_s3_class(result$figure[[1]], "ggplot")
})

test_that("plot_catalog() takes the names of a list as parameter or year", {
  per_parameter <- plot_catalog(list(NO2 = titled("a"), PM10 = titled("b")), "timeseries", names_to = "parameter")
  per_year <- plot_catalog(list(`2020` = titled("a"), `2021` = titled("b")), "ndep_hist", names_to = "year")

  expect_equal(per_parameter$parameter, c("NO2", "PM10"))
  expect_true(all(is.na(per_parameter$year)))
  expect_equal(per_year$year, c("2020", "2021"))
  expect_true(all(is.na(per_year$parameter)))
})

test_that("plot_catalog() takes the names of a nested list as parameter and year", {
  plots <- list(
    NO2 = list(alle = titled("a"), `2020` = titled("b")),
    PM10 = list(`2020` = titled("c"))
  )

  result <- plot_catalog(plots, "distribution_cumulative", names_to = c("parameter", "year"))

  expect_equal(result$parameter, c("NO2", "NO2", "PM10"))
  expect_equal(result$year, c("alle", "2020", "2020"))
  expect_equal(result$figure[[3]]$labels$title, "c")
})

test_that("plot_catalog() stops on unnamed lists", {
  expect_error(plot_catalog(list(titled("a")), "x", names_to = "parameter"), "named")
})

# ---- get_plot() ---------------------------------------------------------------------

make_catalog <- function() {
  dplyr::bind_rows(
    plot_catalog(titled("single"), "rsd_norm"),
    plot_catalog(list(NO2 = titled("no2"), PM10 = titled("pm10")), "timeseries", names_to = "parameter"),
    plot_catalog(list(NO2 = list(`2020` = titled("no2 2020"), `2021` = titled("no2 2021"))), "map", names_to = c("parameter", "year"))
  )
}

test_that("get_plot() returns the one plot matching plot, parameter and year", {
  catalog <- make_catalog()

  expect_equal(get_plot(catalog, "rsd_norm")$labels$title, "single")
  expect_equal(get_plot(catalog, "timeseries", "PM10")$labels$title, "pm10")
  expect_equal(get_plot(catalog, "map", "NO2", 2021)$labels$title, "no2 2021")
})

test_that("get_plot() stops if no plot or several plots match, naming the available ones", {
  catalog <- make_catalog()

  expect_error(get_plot(catalog, "timeseries", "O3"), class = "airquality_plot_error")
  expect_error(get_plot(catalog, "timeseries", "O3"), "PM10")
  expect_error(get_plot(catalog, "timeseries"), class = "airquality_plot_error")
  expect_error(get_plot(catalog, "tiemseries"), class = "airquality_plot_error")
})

# ---- print_year_slider(), print_tabset() ---------------------------------------------

test_that("print_year_slider() starts at the newest year", {
  catalog <- plot_catalog(list(`2019` = titled("a"), `2021` = titled("b")), "ndep_hist", names_to = "year")

  output <- withr::with_pdf(NULL, utils::capture.output(print_year_slider(catalog, "ndep_hist")))

  expect_match(output[grep("year-slider", output)[1]], 'data-start="2021"', fixed = TRUE)
})

test_that("print_year_slider() stops for plots without years or unknown plots", {
  catalog <- plot_catalog(list(NO2 = titled("a")), "timeseries", names_to = "parameter")

  expect_error(print_year_slider(catalog, "timeseries", "NO2"), class = "airquality_plot_error")
  expect_error(print_year_slider(catalog, "unknown"), class = "airquality_plot_error")
})

test_that("print_tabset() writes one tab per plot, titled by its name", {
  output <- withr::with_pdf(NULL, utils::capture.output(print_tabset(list(absolut = titled("a"), relativ = titled("b")))))

  expect_contains(output, c("::: {.panel-tabset}", "##### absolut", "##### relativ"))
})

# ---- parameter_setting() ------------------------------------------------------------

test_that("parameter_setting() returns the setting of a parameter and stops for unknown parameters", {
  axes <- list(NO2 = list(ylim = c(0, 70)), PM10 = list(ylim = c(0, 35)))

  expect_equal(parameter_setting(axes, "PM10")$ylim, c(0, 35))
  expect_error(parameter_setting(axes, "SO2"), "NO2", class = "airquality_plot_error")
})

test_that("print_year_slider() puts an 'alle' plot into its own tab, next to the slider of the years", {
  catalog <- plot_catalog(
    list(NO2 = list(`2021` = titled("b"), alle = titled("all"), `2020` = titled("a"))),
    "distribution_cumulative", names_to = c("parameter", "year")
  )

  output <- withr::with_pdf(NULL, utils::capture.output(print_year_slider(catalog, "distribution_cumulative", "NO2")))
  panels <- grep("year-panel", output, value = TRUE)

  expect_contains(output, c("::: {.panel-tabset}", "##### alle Jahre", "##### einzelne Jahre"))
  expect_equal(sub('.*data-year="([^"]+)".*', "\\1", panels), c("2020", "2021"))
  expect_match(output[grep("year-slider", output)[1]], 'data-start="2021"', fixed = TRUE)
})

test_that("print_year_slider() sorts labels of year ranges by their last year", {
  catalog <- plot_catalog(
    rlang::set_names(list(titled("a"), titled("b"), titled("c")), c("2020–2022", "2018–2020", "2019–2021")),
    "threshold_comparison", names_to = "year"
  )

  output <- withr::with_pdf(NULL, utils::capture.output(print_year_slider(catalog, "threshold_comparison")))
  panels <- grep("year-panel", output, value = TRUE)

  expect_equal(sub('.*data-year="([^"]+)".*', "\\1", panels), c("2018–2020", "2019–2021", "2020–2022"))
  expect_match(output[grep("year-slider", output)[1]], 'data-start="2020–2022"', fixed = TRUE)
})

# ---- axis labels ---------------------------------------------------------------------

test_that("label_big_mark() writes thousands with an apostrophe", {
  expect_equal(label_big_mark(c(5, 1000, 25000)), c("5", "1'000", "25'000"))
})

test_that("ggplot_timeseries_bars() takes the labels of the y axis", {
  data <- tibble::tibble(year = 2020:2021, value = c(1500, 2500), scenario = "a")

  plot <- ggplot_timeseries_bars(data, mapping = ggplot2::aes(x = year, y = value, fill = scenario), ylabels = label_big_mark)
  labels <- withr::with_pdf(NULL, ggplot2::ggplot_build(plot))$layout$panel_params[[1]]$y$get_labels()

  expect_contains(labels, "2'000")
})
