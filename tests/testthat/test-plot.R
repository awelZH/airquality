# Unit tests for R/plot.R (building blocks shared by all report pages). All inputs are synthetic; no
# network.

titled <- function(title) ggplot2::ggplot() + ggplot2::ggtitle(title)

# plot_catalog(), get_plot(), catalog_entries() and print_tabset() are tested in airquality.methods; the
# catalogs here are built with them.

# ---- print_year_slider() ---------------------------------------------------------------

test_that("print_year_slider() starts at the newest year", {
  catalog <- airquality.methods::plot_catalog(list(`2019` = titled("a"), `2021` = titled("b")), "ndep_hist", names_to = "year")

  output <- withr::with_pdf(NULL, utils::capture.output(print_year_slider(catalog, "ndep_hist")))

  expect_match(output[grep("year-slider", output)[1]], 'data-start="2021"', fixed = TRUE)
})

test_that("print_year_slider() stops for plots without years or unknown plots", {
  catalog <- airquality.methods::plot_catalog(list(NO2 = titled("a")), "timeseries", names_to = "parameter")

  expect_error(print_year_slider(catalog, "timeseries", "NO2"), class = "airquality_plot_error")
  expect_error(print_year_slider(catalog, "unknown"), class = "plot_catalog_error")
})

# ---- parameter_setting() ------------------------------------------------------------

test_that("parameter_setting() returns the setting of a parameter and stops for unknown parameters", {
  axes <- list(NO2 = list(ylim = c(0, 70)), PM10 = list(ylim = c(0, 35)))

  expect_equal(parameter_setting(axes, "PM10")$ylim, c(0, 35))
  expect_error(parameter_setting(axes, "SO2"), "NO2", class = "airquality_plot_error")
})

test_that("print_year_slider() puts an 'alle' plot into its own tab, next to the slider of the years", {
  catalog <- airquality.methods::plot_catalog(
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
  catalog <- airquality.methods::plot_catalog(
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
