# Unit tests for R/plot.R (building blocks shared by all report pages). All inputs are synthetic; no
# network.

# ---- plotlist_to_tibble() -----------------------------------------------------------

test_that("plotlist_to_tibble() gives one row per plot of a flat list, with year 'various'", {
  plots <- list(NO2 = ggplot2::ggplot(), PM10 = ggplot2::ggplot())

  result <- plotlist_to_tibble(plots, "monitoring", "timeseries")

  expect_equal(result$pollutant, c("NO2", "PM10"))
  expect_equal(unique(result$type), "monitoring")
  expect_equal(unique(result$source), "timeseries")
  expect_equal(unique(result$year), "various")
  expect_s3_class(result$plot[[1]], "ggplot")
})

test_that("plotlist_to_tibble() gives one row per pollutant and year of a nested list", {
  plots <- list(
    NO2 = list(alle = ggplot2::ggplot(), `2020` = ggplot2::ggplot()),
    PM10 = list(alle = ggplot2::ggplot(), `2020` = ggplot2::ggplot(), `2021` = ggplot2::ggplot())
  )

  result <- plotlist_to_tibble(plots, "exposition", "distribution_cumulative")

  expect_equal(result$pollutant, c("NO2", "NO2", "PM10", "PM10", "PM10"))
  expect_equal(result$year, c("alle", "2020", "alle", "2020", "2021"))
})

# ---- get_plot(), build_panel() -------------------------------------------------------

test_that("get_plot() returns the first plot matching the filter", {
  first <- ggplot2::ggplot() + ggplot2::ggtitle("first")
  plots <- tibble::tibble(
    pollutant = c("NO2", "NO2", "PM10"), source = c("a", "a", "b"),
    plot = list(first, ggplot2::ggplot(), ggplot2::ggplot())
  )

  expect_identical(get_plot(plots, "pollutant == 'NO2' & source == 'a'"), first)
})

test_that("build_panel() writes a year heading and a labelled chunk printing the plot", {
  result <- build_panel(3, 2020, "PM2.5", "distribution_histogram")

  expect_match(result, "^##### 2020")
  expect_match(result, "#| label: exposition-pm2.5-distribution-histogram-2020", fixed = TRUE)
  expect_match(result, "plots$plot[[3]]", fixed = TRUE)
})
