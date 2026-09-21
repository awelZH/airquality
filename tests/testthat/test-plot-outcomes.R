# Unit tests for R/plot_outcomes.R. All inputs are synthetic; no network.

make_outcomes <- function() {
  tidyr::expand_grid(
    year = 2019:2021, parameter = c("NO2", "PM2.5"), scenario = c("tatsächliche Belastung", "vermieden vs. 2015")
  ) |>
    dplyr::mutate(
      pollutant = parameter, outcome_type = "vorzeitige Todesfälle", population = 200000,
      outcome = 100, outcome_lower = 80, outcome_upper = 120, outcome_delta_min_conc = 10
    ) |>
    tibble::add_row(year = 2020, parameter = "NO2", scenario = "tatsächliche Belastung", pollutant = "NO2",
                    outcome_type = "verlorene Lebensjahre", population = 200000, outcome = 999)
}

test_that("plot_premature_deaths() gives one plot per parameter with the premature deaths only", {
  plots <- plot_premature_deaths(make_outcomes(), c("NO2", "PM2.5"))

  expect_named(plots, c("NO2", "PM2.5"))
  expect_equal(nrow(plots$NO2$data), 6)
  expect_false(999 %in% plots$NO2$data$outcome)
})

test_that("plot_premature_deaths() marks the Covid years", {
  plots <- plot_premature_deaths(make_outcomes(), "NO2", covid_years = 2020)

  expect_equal(unique(plots$NO2$data$covid[plots$NO2$data$year == 2020]), "Covid-19")
  expect_equal(unique(plots$NO2$data$covid[plots$NO2$data$year != 2020]), "normal")
})

test_that("plot_premature_deaths() shows deaths per 100'000 inhabitants when relative", {
  absolute <- layer_data_all(plot_premature_deaths(make_outcomes(), "NO2")$NO2)
  relative <- layer_data_all(plot_premature_deaths(make_outcomes(), "NO2", relative = TRUE)$NO2)

  expect_equal(max(absolute[[1]]$ymax), 200)
  expect_equal(max(relative[[1]]$ymax), 100)
  # uncertainty range: lower bound up to upper bound plus the part below the minimum concentration
  expect_equal(unique(relative[[3]]$ymax), (120 + 10) / 200000 * 1e5)
})
