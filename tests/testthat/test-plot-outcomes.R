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

test_that("plot_health_outcomes() gives one plot per parameter with the premature deaths only (default)", {
  plots <- plot_health_outcomes(make_outcomes(), c("NO2", "PM2.5"))

  expect_named(plots, c("NO2", "PM2.5"))
  expect_equal(nrow(plots$NO2$data), 6)
  expect_false(999 %in% plots$NO2$data$outcome)
})

test_that("plot_health_outcomes() marks the Covid years", {
  plots <- plot_health_outcomes(make_outcomes(), "NO2", covid_years = 2020)

  expect_equal(unique(plots$NO2$data$covid[plots$NO2$data$year == 2020]), "Covid-19")
  expect_equal(unique(plots$NO2$data$covid[plots$NO2$data$year != 2020]), "normal")
})

test_that("plot_health_outcomes() shows deaths per 100'000 inhabitants when relative", {
  absolute <- layer_data_all(plot_health_outcomes(make_outcomes(), "NO2")$NO2)
  relative <- layer_data_all(plot_health_outcomes(make_outcomes(), "NO2", relative = TRUE)$NO2)

  expect_equal(max(absolute[[1]]$ymax), 200)
  expect_equal(max(relative[[1]]$ymax), 100)
  # uncertainty range: lower bound up to upper bound plus the part below the minimum concentration
  expect_equal(unique(relative[[3]]$ymax), (120 + 10) / 200000 * 1e5)
})

test_that("plot_health_outcomes() draws the uncertainty only for the actual exposure", {
  plots <- plot_health_outcomes(make_outcomes(), "NO2")
  uncertainty <- layer_data_all(plots$NO2)[[3]] # bars, zero line, then the uncertainty ranges

  expect_equal(nrow(uncertainty), 3) # three years of the actual exposure, not six rows for both scenarios
  expect_equal(unique(uncertainty$ymax), 120 + 10)
})

test_that("plot_health_outcomes() plots the years of life lost with their own labels", {
  plots <- plot_health_outcomes(make_outcomes(), "NO2", outcome_type = "verlorene Lebensjahre")
  relative <- plot_health_outcomes(make_outcomes(), "NO2", outcome_type = "verlorene Lebensjahre", relative = TRUE)

  expect_equal(plots$NO2$data$outcome, 999)
  expect_match(as.character(plots$NO2$labels$title), "Verlorene Lebensjahre durch", fixed = TRUE)
  expect_equal(plots$NO2$labels$subtitle, "Anzahl verlorene Lebensjahre pro Jahr")
  expect_equal(relative$NO2$labels$subtitle, "Anzahl verlorene Lebensjahre pro 100'000 Einwohner/innen pro Jahr")
})

test_that("plot_health_outcomes() keeps the labels of the premature deaths", {
  plots <- plot_health_outcomes(make_outcomes(), "NO2")
  relative <- plot_health_outcomes(make_outcomes(), "NO2", relative = TRUE)

  expect_match(as.character(plots$NO2$labels$title), "Vorzeitige Todesfälle durch", fixed = TRUE)
  expect_equal(plots$NO2$labels$subtitle, "Anzahl vorzeitige Todesfälle pro Jahr")
  expect_equal(relative$NO2$labels$subtitle, "Anzahl vorzeitige Todesfälle pro 100'000 Einwohner/innen pro Jahr")
})

test_that("plot_health_outcomes() stops for an unknown outcome type", {
  expect_error(plot_health_outcomes(make_outcomes(), "NO2", outcome_type = "Spitaleintritte"), class = "airquality_plot_error")
})

test_that("plot_life_years_per_death() divides the years of life lost by the premature deaths of the actual exposure", {
  plots <- plot_life_years_per_death(make_outcomes(), c("NO2", "PM2.5"), covid_years = 2020)

  expect_named(plots, c("NO2", "PM2.5"))
  expect_equal(plots$NO2$data$year, 2020) # only years with both outcomes
  expect_equal(plots$NO2$data$life_years_per_death, 999 / 100)
  expect_equal(plots$NO2$data$covid, "Covid-19")
  expect_equal(nrow(plots$PM2.5$data), 0)
  expect_equal(plots$NO2$labels$subtitle, "verlorene Lebensjahre pro vorzeitigen Todesfall")
  expect_false("Szenario" %in% legend_texts(plots$NO2)) # one scenario only, no legend for it
})
