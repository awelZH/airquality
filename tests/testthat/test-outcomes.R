# Unit tests for R/outcomes.R. All inputs are synthetic; no network.

test_that("drop_incomplete_years() keeps the years with a plausible number of deaths", {
  data <- tibble::tibble(year = 2020:2024, deaths = c(9000, 9500, 9200, 9400, 400))

  expect_message(result <- drop_incomplete_years(data, min_share = 0.8), "2024")
  expect_equal(result$year, 2020:2023)
})

test_that("drop_incomplete_years() keeps every year if all are complete", {
  data <- tibble::tibble(year = 2020:2023, deaths = c(9000, 9500, 9200, 9400))

  expect_equal(drop_incomplete_years(data, min_share = 0.8)$year, 2020:2023)
})

test_that("drop_incomplete_years() measures against the median, not the maximum", {
  # one exceptional year does not make the others incomplete
  data <- tibble::tibble(year = 2020:2023, deaths = c(9000, 9500, 14000, 9400))

  expect_equal(drop_incomplete_years(data, min_share = 0.8)$year, 2020:2023)
})

test_that("drop_incomplete_years() takes the columns it is given", {
  data <- tibble::tibble(jahr = 2020:2022, anzahl = c(500, 480, 20))

  result <- drop_incomplete_years(data, min_share = 0.5, year = jahr, count = anzahl)

  expect_equal(result$jahr, 2020:2021)
})

# ---- input preparation ---------------------------------------------------------------

make_mortality_raw <- function() {
  tibble::tibble(
    jahr = 2020,
    geschlecht = rep(c("männlich", "weiblich"), each = 5),
    alterkat = rep(c(30, 31, 32, 101, 290), 2),
    tukat = "krankheitsbedingt",
    anzahl = c(NA, 10, NA, 4, 6, 5, 20, 7, 3, 9)
  ) |>
    dplyr::bind_rows(tibble::tibble(jahr = 2020, geschlecht = "männlich", alterkat = 31, tukat = "andere", anzahl = 100))
}

make_population_raw <- function() {
  tidyr::expand_grid(
    jahr = 2019:2020, gemeinde = c("A", "B"), geschlecht = c("Mann", "Frau"), einjahresaltersklasse = 29:33
  ) |>
    dplyr::mutate(anzahl = ifelse(jahr == 2019, 100, 120))
}

test_that("prepare_mortality() keeps the natural deaths per year, sex and age", {
  result <- prepare_mortality(make_mortality_raw(), suppressed = 2)

  expect_named(result, c("year", "sex", "age", "deaths"))
  expect_setequal(unique(result$sex), c("male", "female"))
  female <- dplyr::filter(result, sex == "female")
  expect_equal(female$deaths[female$age == 31], 20)
  expect_false(any(result$deaths == 100)) # other causes of death are dropped
})

test_that("prepare_mortality() stops for missing columns", {
  expect_error(prepare_mortality(dplyr::select(make_mortality_raw(), -tukat)), class = "airquality_input_error")
})

test_that("prepare_population_by_age() sums the municipalities per year, sex and age", {
  result <- prepare_population_by_age(make_population_raw())

  expect_named(result, c("year", "sex", "age", "population"))
  expect_setequal(unique(result$sex), c("male", "female"))
  expect_equal(nrow(result), 2 * 2 * 5)
  expect_equal(result$population[result$year == 2020 & result$sex == "male" & result$age == 30], 240)
})

test_that("prepare_population_by_age() stops for missing columns", {
  expect_error(prepare_population_by_age(dplyr::select(make_population_raw(), -anzahl)), class = "airquality_input_error")
})

test_that("prepare_mortality() drops the deaths aged 0-29 and fills the suppressed cells", {
  result <- prepare_mortality(make_mortality_raw(), suppressed = 2)

  male <- dplyr::filter(result, sex == "male")
  expect_equal(male$deaths[male$age %in% c(30, 32)], c(2, 2)) # suppressed (1 to 3 deaths)
  expect_false(290 %in% result$age) # category 290 = deaths aged 0-29
  expect_equal(sum(result$deaths), (2 + 10 + 2 + 4) + (5 + 20 + 7 + 3))
})

test_that("deaths_per_year() sums the deaths from min_age", {
  mortality <- prepare_mortality(make_mortality_raw(), suppressed = 2)

  expect_equal(deaths_per_year(mortality, min_age = 30)$deaths, (2 + 10 + 2 + 4) + (5 + 20 + 7 + 3))
  expect_equal(deaths_per_year(mortality, min_age = 31)$deaths, (10 + 2 + 4) + (20 + 7 + 3))
  expect_equal(deaths_per_year(mortality, min_age = 30)$year, 2020)
})

# ---- estimates -----------------------------------------------------------------------

make_outcomes_meta <- function() {
  tibble::tibble(
    pollutant = "PM2.5", metric = "Jahresmittel", parameter = "PM2.5", outcome_type = "natural cause mortality",
    lower_conc_threshold = 5, crf = 1.118, crf_lower = 1.06, crf_upper = 1.179, crf_conc_increment = 10
  )
}

make_expo <- function() {
  tibble::tibble(
    year = c(2015, 2020), pollutant = "PM2.5", metric = "Jahresmittel", parameter = "PM2.5",
    population_weighted_mean_base = 12, base_year = 2015, population_weighted_mean = c(12, 9),
    population = c(1400000, 1500000), concentration_min = 3, source = "BAFU & BFS"
  )
}

test_that("outcome_scenarios() gives the actual burden and the avoided burden vs. the base year", {
  estimates <- tibble::tibble(
    year = rep(c(2015, 2020), each = 2), parameter = "PM2.5",
    scenario = rep(c("actual", "base"), 2),
    outcome = c(600, 600, 450, 600), outcome_lower = c(300, 300, 220, 300), outcome_upper = c(900, 900, 680, 900)
  )

  result <- outcome_scenarios(estimates, make_expo(), outcome_type = "vorzeitige Todesfälle")

  expect_named(result, c("year", "pollutant", "metric", "parameter", "outcome_type", "population", "scenario",
                         "outcome", "outcome_lower", "outcome_upper", "outcome_delta_min_conc"))
  avoided <- dplyr::filter(result, scenario == "vermieden vs. 2015")
  expect_equal(avoided$outcome[avoided$year == 2020], -150)
  expect_equal(avoided$outcome_lower[avoided$year == 2020], -80)
  expect_equal(avoided$outcome[avoided$year == 2015], 0) # no reduction: 0, without range
  expect_true(is.na(avoided$outcome_upper[avoided$year == 2015]))
  actual <- dplyr::filter(result, scenario == "tatsächliche Belastung")
  expect_equal(actual$outcome, c(600, 450))
  expect_equal(actual$population, c(1400000, 1500000))
  expect_equal(unique(result$outcome_delta_min_conc), 0)
})

test_that("estimate_premature_deaths() gives both scenarios with a range per year and parameter", {
  withr::local_seed(1)
  deaths <- tibble::tibble(year = c(2015, 2020), deaths = c(10000, 11000))

  result <- estimate_premature_deaths(make_expo(), deaths, make_outcomes_meta(), n_sim = 50)

  expect_named(result, c("year", "parameter", "scenario", "outcome", "outcome_lower", "outcome_upper"))
  expect_equal(nrow(result), 4)
  expect_true(all(result$outcome_lower <= result$outcome & result$outcome <= result$outcome_upper))
  r2020 <- dplyr::filter(result, year == 2020)
  expect_lt(r2020$outcome[r2020$scenario == "actual"], r2020$outcome[r2020$scenario == "base"]) # 9 < 12 µg/m3
})
