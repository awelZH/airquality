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
