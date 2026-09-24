# Unit tests for R/pipeline.R. All inputs are synthetic; no network.

test_that("write_output() writes the contract CSV into the directory and returns its path", {
  dir <- withr::local_tempdir()
  data <- tibble::tibble(year = 2020, value = 1 / 3)

  path <- write_output(data, "data_test.csv", dir = file.path(dir, "output"))

  expect_equal(path, file.path(dir, "output", "data_test.csv"))
  expect_equal(readLines(path, encoding = "UTF-8"), c("year;value", "2020;0.3333333333333333"))
})

test_that("append_output() appends to an existing file", {
  dir <- withr::local_tempdir()
  data <- tibble::tibble(run = "a", value = 1)

  append_output(data, "log.csv", dir = dir)
  path <- append_output(dplyr::mutate(data, run = "b"), "log.csv", dir = dir)

  expect_equal(readLines(path), c("run;value", "a;1", "b;1"))
})

test_that("geo_admin_asset_state() keeps what shows a change of the assets, sorted", {
  fake <- function(collection) {
    tibble::tibble(
      collection = collection, item = "i", year = c(2021L, 2020L), asset = c("b.tif", "a.tif"), format = "tif",
      href = paste0("https://x/", c("b", "a")), created = "2026-01-01", updated = c("2026-02-01", "2026-01-01"),
      `file:checksum` = c("cb", "ca")
    )
  }

  result <- geo_admin_asset_state(c(no2 = "ch.no2", pm10 = "ch.pm10"), get_assets = fake)

  expect_named(result, c("collection", "year", "asset", "href", "updated", "checksum"))
  expect_equal(result$collection, c("ch.no2", "ch.no2", "ch.pm10", "ch.pm10"))
  expect_equal(result$year, c(2020L, 2021L, 2020L, 2021L))
  expect_equal(result$checksum[1], "ca")
})

test_that("restricted_file() returns an existing path and stops with a hint otherwise", {
  file <- withr::local_tempfile(lines = "x")

  expect_equal(restricted_file(file), file)
  expect_error(restricted_file(file.path(tempdir(), "missing.csv")), "README", class = "airquality_input_error")
})

test_that("check_wip_outputs() warns about work-in-progress outputs older than their inputs or missing", {
  dir <- withr::local_tempdir()
  wip <- file.path(dir, "wip.csv")
  input <- file.path(dir, "input.csv")
  writeLines("x", wip)
  writeLines("x", input)
  Sys.setFileTime(wip, Sys.time() - 3600)
  Sys.setFileTime(input, Sys.time())

  expect_warning(result <- check_wip_outputs(c(wip, file.path(dir, "gone.csv")), based_on = input), "older")
  expect_equal(result$status, c("older than its inputs", "missing"))

  Sys.setFileTime(wip, Sys.time() + 60)
  expect_no_warning(result <- check_wip_outputs(wip, based_on = input))
  expect_equal(result$status, "up to date")
})
