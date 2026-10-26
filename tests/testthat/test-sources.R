# The inventory of the data sources derived on every run (R/sources.R): years, spatial resolution, version

test_that("compact_years() writes runs of consecutive years as ranges", {
  expect_equal(compact_years(c(2010:2025, 1990, 2000, 2000)), "1990, 2000, 2010–2025")
  expect_equal(compact_years(c(2019, 2020)), "2019–2020")
  expect_equal(compact_years(2020), "2020")
  expect_equal(compact_years(c(NA, NA)), NA_character_)
  expect_equal(compact_years(numeric()), NA_character_)
})

test_that("resolution_ranges() condenses the cell size per year to ranges", {
  expect_equal(resolution_ranges(c(2021, 1990:2020), c(20, rep(200, 30), 20)), "200 m 1990–2019, 20 m 2020–2021")
  expect_equal(resolution_ranges(c(1990, 2000), c(1000, 1000)), "1 km")
  expect_equal(resolution_ranges(c(2015, 2016), c(100, 100)), "100 m")
  expect_equal(resolution_ranges(numeric(), numeric()), NA_character_)
})

test_that("cell_sizes() takes the spacing of the cell centres per year", {
  cells <- tibble::tibble(x = c(50, 150, 350, 500, 1500), year = c(2020, 2020, 2020, 2021, 2021))

  result <- cell_sizes(cells, "ch.x")

  expect_equal(result, tibble::tibble(endpoint = "ch.x", year = c(2020, 2021), res = c(100, 1000)))
})

test_that("package_versions() gives the installed version of the package of every r_package source", {
  ressources <- tibble::tibble(KEY = c("a", "b"), SOURCE_TYPE = c("r_package", "repo_file"),
                               ENDPOINT = c("stats::median", "data/meta/x.csv"))

  expect_equal(package_versions(ressources), list(a = as.character(utils::packageVersion("stats"))))
})

test_that("sources_inventory() gives one row per key, from assets, data years, grids and versions", {
  ressources <- tibble::tibble(KEY = c("map_no2", "emikat", "thresholds"),
                               ENDPOINT = c("ch.no2", "https://ckan/emikat", "data/meta/t.csv"))
  assets <- tibble::tibble(collection = "ch.no2", year = c(2019L, 2020L, 2020L),
                           updated = c("2025-02-28T12:03:29Z", "2025-03-01T08:00:00Z", "2025-03-01T08:00:00Z"))
  grids <- tibble::tibble(endpoint = "ch.no2", year = c(2019, 2020), res = c(200, 20))

  result <- sources_inventory(ressources, assets = assets, years = list(emikat = c(1990:2025, 2030)),
                              grids = grids, versions = list(emikat = "2026-02-06T14:12:28+01:00"))

  expect_equal(result$KEY, c("map_no2", "emikat", "thresholds"))
  expect_equal(result$years, c("2019–2020", "1990–2025, 2030", NA))
  expect_equal(result$resolution, c("200 m 2019, 20 m 2020", NA, NA))
  expect_equal(result$version, c("2025-03-01", "2026-02-06", NA))
})

test_that("log_sources_inventory() appends a run and names the sources that changed", {
  dir <- withr::local_tempdir()
  first <- tibble::tibble(KEY = c("a", "b"), years = c("2019–2020", "2020"), resolution = NA_character_, version = NA_character_)
  second <- dplyr::mutate(first, years = c("2019–2021", "2020"))

  log_sources_inventory(first, "inventory.csv", dir, run = "2026-10-23 10:00:00")
  messages <- paste(testthat::capture_messages(
    path <- log_sources_inventory(second, "inventory.csv", dir, run = "2026-10-24 10:00:00")
  ), collapse = "")

  expect_match(messages, "changed")
  expect_match(messages, "\"a\"", fixed = TRUE)
  expect_no_match(messages, "\"b\"", fixed = TRUE)

  log <- readr::read_delim(path, delim = ";", show_col_types = FALSE)
  expect_equal(nrow(log), 4)
  expect_named(log, c("run", "KEY", "years", "resolution", "version"))
})

test_that("log_sources_inventory() names the sources removed from the register", {
  dir <- withr::local_tempdir()
  first <- tibble::tibble(KEY = c("a", "b"), years = "2020", resolution = NA_character_, version = NA_character_)

  log_sources_inventory(first, "inventory.csv", dir, run = "2026-10-23 10:00:00")
  messages <- paste(testthat::capture_messages(
    log_sources_inventory(first[1, ], "inventory.csv", dir, run = "2026-10-24 10:00:00")
  ), collapse = "")

  expect_match(messages, "\"b\"", fixed = TRUE)
  expect_no_match(messages, "\"a\"", fixed = TRUE)
})

test_that("log_sources_inventory() appends nothing when the inventory is unchanged", {
  dir <- withr::local_tempdir()
  inventory <- tibble::tibble(KEY = c("a", "b"), years = c("2020", NA), resolution = NA_character_, version = NA_character_)

  log_sources_inventory(inventory, "inventory.csv", dir, run = "2026-10-23 10:00:00")
  expect_no_message(path <- log_sources_inventory(inventory, "inventory.csv", dir, run = "2026-10-24 10:00:00"))

  expect_equal(nrow(readr::read_delim(path, delim = ";", show_col_types = FALSE)), 2)
})

test_that("sources_table() joins register and inventory into the German table of the website, without references", {
  ressources <- tibble::tibble(
    KEY = c("map_no2", "mortality", "old"), TITLE = c("NO2-Karten", "Todesfälle", "Alt"),
    PROVIDER = c("BAFU", "Statistisches Amt", "BFS"), ACCESS = c("public", "restricted", "public"),
    SOURCE_TYPE = c("geoadmin_stac", "restricted_file", "reference"),
    LANDING_PAGE = c("https://x/no2", NA, "https://x/old"), LICENSE = c("frei, Quellenangabe", NA, NA)
  )
  inventory <- tibble::tibble(KEY = c("map_no2", "mortality", "old"), years = c("1990–2025", "1996–2025", NA),
                              resolution = c("200 m 1990–2019", NA, NA), version = c("2025-03-01", NA, NA))

  table <- sources_table(ressources, inventory)

  expect_named(table, c("Datengrundlage", "Urheber", "Jahre", "Auflösung", "Stand", "Zugang"))
  expect_equal(table$Datengrundlage, c("[NO2-Karten](https://x/no2)", "Todesfälle"))
  expect_equal(table$Zugang, c("öffentlich; frei, Quellenangabe", "nicht öffentlich"))
  expect_equal(table$Auflösung, c("200 m 1990–2019", ""))
})
