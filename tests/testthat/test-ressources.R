# data/meta/ressources.csv: the register of the data sources (documentation and endpoints), read by the code with
# filter_ressources(<key>); the schema is described in data/meta/README.md

root <- testthat::test_path("..", "..")

register <- function() read_ressources(file.path(root, "data/meta/ressources.csv"))

# matches of `pattern` (first group) in the R and Quarto files of the given directories
code_matches <- function(dirs, pattern) {
  files <- list.files(file.path(root, dirs), pattern = "[.](R|qmd)$", recursive = TRUE, full.names = TRUE)
  purrr::map(files, \(file) stringr::str_match_all(paste(readLines(file, encoding = "UTF-8", warn = FALSE), collapse = "\n"), pattern)[[1]][, 2]) |>
    unlist() |>
    unique() |>
    sort()
}

ressource_columns <- c("ID", "KEY", "TITLE", "DESCRIPTION", "PROVIDER", "ACCESS", "SOURCE_TYPE", "ENDPOINT",
                       "LANDING_PAGE", "FORMAT", "LICENSE", "UNIT", "TEMPORAL", "USED_BY")
source_types <- c("opendataswiss", "geoadmin_stac", "geolion_wfs", "r_package", "repo_file", "restricted_file", "reference")


# ---- schema ------------------------------------------------------------------------------

test_that("ressources.csv has the documented columns, unique ids and keys and known types", {
  ressources <- register()

  expect_named(ressources, ressource_columns)
  expect_equal(anyDuplicated(ressources$ID), 0)
  expect_equal(anyDuplicated(ressources$KEY), 0)
  expect_true(all(grepl("^[a-z0-9_]+$", ressources$KEY)))
  expect_true(all(!is.na(ressources$TITLE)))
  expect_in(ressources$ACCESS, c("public", "restricted"))
  expect_in(ressources$SOURCE_TYPE, source_types)
  expect_true(all(!is.na(ressources$ENDPOINT[ressources$SOURCE_TYPE != "reference"])))
  expect_true(all(ressources$SOURCE_TYPE[ressources$ACCESS == "restricted"] == "restricted_file"))
})

test_that("prepare_ressources() refuses a table without the documented columns", {
  expect_error(prepare_ressources(tibble::tibble(KEY = "a", ENDPOINT = "x")), class = "airquality_input_error")
})


# ---- access by key -----------------------------------------------------------------------

test_that("filter_ressources() returns the endpoint of exactly one key", {
  ressources <- tibble::tibble(KEY = c("a", "b", "b"), ENDPOINT = c("x", "y", "z"))

  expect_equal(filter_ressources(ressources, "a"), "x")
  expect_error(filter_ressources(ressources, "b"), "2 rows", class = "airquality_input_error")
  expect_error(filter_ressources(ressources, "c"), "a", class = "airquality_input_error")
})


# ---- consistency of code and register ------------------------------------------------------

test_that("every key the code asks for is in ressources.csv", {
  keys <- code_matches(c("pipelines", "report", "R", "tests/regression"), "filter_ressources\\([^,()]+,\\s*\"([a-z0-9_]+)\"\\)")

  expect_gt(length(keys), 0)
  expect_in(keys, register()$KEY)
})

test_that("every geo.admin collection of the code is a geoadmin_stac row", {
  ressources <- register()

  expect_in(c(unname(exposition_collections), ndep_collection),
            ressources$ENDPOINT[ressources$SOURCE_TYPE == "geoadmin_stac"])
})

test_that("every metadata file in data/meta is a repo_file row, and every repo_file exists", {
  ressources <- register()
  repo_files <- ressources$ENDPOINT[ressources$SOURCE_TYPE == "repo_file"]
  meta_files <- file.path("data/meta", setdiff(list.files(file.path(root, "data/meta"), pattern = "[.]csv$"), "ressources.csv"))

  expect_in(meta_files, repo_files)
  expect_true(all(file.exists(file.path(root, repo_files))))
})

test_that("every airquality.data object of the pipelines and wip is an r_package row", {
  objects <- code_matches(c("pipelines", "wip"), "(airquality\\.data::[A-Za-z0-9_.]+)")
  ressources <- register()

  expect_gt(length(objects), 0)
  expect_in(objects, ressources$ENDPOINT[ressources$SOURCE_TYPE == "r_package"])
})

test_that("every file in USED_BY exists, and only references are used by no file", {
  ressources <- register()
  used_by <- unlist(strsplit(stats::na.omit(ressources$USED_BY), ",\\s*"))

  expect_true(all(file.exists(file.path(root, used_by))))
  expect_equal(is.na(ressources$USED_BY), ressources$SOURCE_TYPE == "reference")
})

test_that("read_ressources() reads the register as UTF-8", {
  path <- withr::local_tempfile(fileext = ".csv")
  ressources <- tibble::as_tibble(rlang::set_names(as.list(rep("x", length(ressource_columns))), ressource_columns))
  ressources$PROVIDER <- "Amt für Abfall, Wasser, Energie und Luft"
  readr::write_delim(ressources, path, delim = ";")

  expect_equal(read_ressources(path)$PROVIDER, "Amt für Abfall, Wasser, Energie und Luft")
})
