# The output CSVs are a contract with external processes: file names, column
# names and column order must stay as in the frozen baseline.

baseline_dir <- function() {
  testthat::test_path("..", "regression", "baseline")
}

output_dir <- function() {
  testthat::test_path("..", "..", "data", "output")
}

read_header <- function(file) {
  strsplit(readLines(file, n = 1, encoding = "UTF-8"), ";", fixed = TRUE)[[1]]
}

test_that("every baseline output file still exists", {
  skip_if_not(dir.exists(baseline_dir()), "no baseline available")

  expect_setequal(list.files(output_dir(), "\\.csv$"), list.files(baseline_dir(), "\\.csv$"))
})

test_that("output files keep their columns and column order", {
  skip_if_not(dir.exists(baseline_dir()), "no baseline available")

  for (file in list.files(baseline_dir(), "\\.csv$")) {
    expect_identical(
      read_header(file.path(output_dir(), file)),
      read_header(file.path(baseline_dir(), file)),
      label = file
    )
  }
})

test_that("output files are semicolon-delimited with a single header line", {
  skip_if_not(dir.exists(output_dir()), "no outputs available")

  for (file in list.files(output_dir(), "\\.csv$", full.names = TRUE)) {
    lines <- readLines(file, encoding = "UTF-8")
    expect_equal(sum(lines == lines[1]), 1, label = basename(file))
  }
})
