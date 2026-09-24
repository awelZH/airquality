# The functions of the analysis (R/) for the tests: no package, the files are sourced like tar_source() does
# in _targets.R. Run the tests from the project root with testthat::test_dir("tests/testthat").
for (file in list.files(testthat::test_path("..", "..", "R"), pattern = "[.]R$", full.names = TRUE)) {
  source(file, local = TRUE, encoding = "UTF-8")
}
