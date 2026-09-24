# Every package the project code uses must be declared in DESCRIPTION (the dependency manifest that
# renv snapshots with snapshot.type = "explicit").

test_that("every package used by the code is declared in DESCRIPTION", {
  skip_if_not_installed("renv")
  root <- testthat::test_path("..", "..")

  used <- unique(renv::dependencies(root, quiet = TRUE, progress = FALSE)$Package)
  fields <- read.dcf(file.path(root, "DESCRIPTION"), fields = c("Imports", "Suggests"))[1, ]
  declared <- trimws(sub("[(].*$", "", unlist(strsplit(paste(stats::na.omit(fields), collapse = ","), ","))))
  base <- rownames(utils::installed.packages(priority = "base"))

  expect_equal(sort(setdiff(used, c(declared, base, "renv"))), character())
})
