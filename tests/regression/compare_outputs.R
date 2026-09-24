# Compare two directories of output CSVs (e.g. reference vs. candidate of run_pipeline.R, or a run vs.
# data/output). Works for all 14 output files without per-file configuration.
#
# Per file common to both directories:
# * identical: byte-identical files
# * header: same column names and order (the contract)
# * rows: number of rows in both, and rows whose key is only in one of them
# * na_changed: numeric values that are NA in one file only (rows in both)
# * max_abs_rel: largest absolute relative deviation over all numeric columns
#   (keys = all non-numeric columns, plus integer-valued ones such as year until the key is unique;
#   if no unique key is found, rows are compared in file order)
#
# run from the project root:
#   source("tests/regression/compare_outputs.R")
#   compare_outputs("tests/regression/results/emissions/reference", "tests/regression/results/emissions/candidate")

read_output_csv <- function(file) {
  readr::read_delim(file, delim = ";", show_col_types = FALSE, progress = FALSE,
                    locale = readr::locale(encoding = "UTF-8"))
}

compare_output_file <- function(file_base, file_new) {
  if (identical(unname(tools::md5sum(file_base)), unname(tools::md5sum(file_new)))) {
    base <- read_output_csv(file_base)
    return(tibble::tibble(identical = TRUE, header = TRUE, rows_base = nrow(base), rows_new = nrow(base),
                          only_base = 0L, only_new = 0L, na_changed = 0L, max_abs_rel = 0))
  }

  base <- read_output_csv(file_base)
  new <- read_output_csv(file_new)
  header <- identical(names(base), names(new))
  common_cols <- intersect(names(base), names(new))
  numeric_cols <- common_cols[purrr::map_lgl(common_cols, \(col) is.numeric(base[[col]]) && is.numeric(new[[col]]))]
  keys <- setdiff(common_cols, numeric_cols)

  # integer-valued numeric columns (e.g. year) join the key, in column order, until it is unique
  is_unique <- function(keys) {
    length(keys) > 0 &&
      !anyDuplicated(dplyr::select(base, dplyr::all_of(keys))) &&
      !anyDuplicated(dplyr::select(new, dplyr::all_of(keys)))
  }
  integer_cols <- numeric_cols[purrr::map_lgl(numeric_cols, \(col) all(base[[col]] == round(base[[col]]), na.rm = TRUE))]
  for (col in integer_cols) {
    if (is_unique(keys)) break
    keys <- c(keys, col)
  }
  numeric_cols <- setdiff(numeric_cols, keys)
  unique_keys <- is_unique(keys)
  base <- dplyr::mutate(base, .in = TRUE)
  new <- dplyr::mutate(new, .in = TRUE)
  if (unique_keys) {
    joined <- dplyr::full_join(base, new, by = keys, suffix = c(".base", ".new"), na_matches = "na")
  } else {
    if (nrow(base) != nrow(new)) {
      return(tibble::tibble(identical = FALSE, header = header, rows_base = nrow(base), rows_new = nrow(new),
                            only_base = NA_integer_, only_new = NA_integer_, na_changed = NA_integer_,
                            max_abs_rel = NA_real_))
    }
    joined <- dplyr::bind_cols(
      dplyr::rename_with(base, \(x) paste0(x, ".base")),
      dplyr::rename_with(new, \(x) paste0(x, ".new"))
    )
  }
  in_both <- !is.na(joined$.in.base) & !is.na(joined$.in.new)

  values <- purrr::map(numeric_cols, \(col) {
    list(base = joined[[paste0(col, ".base")]][in_both], new = joined[[paste0(col, ".new")]][in_both])
  })
  na_changed <- sum(purrr::map_int(values, \(v) sum(is.na(v$base) != is.na(v$new))))
  rel <- unlist(purrr::map(values, \(v) dplyr::if_else(v$base == v$new, 0, abs(v$new - v$base) / abs(v$base))))

  tibble::tibble(
    identical = FALSE,
    header = header,
    rows_base = nrow(base),
    rows_new = nrow(new),
    only_base = sum(is.na(joined$.in.new)),
    only_new = sum(is.na(joined$.in.base)),
    na_changed = na_changed,
    max_abs_rel = if (any(is.finite(rel))) max(rel[is.finite(rel)]) else NA_real_
  )
}

compare_outputs <- function(dir_base, dir_new, files = NULL) {
  files_base <- list.files(dir_base, "\\.csv$")
  files_new <- list.files(dir_new, "\\.csv$")
  files <- files %||% union(files_base, files_new)

  result <-
    purrr::map(files, \(file) {
      if (!(file %in% files_base) || !(file %in% files_new)) {
        return(tibble::tibble(file = file, identical = FALSE, header = NA))
      }
      dplyr::bind_cols(tibble::tibble(file = file),
                       compare_output_file(file.path(dir_base, file), file.path(dir_new, file)))
    }) |>
    purrr::list_rbind()

  print(result, width = Inf)
  invisible(result)
}
