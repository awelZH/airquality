# Health outcomes. The topic itself is still work in progress (scripts/_compile_outcomes.R); this file
# holds the parts already reworked.


#' Drop years whose mortality data are not complete yet
#'
#' The mortality of the current year is delivered piece by piece, so its deaths are far below a full
#' year and the estimated premature deaths of that year would be far too low. A year counts as complete
#' when its deaths reach `min_share` of the median year.
#'
#' @param data Deaths per year, e.g. one row per year with the number of deaths.
#' @param min_share Share of the median year a year needs to count as complete.
#' @param year,count Columns holding the year and the number of deaths.
#'
#' @return `data` without the incomplete years; the dropped years are reported.
#'
#' @keywords internal
drop_incomplete_years <- function(data, min_share, year = year, count = deaths) {

  data <- dplyr::mutate(data, .complete = {{ count }} >= min_share * stats::median({{ count }}))
  dropped <- dplyr::pull(dplyr::filter(data, !.data$.complete), {{ year }})

  if (length(dropped) > 0) {
    cli::cli_inform("Dropping {length(dropped)} year{?s} without complete mortality data: {.val {dropped}}.")
  }

  data |>
    dplyr::filter(.data$.complete) |>
    dplyr::select(-".complete")
}
