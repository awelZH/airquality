
#' Reads Swiss BFS life expectancy data ("Kohortensterbetafeln) from official api
#'
#' @param destination_path
#'
#' @export
read_bfs_life_expectancy_data <- function(destination_path = tempdir()){

  # get download url from BFS api
  url <- get_bfs_asset_url(bfs_nr = "px-x-0102020300_101")

  # download temp file from api
  temp <- tempfile(tmpdir = destination_path, fileext = ".px")
  on.exit(unlink(temp), add = TRUE)
  httr2::request(url) |>
    httr2::req_perform(path = temp)

  # read *.px
  data <- pxR::read.px(temp, encoding = "UTF-8")

  return(data)
}
