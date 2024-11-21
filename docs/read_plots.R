# library(ggplot2)
# library(dplyr)
# library(rlang)
# library(tibble)

#' Extracts plot from tibble created by plotlist_to_tibble()
#'
#' @param plots_df 
#' @param filter 
#'
#' @keywords internal
get_plot <- function(plots_df, filter_expr = "pollutant == 'NOx' & source == 'inventory_absolute'") {
  
  plot <- 
    plots_df |> 
    dplyr::filter(!!rlang::parse_expr(filter_expr)) |> 
    dplyr::pull(plot)
  
  return(plot[[1]])
}

# plots <- readRDS("plots.rds")

# unique(plots$type)
# unique(plots$source)
# unique(plots$pollutant)
# unique(plots$year)

# get_plot(plots, "pollutant == 'PM2.5' & type == 'monitoring'")
