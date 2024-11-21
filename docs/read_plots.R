# library(ggplot2)
# library(dplyr)
# library(rlang)
# library(tibble)
# library(glue)

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

#' Construct character code chunks to automate plotting by years as tabset-panels
#'
#' @param is
#' @param year 
#' @param pollutant 
#' @param source 
#' @param type 
#'
#' @description
#' apapted, from here: https://www.andrewheiss.com/blog/2024/11/04/render-generated-r-chunks-quarto/
#' Heiss, Andrew. 2024. “Guide to Generating and Rendering Computational Markdown Content Programmatically with Quarto.” November 4, 2024. https://doi.org/10.59350/pa44j-cc302.
#' 
#' @keywords internal
build_panel <- function(id, year, pollutant, source, type = "exposition") {
  
  source <- stringr::str_replace(source, "_", "-")
  chunk_label <- glue::glue("{tolower(type)}-{tolower(pollutant)}-{tolower(source)}-{year}")
  
  output <- 
    glue::glue(
      "##### <<year>>
      ```{r}
      #| label: <<chunk_label>>
      plots$plot[[<<id>>]]
      ```", .open = "<<", .close = ">>"
    )
  
  return(output)
}

# plots <- readRDS("plots_exposition.rds")

# unique(plots$type)
# unique(plots$source)
# unique(plots$pollutant)
# unique(plots$year)

# get_plot(plots, "pollutant == 'PM2.5' & type == 'exposition' & source == 'distribution_cumulative'")




