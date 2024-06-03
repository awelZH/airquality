



### -----------------------------------------------
### for plotting with ggplot
### -----------------------------------------------


### function to plot standard timeseries of yearly values
ggplot_timeseries <- function(data, mapping = ggplot2::aes(x = starttime, y = value, color = siteclass), lims = c(0,NA), titlelab = NULL, captionlab = NULL, pointshape = 19, pointsize = 2,
                              threshold = list(value = NA, color = "gray30", label = NULL, labelsize = 4, linetype = 2, linesize = 1), 
                              theme = ggplot2::theme_minimal()) {
  
  plot <- 
    ggplot2::ggplot(data, mapping = mapping) + 
    ggplot2::geom_point(size = pointsize, shape = pointshape) +
    ggplot2::scale_x_datetime(expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(limits = lims, expand = c(0.01,0.01)) +
    titlelab +
    captionlab +
    theme
  
  if (!is.na(sum(threshold$value))){
    text <- tibble::tibble(x = rep(min(data$starttime), length(threshold$value)), y = threshold$value, label = threshold$labels)
    plot <-
      plot + 
      ggplot2::geom_hline(yintercept = threshold$value, color = threshold$color, linetype = threshold$linetype, linewidth = threshold$linesize) +
      ggplot2::geom_text(data = text, mapping = ggplot2::aes(x = x, y = y, label = label), size = threshold$labelsize, 
                         hjust = 0, vjust = 0, nudge_y = pmax(0, 0.01 * max(lims), na.rm = TRUE), inherit.aes = FALSE)
  }
  
  return(plot)
}




### function to plot exposition distribution histogram
ggplot_expo_hist <- function(data, x, y, barwidth = 1, xlims = c(0,NA), xbreaks = waiver(), titlelab = NULL, captionlab = NULL, xlabel = NULL,
                             threshold = list(value = NA, label = NULL, labelsize = 4, linetype = 2, linesize = 1),
                             fill_scale = NULL, theme = ggplot2::theme_minimal()) {
  
  if (is.null(fill_scale)) {
    mapping <- ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y))
  } else {
    mapping <- ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y), fill = !!rlang::sym(x))
  }
  
  plot <-
    ggplot2::ggplot(data, mapping = mapping) +
    ggplot2::geom_bar(stat = "identity", color = NA, width = barwidth) +
    ggplot2::scale_x_continuous(limits = xlims, breaks = xbreaks, expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01), labels = function(x) format(x, big.mark = "'", scientific = FALSE)) +
    fill_scale +
    xlabel +
    titlelab +
    captionlab +
    theme +
    ggplot2::theme(axis.title.x = ggplot2::element_text()) 
  
  if (!is.na(sum(threshold$value))){
    text <- tibble::tibble(x = threshold$value, label = threshold$labels)
    plot <-
      plot +
      ggplot2::geom_vline(xintercept = threshold$value, color = threshold$color, linetype = threshold$linetype, linewidth = threshold$linesize) +
      ggplot2::geom_text(data = text, mapping = ggplot2::aes(x = x, y = 0, label = label), size = threshold$labelsize,
                         hjust = 0, vjust = 0, angle = 90, nudge_x = pmin(0, -0.01 * max(xlims), na.rm = TRUE), inherit.aes = FALSE)
  }
  
  return(plot)
}




### function to plot relative cumulative exposition distribution
ggplot_expo_cumulative <- function(data, x, y, linewidth = 1, xlims = c(0,NA), xbreaks = waiver(), titlelab = NULL, captionlab = NULL, xlabel = NULL,
                                   threshold = list(value = NA, label = NULL, labelsize = 4, linetype = 2, linesize = 1),
                                   theme = ggplot2::theme_minimal()) {
  
  plot <-
    ggplot2::ggplot(data, mapping = ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y))) +
    ggplot2::geom_line(linewidth = linewidth, color = "gray40") +
    ggplot2::scale_x_continuous(limits = xlims, breaks = xbreaks, expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(limits = c(0,1), expand = c(0.01,0.01), labels = scales::percent_format()) +
    xlabel +
    titlelab +
    captionlab +
    theme +
    ggplot2::theme(axis.title.x = ggplot2::element_text()) 
  
  if (!is.na(sum(threshold$value))){
    text <- tibble::tibble(x = threshold$value, label = threshold$labels)
    plot <-
      plot +
      ggplot2::geom_vline(xintercept = threshold$value, color = threshold$color, linetype = threshold$linetype, linewidth = threshold$linesize) +
      ggplot2::geom_text(data = text, mapping = ggplot2::aes(x = x, y = 0, label = label), size = threshold$labelsize,
                         hjust = 0, vjust = 0, angle = 90, nudge_x = pmin(0, -0.01 * max(xlims), na.rm = TRUE), inherit.aes = FALSE)
  }
  
  return(plot)
}




### function to ggplot emissions employing structured coloring... this is not ideal, but the best I can do
ggplot_emissions <- function(data, cols, pos = "stack", width = 0.8, theme = ggplot2::theme_minimal()) {
  
  groups <-
    data %>% 
    dplyr::group_by(pollutant, unit, sector, subsector) %>% 
    dplyr::summarise(emission = mean(emission)) %>% 
    dplyr::group_by(pollutant, unit, sector) %>% 
    dplyr::mutate(
      subsector = dplyr::case_when(
        emission < min(sort(emission, decreasing = TRUE)[1:3]) ~ "sonstige",
        TRUE ~ subsector
      )
    ) %>%
    dplyr::group_by(pollutant, unit, sector, subsector) %>% 
    dplyr::summarise(emission = sum(emission)) %>% 
    dplyr::group_by(pollutant, unit) %>% 
    dplyr::mutate(
      subsector = dplyr::case_when(
        subsector != "weitere" & emission < 0.02 * sum(emission)  ~ "sonstige",
        TRUE ~ subsector
      )
    ) %>%
    dplyr::group_by(pollutant, unit, sector, subsector) %>% 
    dplyr::summarise(emission = sum(emission)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(others = stringr::str_detect(subsector, "sonstige")) %>% 
    dplyr::arrange(sector, dplyr::desc(others), emission) %>% 
    dplyr::mutate(subsector = paste0(sector, " / ", subsector)) %>% 
    dplyr::mutate(rootcol = dplyr::recode(sector, !!!cols)) %>% 
    dplyr::group_by(sector) %>% 
    dplyr::mutate(col = colorRampPalette(c(unique(rootcol), shades::brightness(unique(rootcol), 0.6)))(length(subsector))) %>% 
    dplyr::ungroup()
  
  plot <- 
    data %>% 
    dplyr::mutate(
      subsector = paste0(sector, " / ", subsector),
      subsector = dplyr::case_when(
        !(subsector %in% groups$subsector) ~ paste0(sector," / sonstige"),
        TRUE ~ subsector
      ),
      subsector = factor(subsector, levels = groups$subsector)
    ) %>% 
    dplyr::group_by(year, pollutant, unit, sector, subsector) %>% 
    dplyr::summarise(emission = sum(emission)) %>% 
    dplyr::ungroup() %>% 
    ggplot2::ggplot(aes(x = factor(year), y = emission, fill = subsector)) +
    ggplot2::geom_bar(stat = "identity", position = pos, width = width) + 
    ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = "'"), expand = c(0.01,0.01)) +
    ggplot2::scale_fill_manual(values = setNames(groups$col, groups$subsector)) +
    theme +
    ggplot2::theme(legend.title = ggplot2::element_blank())
  
  return(plot)
}

