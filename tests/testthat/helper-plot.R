# Helpers for the plot tests: read texts and layers of a built ggplot without a visible device.

# all text labels drawn in a grob tree (legend titles, key labels, ...)
grob_texts <- function(grob) {
  texts <- if (inherits(grob, "text") || inherits(grob, "titleGrob")) as.character(grob$label) else character()
  children <- c(if (inherits(grob, "gTree")) grob$children else list(), if (inherits(grob, "gtable")) grob$grobs else list())
  c(texts, unlist(purrr::map(children, grob_texts)))
}

legend_texts <- function(plot) {
  gt <- withr::with_pdf(NULL, ggplot2::ggplotGrob(plot))
  boxes <- gt$grobs[grepl("^guide-box", gt$layout$name)]
  unlist(purrr::map(boxes, grob_texts))
}

# the computed data of every layer, as ggplot_build() returns them
layer_data_all <- function(plot) {
  withr::with_pdf(NULL, ggplot2::ggplot_build(plot))$data
}

# threshold values as scripts/_plot_setup.R prepares them (thresholds plus line styles)
make_threshold_values <- function() {
  tibble::tibble(
    source = rep(c("LRV Grenzwert", "WHO Richtwert"), each = 4),
    col = rep(c("red3", "gray30"), each = 4),
    lty = rep(c(1, 2), each = 4),
    lsz = 1,
    lbsz = 4,
    pollutant = rep(c("NO2", "PM10", "O3", "O3"), 2),
    metric = "Jahresmittel",
    metric_description = rep(c("Jahresmittel", "Jahresmittel", "typische Spitzenbelastung", "mittlere Sommertagbelastung"), 2),
    parameter = rep(c("NO2", "PM10", "O3_max_98p_m1", "O3_peakseason_mean_d1_max_mean_h8gl"), 2),
    interval = "y1",
    threshold = c(30, 20, 100, NA, 10, 15, NA, 60),
    unit = "µg/m3"
  ) |>
    dplyr::filter(!is.na(threshold))
}
