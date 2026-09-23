# Setup for plotting: packages, local functions, analysis settings and presentation settings.
# Self-contained, so the plots can be built in the console as well as in the Quarto report (docs/*.qmd).
# Run from the project root, then source one of the topic scripts, e.g.
#   source("scripts/_plot_setup.R", encoding = "UTF-8")
#   source("scripts/_plot_exposition.R", encoding = "UTF-8") # -> plots_exposition
#   get_plot(plots_exposition, "population_weighted_mean", "NO2")
# ---

# the plot code uses prefixes; airquality.methods is attached only for prepare_emission_trends() (trend data,
# work in progress), which calls longpollutant() without prefix
suppressPackageStartupMessages(library(airquality.methods))

# local functions
devtools::load_all(quiet = TRUE)

# analysis settings (plot_years, plot_n_years, plot_parameters_*, plot_reference_year_emissions, crs, ...)
source("scripts/_settings.R", encoding = "UTF-8")

# ressource table for input datasets (LRV & WHO threshold values)
ressources <- prepare_ressources(airquality.methods::read_local_csv("inst/extdata/meta/ressources.csv", show_col_types = FALSE))


# list of output data sources for plotting
ressources_plotting <-
  list(
    emissions = list(
      emikat = "inst/extdata/output/data_emissions.csv",
      rsd_norm = "inst/extdata/output/data_nox_vehicle_emissions_rsd_per_norm.csv",
      rsd_yearmodel = "inst/extdata/output/data_nox_emissions_rsd_per_yearmodel.csv",
      rsd_yearmeas = "inst/extdata/output/data_nox_emissions_rsd_per_yearmeas.csv"
    ),
    monitoring = list(
      airquality = "inst/extdata/output/data_airquality_monitoring_y1.csv",
      ndep_pars = "inst/extdata/output/data_ndep_pars_monitoring_y1.csv",
      ndep = "inst/extdata/output/data_ndep_monitoring_y1.csv"
    ),
    trends = list(
      trends = "inst/extdata/output/data_airquality_trends_relative_y1.csv",
      trends_agg = "inst/extdata/output/data_airquality_trends_relative_aggregated_y1.csv"
    ),
    exposition = list(
      weightedmean_canton = "inst/extdata/output/data_exposition_weighted_means_canton.csv",
      weightedmean_municip = "inst/extdata/output/data_exposition_weighted_means_municipalities.csv",
      expo_distr_pollutants = "inst/extdata/output/data_exposition_distribution_pollutants.csv",
      expo_distr_ndep ="inst/extdata/output/data_exposition_distribution_ndep.csv"
    ),
    outcomes = list(
      outcomes = "inst/extdata/output/data_health_outcomes.csv"
    )
  )


# data subsetting parameters: see scripts/_settings.R (plot_years, plot_n_years, plot_parameters_timeseries,
# plot_parameters_exposition, plot_reference_year_emissions)
siteclass_levels <- rev(c("ländlich - Hintergrund", "klein-/vorstädtisch - Hintergrund",
                          "städtisch - Hintergrund", "städtisch - verkehrsbelastet"))

# emissions: sectors moved to the end of the stack and the legend, per pollutant (NH3: agriculture last,
# to show its influence over time)
plot_emissions_sectors_last <- list(NH3 = "Land- und Forstw.")


# plotting size parameters
basesize <- 12 # ggplot theme base_size
pointsize <- 2 # size of point markers
linewidth <- 1 # width of lines
jitter_seed <- 1 # seed of jittered points, so figures stay the same between renders

# axes per parameter: monitoring time series (y limits and breaks) and exposition plots (bar width, x breaks;
# their range is the x range); a parameter without entry stops the plot with an error
plot_axes_timeseries <- list(
  NO2 = list(ylim = c(0,70), ybreaks = seq(0,70,10)),
  PM10 = list(ylim = c(0,35), ybreaks = seq(0,35,5)),
  PM2.5 = list(ylim = c(0,20), ybreaks = seq(0,20,4)),
  eBC = list(ylim = c(0,4), ybreaks = seq(0,4,0.5)),
  O3_max_98p_m1 = list(ylim = c(0,210), ybreaks = seq(0,210,30)),
  O3_peakseason_mean_d1_max_mean_h8gl = list(ylim = c(0,130), ybreaks = seq(0,120,20))
)
bar_scale <- 0.9 # share of the class width covered by a bar
plot_axes_exposition <- list(
  NO2 = list(barwidth = 1 * bar_scale, xbreaks = seq(0,55,5)),
  O3_max_98p_m1 = list(barwidth = 2 * bar_scale, xbreaks = seq(0,180,20)),
  O3_peakseason_mean_d1_max_mean_h8gl = list(barwidth = 2 * bar_scale, xbreaks = seq(0,120,10)),
  PM10 = list(barwidth = 0.5 * bar_scale, xbreaks = seq(0,24,2)),
  PM2.5 = list(barwidth = 0.5 * bar_scale, xbreaks = seq(0,18.5,1)),
  eBC = list(barwidth = 0.05 * bar_scale, xbreaks = seq(0,2.2,0.2)),
  Ndep = list(barwidth = 1 * bar_scale, xbreaks = seq(-5,45,5))
)


# read LRV legal threshold limit values & WHO air quality guideline values
immission_threshold_values <- readr::read_delim(filter_ressources(ressources, 10), delim = ";",locale = readr::locale(encoding = "UTF-8"))


# add plotting parameter to LRV threshold limit values & WHO air quality guideline values
col_lrv <- "red3" # color of LRV threshold value
col_who <- "gray30" # color of WHO guideline threshold value
lty_lrv <- 1 # line type of LRV threshold value
lty_who <- 2 # line type WHO guideline threshold value
lsz_lrv <- 1 # line width of LRV threshold value
lsz_who <- 1 # line width of WHO guideline threshold value
lbsz <- 4 # label size of threshold value line text
immission_threshold_values <-
  tibble::tibble(
    source = c("LRV Grenzwert", "WHO Richtwert"),
    col = c(col_lrv, col_who),
    lty = c(lty_lrv, lty_who),
    lsz = c(lsz_lrv, lsz_who),
    lbsz = lbsz
  ) |> 
  dplyr::right_join(immission_threshold_values, by = "source")

threshold_ndep <- extract_threshold(dplyr::filter(immission_threshold_values, source == "LRV Grenzwert"), "NO2")
threshold_ndep$value <- 0
threshold_ndep$labels <- "krit. Eintragsrate"


# colors and color scales
scale_fill_siteclass <- 
  ggplot2::scale_fill_manual(name = "Standortklasse", values = c(
    "ländlich - Hintergrund" = scales::viridis_pal(option = "D", begin = 0.2, end = 0.97)(4)[4],
    "klein-/vorstädtisch - Hintergrund" = scales::viridis_pal(option = "D", begin = 0.2, end = 0.97)(4)[3],
    "städtisch - Hintergrund" = scales::viridis_pal(option = "D", begin = 0.2, end = 0.97)(4)[2],
    "städtisch - verkehrsbelastet" = scales::viridis_pal(option = "D", begin = 0.2, end = 0.97)(4)[1],
    "empf. Ökosystem" = "gray20"
  ))

scale_color_siteclass <- 
  ggplot2::scale_color_manual(name = "Standortklasse", na.value = "gray60", values = c(
    "ländlich - Hintergrund" = scales::viridis_pal(option = "D", begin = 0.2, end = 0.97)(4)[4],
    "klein-/vorstädtisch - Hintergrund" = scales::viridis_pal(option = "D", begin = 0.2, end = 0.97)(4)[3],
    "städtisch - Hintergrund" = scales::viridis_pal(option = "D", begin = 0.2, end = 0.97)(4)[2],
    "städtisch - verkehrsbelastet" = scales::viridis_pal(option = "D", begin = 0.2, end = 0.97)(4)[1],
    "empf. Ökosystem" = "gray20"
  ))

cols_ecosys <- setNames(c("steelblue", colorspace::sequential_hcl(n = 3, palette = "ag_GrnYl"), "gray80", "gray10"), c("Hochmoor", "Flachmoor", "Trockenrasen", "Wald", "kein empf. Ökosys.", "Median aller Standorte"))
scale_color_ecosys <- ggplot2::scale_color_manual(name = "Ökosystem", values = cols_ecosys)
scale_fill_ecosys <- ggplot2::scale_fill_manual(name = "Ökosystem", values = cols_ecosys, guide = "none")

shapes_estimated <- setNames(c(21,23,25), c("<5 kg-N", "5-12 kg-N", ">12 kg-N"))
scale_shape_estimated <- ggplot2::scale_shape_manual(name = "geschätzt", values = shapes_estimated)

# ggplot2 custom themes
theme_ts <-
  ggplot2::theme_minimal(base_size = basesize, base_family = "Arial") +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = ggplot2::rel(1)),
    plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.8)),
    plot.caption = ggplot2::element_text(hjust = 1, color = "gray40", size = ggplot2::rel(0.66)),
    plot.background = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_line(color = "gray30"),
    axis.ticks = ggplot2::element_line(color = "gray30"),
    axis.title = ggplot2::element_blank()
  )

theme_map <-
  ggplot2::theme_void(base_size = basesize, base_family = "Arial") +
  ggplot2::theme(
    plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.8)),
    plot.caption = ggplot2::element_text(hjust = 1, color = "gray40", size = ggplot2::rel(0.75)),
    panel.background = ggplot2::element_blank(),
    plot.background = ggplot2::element_blank()
  )
