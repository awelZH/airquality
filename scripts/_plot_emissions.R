# Plots of the air pollutant emissions -> plots_emissions (needs scripts/_plot_setup.R)

plots <- list()


# emission inventory of the Canton of Zurich per pollutant, subsector and year (absolute and relative)
data_emikat <- airquality.methods::read_local_csv(ressources_plotting$emissions$emikat, delim = ";", locale = readr::locale(encoding = "UTF-8"))
pollutants <- rlang::set_names(unique(data_emikat$pollutant))

plots$emissions$inventory_absolute <-
  purrr::map(pollutants, \(pollutant) {
    ggplot_emissions(data = dplyr::filter(data_emikat, pollutant == !!pollutant), theme = theme_ts,
                     sectors_last = plot_emissions_sectors_last[[pollutant]])
  })

plots$emissions$inventory_relative <-
  purrr::map(pollutants, \(pollutant) {
    ggplot_emissions(data = dplyr::filter(data_emikat, pollutant == !!pollutant), relative = TRUE, pos = "fill", theme = theme_ts,
                     sectors_last = plot_emissions_sectors_last[[pollutant]])
  })


# RSD NOx emissions per vehicle type, fuel type and Euro norm; per vehicle model year; per year of measurement
data_rsd_per_norm <- airquality.methods::read_local_csv(ressources_plotting$emissions$rsd_norm)
data_rsd_per_yearmodel <- airquality.methods::read_local_csv(ressources_plotting$emissions$rsd_yearmodel)
data_rsd_per_yearmeas <- airquality.methods::read_local_csv(ressources_plotting$emissions$rsd_yearmeas)

plots$emissions$rsd_norm$NOx <- plot_rsd_per_norm(data_rsd_per_norm, theme = theme_ts)
plots$emissions$rsd_yearmodel$NOx <- plot_rsd_per_yearmodel(data_rsd_per_yearmodel, theme = theme_ts)
plots$emissions$rsd_yearmeas$NOx <- plot_rsd_per_yearmeas(data_rsd_per_yearmeas, theme = theme_ts)


# collect plots in a tibble for use in *.qmd
# ---
plots_emissions <-
  dplyr::bind_rows(
    plotlist_to_tibble(plots$emissions$inventory_absolute, "emission", "inventory_absolute"),
    plotlist_to_tibble(plots$emissions$inventory_relative, "emission", "inventory_relative"),
    plotlist_to_tibble(plots$emissions$rsd_norm, "emission", "rsd_norm"),
    plotlist_to_tibble(plots$emissions$rsd_yearmodel, "emission", "rsd_yearmodel"),
    plotlist_to_tibble(plots$emissions$rsd_yearmeas, "emission", "rsd_yearmeas")
  )
