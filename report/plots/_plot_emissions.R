# Plots of the air pollutant emissions -> plots_emissions, a plot catalog (needs report/plots/_plot_setup.R)

plots <- list()


# emission inventory of the Canton of Zurich per pollutant, subsector and year (absolute and relative)
data_emikat <- airquality.methods::read_local_csv(ressources_plotting$emissions$emikat, delim = ";", locale = readr::locale(encoding = "UTF-8"))
pollutants <- rlang::set_names(unique(data_emikat$pollutant))

plots$emissions$inventory_absolute <-
  purrr::map(pollutants, \(pollutant) {
    plot_emission_inventory(data = dplyr::filter(data_emikat, pollutant == !!pollutant), theme = theme_ts,
                            sectors_last = plot_emissions_sectors_last[[pollutant]])
  })

plots$emissions$inventory_relative <-
  purrr::map(pollutants, \(pollutant) {
    plot_emission_inventory(data = dplyr::filter(data_emikat, pollutant == !!pollutant), relative = TRUE, position = "fill", theme = theme_ts,
                            sectors_last = plot_emissions_sectors_last[[pollutant]])
  })


# RSD NOx emissions per vehicle type, fuel type and Euro norm; per vehicle model year; per year of measurement
data_rsd_per_norm <- airquality.methods::read_local_csv(ressources_plotting$emissions$rsd_norm)
data_rsd_per_yearmodel <- airquality.methods::read_local_csv(ressources_plotting$emissions$rsd_yearmodel)
data_rsd_per_yearmeas <- airquality.methods::read_local_csv(ressources_plotting$emissions$rsd_yearmeas)

plots$emissions$rsd_norm$NOx <- plot_rsd_per_norm(data_rsd_per_norm, theme = theme_ts)
plots$emissions$rsd_yearmodel$NOx <- plot_rsd_per_yearmodel(data_rsd_per_yearmodel, theme = theme_ts)
plots$emissions$rsd_yearmeas$NOx <- plot_rsd_per_yearmeas(data_rsd_per_yearmeas, theme = theme_ts)


# collect the plots in a catalog for the Quarto pages (get_plot(plots_emissions, "inventory_absolute", "NOx"))
# ---
plots_emissions <-
  dplyr::bind_rows(
    plot_catalog(plots$emissions$inventory_absolute, "inventory_absolute", names_to = "parameter"),
    plot_catalog(plots$emissions$inventory_relative, "inventory_relative", names_to = "parameter"),
    plot_catalog(plots$emissions$rsd_norm$NOx, "rsd_norm"),
    plot_catalog(plots$emissions$rsd_yearmodel$NOx, "rsd_yearmodel"),
    plot_catalog(plots$emissions$rsd_yearmeas$NOx, "rsd_yearmeas")
  )
