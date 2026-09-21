# Plots of the health outcomes -> plots_outcomes (needs scripts/_plot_setup.R)

plots <- list()


# plotting selected health-outcomes due to population exposition by air pollutants
# ---
data_outcomes <- airquality.methods::read_local_csv(file = ressources_plotting$outcomes$outcomes, locale = readr::locale(encoding = "UTF-8"))

# plotting timeseries of preliminary deaths for Canton Zürich
plots$outcomes$preliminary_deaths_abs <- plot_pars_prelim_deaths_timeseries(data_outcomes, c("PM2.5", "NO2", "O3_peakseason_mean_d1_max_mean_h8gl"), relative = FALSE)

# plotting timeseries of preliminary deaths per 100'000 inhabitants for Canton Zürich
plots$outcomes$preliminary_deaths_rel <- plot_pars_prelim_deaths_timeseries(data_outcomes, c("PM2.5", "NO2", "O3_peakseason_mean_d1_max_mean_h8gl"), relative = TRUE)

# plotting timeseries of years of life lost for Canton Zürich
# TODO ...




# collect plots in a tibble for use in *.qmd
# ---
plots_outcomes <-
  dplyr::bind_rows(
    plotlist_to_tibble(plots$outcomes$preliminary_deaths_abs, "outcomes", "preliminary_deaths_abs"),
    plotlist_to_tibble(plots$outcomes$preliminary_deaths_rel, "outcomes", "preliminary_deaths_rel")
  )
