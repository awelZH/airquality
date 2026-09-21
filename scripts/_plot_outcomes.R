# Plots of the health outcomes -> plots_outcomes (needs scripts/_plot_setup.R)

plots <- list()


# premature deaths in the Canton of Zurich due to air pollution, absolute and per 100'000 inhabitants
data_outcomes <- airquality.methods::read_local_csv(file = ressources_plotting$outcomes$outcomes, locale = readr::locale(encoding = "UTF-8"))

plots$outcomes$preliminary_deaths_abs <- plot_pars_prelim_deaths_timeseries(data_outcomes, plot_parameters_outcomes, relative = FALSE, theme = theme_ts)
plots$outcomes$preliminary_deaths_rel <- plot_pars_prelim_deaths_timeseries(data_outcomes, plot_parameters_outcomes, relative = TRUE, theme = theme_ts)

# years of life lost
# TODO ...


# collect plots in a tibble for use in *.qmd
# ---
plots_outcomes <-
  dplyr::bind_rows(
    plotlist_to_tibble(plots$outcomes$preliminary_deaths_abs, "outcomes", "preliminary_deaths_abs"),
    plotlist_to_tibble(plots$outcomes$preliminary_deaths_rel, "outcomes", "preliminary_deaths_rel")
  )
