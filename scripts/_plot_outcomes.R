# Plots of the health outcomes -> plots_outcomes, a plot catalog (needs scripts/_plot_setup.R)

plots <- list()


# premature deaths in the Canton of Zurich due to air pollution, absolute and per 100'000 inhabitants
data_outcomes <- airquality.methods::read_local_csv(file = ressources_plotting$outcomes$outcomes, locale = readr::locale(encoding = "UTF-8"))

plots$outcomes$preliminary_deaths_abs <- plot_premature_deaths(data_outcomes, plot_parameters_outcomes, relative = FALSE, theme = theme_ts)
plots$outcomes$preliminary_deaths_rel <- plot_premature_deaths(data_outcomes, plot_parameters_outcomes, relative = TRUE, theme = theme_ts)

# years of life lost
# TODO ...


# collect the plots in a catalog for the Quarto pages (get_plot(plots_outcomes, "preliminary_deaths_abs", "NO2"))
# ---
plots_outcomes <-
  dplyr::bind_rows(
    plot_catalog(plots$outcomes$preliminary_deaths_abs, "preliminary_deaths_abs", names_to = "parameter"),
    plot_catalog(plots$outcomes$preliminary_deaths_rel, "preliminary_deaths_rel", names_to = "parameter")
  )
