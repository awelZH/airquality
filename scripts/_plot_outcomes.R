# Plots of the health outcomes -> plots_outcomes, a plot catalog (needs scripts/_plot_setup.R)

plots <- list()

data_outcomes <- airquality.methods::read_local_csv(file = ressources_plotting$outcomes$outcomes, locale = readr::locale(encoding = "UTF-8"))


# premature deaths in the Canton of Zurich due to air pollution, absolute and per 100'000 inhabitants
plots$outcomes$preliminary_deaths_abs <- plot_health_outcomes(data_outcomes, plot_parameters_outcomes, relative = FALSE, theme = theme_ts)
plots$outcomes$preliminary_deaths_rel <- plot_health_outcomes(data_outcomes, plot_parameters_outcomes, relative = TRUE, theme = theme_ts)

# years of life lost, absolute, per 100'000 inhabitants and per premature death
plots$outcomes$life_years_lost_abs <-
  plot_health_outcomes(data_outcomes, plot_parameters_outcomes, outcome_type = "verlorene Lebensjahre", relative = FALSE, theme = theme_ts)
plots$outcomes$life_years_lost_rel <-
  plot_health_outcomes(data_outcomes, plot_parameters_outcomes, outcome_type = "verlorene Lebensjahre", relative = TRUE, theme = theme_ts)
plots$outcomes$life_years_lost_per_death <- plot_life_years_per_death(data_outcomes, plot_parameters_outcomes, theme = theme_ts)


# collect the plots in a catalog for the Quarto pages (get_plot(plots_outcomes, "preliminary_deaths_abs", "NO2"))
# ---
plots_outcomes <-
  dplyr::bind_rows(
    plot_catalog(plots$outcomes$preliminary_deaths_abs, "preliminary_deaths_abs", names_to = "parameter"),
    plot_catalog(plots$outcomes$preliminary_deaths_rel, "preliminary_deaths_rel", names_to = "parameter"),
    plot_catalog(plots$outcomes$life_years_lost_abs, "life_years_lost_abs", names_to = "parameter"),
    plot_catalog(plots$outcomes$life_years_lost_rel, "life_years_lost_rel", names_to = "parameter"),
    plot_catalog(plots$outcomes$life_years_lost_per_death, "life_years_lost_per_death", names_to = "parameter")
  )
