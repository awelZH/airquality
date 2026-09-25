# Plots of the health outcomes -> plots_outcomes, a plot catalog (needs report/plots/_plot_setup.R)

plots <- list()

data_outcomes <- airquality.methods::read_local_csv(file = ressources_plotting$outcomes$outcomes, locale = readr::locale(encoding = "UTF-8"))


# premature deaths in the Canton of Zurich due to air pollution, absolute and per 100'000 inhabitants
plots$outcomes$preliminary_deaths_abs <- plot_health_outcomes(data_outcomes, plot_parameters_outcomes, relative = FALSE, ylabels = label_big_mark, theme = theme_ts)
plots$outcomes$preliminary_deaths_rel <- plot_health_outcomes(data_outcomes, plot_parameters_outcomes, relative = TRUE, ylabels = label_big_mark, theme = theme_ts)

# years of life lost, absolute and per 100'000 inhabitants (subtitle: long-term mean per premature death)
plots$outcomes$life_years_lost_abs <-
  plot_health_outcomes(data_outcomes, plot_parameters_outcomes, outcome_type = "verlorene Lebensjahre", relative = FALSE, ylabels = label_big_mark, theme = theme_ts)
plots$outcomes$life_years_lost_rel <-
  plot_health_outcomes(data_outcomes, plot_parameters_outcomes, outcome_type = "verlorene Lebensjahre", relative = TRUE, ylabels = label_big_mark, theme = theme_ts)


# collect the plots in a catalog for the Quarto pages (airquality.methods::get_plot(plots_outcomes, "preliminary_deaths_abs", "NO2"))
# ---
plots_outcomes <-
  dplyr::bind_rows(
    airquality.methods::plot_catalog(plots$outcomes$preliminary_deaths_abs, "preliminary_deaths_abs", names_to = "parameter"),
    airquality.methods::plot_catalog(plots$outcomes$preliminary_deaths_rel, "preliminary_deaths_rel", names_to = "parameter"),
    airquality.methods::plot_catalog(plots$outcomes$life_years_lost_abs, "life_years_lost_abs", names_to = "parameter"),
    airquality.methods::plot_catalog(plots$outcomes$life_years_lost_rel, "life_years_lost_rel", names_to = "parameter")
  )
