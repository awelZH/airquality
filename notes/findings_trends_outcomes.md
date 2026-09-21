# Trends and outcomes: findings (not yet reworked)

Read before working on trends or outcomes.

The random forest meteo-normalisation has no seed, so `type == "Trend"` values vary between runs
(median 0.3 pp, max 2.5 pp of relative immission) – set a seed in phase 2a. A full project run takes
≈ 30 min for the trends and a few minutes for the rest. The YLL part of `_compile_outcomes.R` is
unfinished work (`year <- 2018`).

**Plots reworked** (2026-09-21, the data scripts stay WIP): `R/plot_outcomes.R`, `R/plot_trends.R`, thin
`_plot_outcomes.R`/`_plot_trends.R`, tests `test-plot-outcomes.R`/`test-plot-trends.R`; the 9 figures
byte-identical. Details in `findings_plots.md`. Open for the data rework: `prepare_emission_trends()`
calls `longpollutant()` without prefix (the only reason `_plot_setup.R` still attaches
`airquality.methods`); the detailed trend plot keeps trends per site up to `lubridate::year(Sys.Date())`,
not `year_last` (decision 7).
