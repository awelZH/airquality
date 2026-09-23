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

**Uncertainty only for the actual exposure** (user decision 2026-09-23): `plot_premature_deaths()` drew
the uncertainty range over both scenarios, also over the avoided deaths, where it has no meaning;
`uncertainty_scenario = "tatsächliche Belastung"` now selects the data of that layer.

**Rerun with the updated mortality data** (`inst/extdata/tod_nat_gatu.csv`, 2026-09-22 by the user):
2024 was almost empty before and now holds a full year (PM2.5: 3.6 → 250 premature deaths). The years
2010–2023 move by up to 4 % as well, although their mortality data did not change: the estimate draws
`nsim = 500` simulations without a seed, so every run differs slightly. 2025 stays at about 5 deaths
because the mortality data of the current year are missing – worth deciding whether the report should
show that year at all (see the year range of the outcomes).

**Completeness check per year** (user decision 2026-09-23): `drop_incomplete_years()` (new `R/outcomes.R`,
the first reworked piece of this topic) drops years whose deaths do not reach `outcomes_min_year_share`
(0.8, `_settings.R`) of the median year, measured against the median so that one exceptional year does
not disqualify the others. The mortality data of the current year arrive piece by piece, so 2025 fell
out of the outcomes (2010–2024 remain, 90 instead of 96 rows).
