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

**Refactoring of the premature deaths** (2026-09-24, step 3a of `plan_outcomes.md`): functions in
`R/outcomes.R` (`prepare_mortality()`, `prepare_population_by_age()`, `deaths_per_year()`,
`estimate_premature_deaths()`, `outcome_scenarios()`), 44-line script, population by age as resource 28.
Old behaviour kept on purpose; the deaths per year (input of healthiar) are identical to the old code
for 2010–2025 (e.g. 2019: 10'597). No byte identity: the old code draws the range by Monte Carlo without
seed; two old runs differ by up to 8 % (central), 129 % (lower bound, small values) and 6 % (upper), the
refactored run vs. the old one by 4.7 %, 73 % and 9.5 %. Findings in the old behaviour, corrected in E1:
category 290 (deaths aged 0–29, 94–137 per year) spread over the suppressed cells aged ≥ 30 (which can
hold at most 45–72); the deaths were summed over the population table, so ages with population but
without a death record counted 1 death each (1–6 per year, ages 30–38 or > 100; a former work-around
for `attribute_lifetable()`), and deaths at ages missing in the population table fell out (0–25 per
year). Together well below 1 % of about 10'000 deaths per year. Note when checking by hand: the mortality
file must be read as UTF-8, otherwise "männlich" does not match and the men drop out.

**E1: deaths from age 30 only** (user decision 2026-09-24, own commit): `prepare_mortality(suppressed =
outcomes_suppressed_deaths)` drops category 290 (deaths aged 0–29) and fills the suppressed cells with
2 deaths (midpoint of 1–3); `deaths_per_year()` sums the deaths from `outcomes_min_age` directly, without
the population table. Effect on the deaths per year 2010–2024: −46 to −86 (−0.43 % to −0.87 %), of which
dropping category 290 −50 to −101 and summing without the population join −4 to +21 (the fictitious
deaths and the dropped high ages). The premature deaths are proportional to the deaths, so they drop by
the same share. The population by age is no longer read for the deaths; it returns with the life table.
