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

**Rerun with the updated mortality data** (`data/tod_nat_gatu.csv`, 2026-09-22 by the user):
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

**E2: deterministic estimate and range** (user decision 2026-09-24, own commit): `estimate_premature_deaths()`
takes `health_main` of `healthiar::attribute_health()` – the estimate with the central relative risk,
the range with its lower and upper 95 % bound – instead of the Monte-Carlo median and quantiles of
`summarize_uncertainty()` (500 simulations, no seed). The relative risk is the only uncertain input, so
the Monte Carlo only approximated this range. Two runs are now byte-identical. Difference to the
Monte-Carlo run after E1: central values up to 4.3 % (e.g. O3 2024: 350 → 365), upper bounds up to 6 %,
lower bounds up to 83 % where they are small (O3, lower relative risk 1.002: 2024 31 → 57; 2019 120 → 70).

**Years of life lost** (2026-09-24, step 4): `lifetable_data()` builds the life tables per year, sex and
single age from 30 (deaths from `prepare_mortality()`, mid-year population = mean of the year-ends of the
previous and the same year, 2010 its year-end; ages from `outcomes_lifetable_max_age` = 100 condensed;
ages without deaths get 0, healthiar warns about it and the warning is muffled). `estimate_life_years_lost()`
calls `healthiar::attribute_lifetable(health_outcome = "yll", approach_exposure = "single_year",
approach_newborns = "without_newborns")` for the actual and the base-year exposure, deterministic like the
deaths (range from the RR bounds), sexes summed. New rows `outcome_type = "verlorene Lebensjahre"` in
`data_health_outcomes.csv` (180 instead of 90 rows, same 11 columns); the premature deaths unchanged.
Run time about 1 minute (90 life-table calls of 0.75 s). Results (actual exposure): PM2.5 2010 11'179,
2019 5'186, 2024 2'615 years; NO2 2024 1'182; O3 2024 3'813. Years per premature death 10.3–11.4,
slowly falling (PM2.5 2011 11.35, 2024 10.40). Reduction of life expectancy not implemented (only with an
official life table); a period life table built from the same data gave for PM2.5 2019 e30 −4.5 months
(women) and −5.0 months (men).

**Plots and page** (2026-09-24, step 5): `plot_premature_deaths()` became `plot_health_outcomes(outcome_type = )`
(labels per outcome type in `outcome_labels`, error class `airquality_plot_error` for others); the six
premature-death figures stay byte-identical (`run_plots()`). New `plot_life_years_per_death()` (years of
life lost / premature deaths of the same year and parameter, actual exposure, central values, no range,
no scenario legend). Catalog entries `life_years_lost_abs`, `_rel`, `_per_death`. `Gesundheitsfolgen.qmd`:
per pollutant "vorzeitige Todesfälle" (tabset absolut/relativ) and "verlorene Lebensjahre" (tabset
absolut/relativ/pro Todesfall); "Grundlagen": the range is the CRF 95 % CI only (the minimum-concentration
threshold, E5 b, is mentioned as not included, results a lower bound), the life-table method and the
ratio per death explained.

**Clean-up** (step 6): removed the dead outcome code `prepare_preliminary_deaths()`, `calc_outcome()`,
`calculate_all_outcomes()`, `get_base_scenario_year()`, `prepare_life_expectancy_data()`,
`read_bfs_life_expectancy_data()`, `get_bfs_asset_url()` (files `prepare_helpers.R`, `read.R`,
`read_helpers.R` deleted) and `pxR` from DESCRIPTION and `_setup.R` (still in `renv.lock`; a
`renv::snapshot()` would drop it). `prepare.R` and `aggregate.R` now hold trend code only.

**Page follow-ups** (user decisions 2026-09-24): the minimum-concentration threshold scenario stays out
(E5 b confirmed after quantifying it: O3 never, PM2.5 only 2014/2023/2024 slightly, NO2 strongly affected,
e.g. 2024 +282 on 114 deaths). The separate plots of the years lost per premature death are gone; instead
the subtitle of the years-of-life-lost plots gives in a second line the long-term mean of the yearly ratio
(`mean_life_years_per_death()`, rounded to whole years: PM2.5 10.72, NO2 10.64, O3 10.62 → 11), and their title reads
"Verlorene Lebensjahre in der Bevölkerung durch …". All outcome plots label the y axis with `'` as
thousands separator (`label_big_mark()` in `R/plot.R`, argument `ylabels` of `ggplot_timeseries_bars()`,
default unchanged for the other pages). "! WORK-IN-PROGRESS !" removed from the page. Figures: 354 of the
other pages byte-identical; changed only the 6 life-years plots and the premature deaths PM2.5 absolute
(the only one with values ≥ 1'000).

**Outcomes in the pipeline** (phase 2b, 2026-09-24): `pipelines/outcomes.R` takes the population-weighted
means of the canton from the target `expo_pop_means_canton` instead of reading
`data_exposition_weighted_means_canton.csv` back. On frozen inputs `data_health_outcomes.csv` differs from
the old script by at most 4.5e-11 (relative): the CSV round trip lost the last digits. `recode_sex()` uses
`dplyr::case_when()` (`case_match()` is deprecated in dplyr 1.2).
