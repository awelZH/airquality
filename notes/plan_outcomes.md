# Plan: health outcomes (premature deaths, years of life lost)

Read before working on the health outcomes. Approved 2026-09-24 (revised after the spike), **all steps done
2026-09-24** (commits 5b7e43e … the clean-up); findings go to
`findings_trends_outcomes.md`.

## Context

`scripts/_compile_outcomes.R` is still WIP (327 lines, functions defined inside the script, dead code,
hard-coded URL, wrong `rm()`). Premature deaths are estimated with `healthiar::attribute_health()` on
the deaths aged ≥ 30, but the bars show an unseeded Monte-Carlo median (runs differ by up to 4 %), and
`prepare_mortality()` adds the deaths aged 0–29 (category 290, 94–137 per year) to the suppressed cells
aged ≥ 30 (which can hold at most 45–72): the baseline is ≈ 1 % too high. Years of life lost (YLL) exist
only as a commented fragment (PM2.5, 2018). The page claims the uncertainty includes the
"minimum concentration" threshold, but `outcome_delta_min_conc` is always 0.
Goal: pure, tested functions in `R/outcomes.R` + thin script (pattern of emissions/monitoring),
reproducible results, YLL for PM2.5, NO2 and O3 peak season with healthiar, plots on the page like the
premature deaths.

healthiar: installed 0.2.6.1742 (15.09.2026) = current GitHub master, newer than CRAN 0.2.6
(11.09.2026); no update needed. Used features: `attribute_lifetable()` respects `max_age` and accepts
ages > 100 (drops the `common_maxage <- 99` hack), fractional deaths allowed (drops `frequency = 1`),
`main_results_by` if several cases are batched; `threshold` defaults to `cutoff` (ERF unchanged).

## User decisions (2026-09-24)

* E1 a: deaths aged 0–29 (`alterkat == 290`) are dropped; suppressed cells (NA, 1–3 deaths) get 2
  (≈ 30–48 deaths per year instead of 94–137 redistributed). Own content commit.
* E2 (revised after the spike): bar = deterministic point estimate (`health_main`, `erf_ci ==
  "central"`), range = deterministic `lower`/`upper` of `health_main` (impact at `rr_lower`/`rr_upper`,
  i.e. exactly the CRF 95 % CI); no Monte Carlo (`summarize_uncertainty()` not used, no `nsim`/seed
  settings). Spike PM2.5 2019: YLL 5'121 (2'681–7'545) vs. Monte Carlo 100 sims 4'923 (2'787–6'990);
  0.75 s per case instead of 14 s. Reproducible without seed.
* E3 a: YLL with `attribute_lifetable(health_outcome = "yll", approach_exposure = "single_year",
  approach_newborns = "without_newborns", min_age = 30)`, per sex, one year's exposure.
* E4: mid-year population = mean of the year-end populations of x−1 and x; 2010 uses year-end 2010
  (noted); last age group condensed to 100+ (`outcomes_lifetable_max_age`). Verify that the population
  dataset is per 31.12.
* E5 b: minimum-concentration sensitivity prepared, not computed: `outcome_delta_min_conc` stays 0
  (contract column kept), page text adapted (range = CRF 95 % CI only).
* E6 a: premature deaths stay with `attribute_health()` on the summed deaths ≥ 30.
* Output: `data_health_outcomes.csv` keeps its 11 columns; YLL are new rows
  (`outcome_type = "verlorene Lebensjahre"`), same scenarios and uncertainty columns.
* YLL per premature death: additional tab "pro Todesfall" next to absolut/relativ under "verlorene
  Lebensjahre", computed in the plot from the CSV (YLL / premature deaths of the same year, parameter
  and scenario "tatsächliche Belastung"; point estimates only, no range: the ratio hardly depends on the
  CRF bound). No extra CSV rows. Spike PM2.5 2019: 5'121 / 494 ≈ 10.4 years per death.
* Reduction of life expectancy: **not implemented** (user decision: only with an official, citable life
  table). The spike value (PM2.5 2019: e30 −4.5 months women, −5.0 men) came from a period life table
  constructed by us from the same deaths and population; recorded in the findings only. Possible later
  source: BFS "Periodensterbetafeln für die Schweiz" (px-x-0102020300_102): national, not cantonal,
  from the BFS mortality model, edition 2023, irregular updates – to be decided by the user.

## Steps

0. **Document the plan** in `notes/plan_outcomes.md` (done; update it to this revision), linked in
   `CLAUDE.md`.
1. **Spike (done, no commit)**: see E2 and the life-expectancy item above. Ages 30–38 have a few
   true zero-death cells (1–5 over 15 years); healthiar warns but computes – kept.
2. **Regression reference**: add topic `outcomes` to `tests/regression/run_topic.R` (script, settings
   `ressources`, `outcomes_min_year_share`, …; the population download frozen via the existing
   `read_opendataswiss` record/replay). Reference run with the old code. The old results are unseeded,
   so the comparison checks the structure, the deterministic point estimates and the ranges within
   Monte-Carlo tolerance, not byte identity.
3. **Deaths, refactoring** (tests first, testthat 3e, synthetic data): in `R/outcomes.R`
   * `prepare_mortality()` (raw → `year`, `sex`, `age`, `deaths`; `check_columns()`, error class
     `airquality_input_error`), `prepare_population_by_age()` (municipalities summed, mid-year),
     `deaths_per_year()`, `drop_incomplete_years()` (exists),
   * `estimate_premature_deaths()` per parameter and year (scenarios actual / base year),
   * `outcome_scenarios()` shared by deaths and YLL: avoided = actual − base if negative, labels
     ("tatsächliche Belastung", "vermieden vs. <base_year>"), join of `pollutant`, `metric`,
     `population`, output schema incl. `outcome_delta_min_conc = 0`.
   Thin `scripts/_compile_outcomes.R`; settings `outcomes_min_age`, `outcomes_lifetable_max_age`,
   `outcomes_erf_shape` in `_settings.R`; population dataset as new entry in
   `data/meta/ressources.csv`. First commit keeps the old behaviour where possible; content
   changes as own commits: E1, then E2 (deterministic point estimate and range).
4. **YLL**: `lifetable_data()` (deaths + population per year, sex, age ≥ 30, 100+ condensed, deaths
   ≥ 0 fractional ok, population ≥ deaths checked), `estimate_life_years_lost()` per parameter and year
   (actual / base year, per sex summed), rows through `outcome_scenarios()`. Tests with a synthetic mini
   life table (e.g. checks: exposure at the cutoff → 0 YLL; more exposure → more YLL; sexes summed).
   Commit.
5. **Plots and page**: `plot_premature_deaths()` → `plot_health_outcomes(data, parameters,
   outcome_type, relative, …)` in `R/plot_outcomes.R` (titles/subtitles per outcome type, e.g.
   "Verlorene Lebensjahre durch …", "pro 100'000 Einwohner/innen"); catalog entries
   `life_years_lost_abs`/`_rel` in `scripts/_plot_outcomes.R`; `docs/Gesundheitsfolgen.qmd` per
   pollutant: `#### vorzeitige Todesfälle` (tabset absolut/relativ) and `#### verlorene Lebensjahre`
   (tabset absolut/relativ/pro Todesfall, catalog entry `life_years_lost_per_death`); "Grundlagen"
   text: YLL method added, the minimum-concentration sentence replaced (E5 b), the range described as
   the CRF 95 % CI. Plot regression: premature-death figures byte-identical to the state after step 3 except for
   the intended changes. Commit, then render commit.
6. **Clean-up and docs**: remove dead code (`prepare_preliminary_deaths()`, `calc_outcome()`,
   `calculate_all_outcomes()`, `get_base_scenario_year()`, `read_bfs_life_expectancy_data()`,
   `prepare_life_expectancy_data()`, `get_bfs_asset_url()`, `pxR` from DESCRIPTION/`_setup.R`, empty
   `prepare_helpers.R`/`read_helpers.R`/`read.R` if nothing remains), fix unprefixed `left_join`;
   `notes/findings_trends_outcomes.md` with numbers (E1 effect, E2 difference median vs point estimate,
   YLL per year, runtime), `CLAUDE.md` status. Commit.

Commits only on the user's word; each content change in its own commit.

## Critical files

* `scripts/_compile_outcomes.R`, `R/outcomes.R` (existing `drop_incomplete_years()`), `R/prepare.R`,
  `R/prepare_helpers.R`, `R/read.R`, `R/read_helpers.R`
* `R/plot_outcomes.R`, `scripts/_plot_outcomes.R`, `docs/Gesundheitsfolgen.qmd`
* `scripts/_settings.R`, `data/meta/ressources.csv`, `data/meta/outcomes_metadata.csv`
  (CRF, cutoff), `data/tod_nat_gatu.csv` (mortality)
* `tests/testthat/test-outcomes.R`, `tests/testthat/test-plot-outcomes.R`, `tests/regression/run_topic.R`
* Reuse: `check_columns()` (`R/helpers.R`), `filter_ressources()`, `airquality.methods::read_opendataswiss()`
  (sets a user agent), `read_local_csv()`/`write_local_csv()`, `ggplot_timeseries_bars()`,
  `plot_catalog()`, `print_tabset()`.

## Verification

* `devtools::test()` green (new tests first, then implementation).
* `run_topic("outcomes", "candidate")` + `compare_outputs()` against the reference: same 11 columns
  (schema test `test-output-schema.R`), premature deaths change only by E1/E2 (quantified), YLL rows
  present for 3 parameters × 2010–2024 × scenarios; two candidate runs byte-identical (deterministic).
* `run_plots()`/`compare_plots()` for the outcomes; `check_pages()`; `quarto::quarto_render("docs/")`
  and a look at the new figures.
