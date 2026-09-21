# Plots: decisions and findings

Read before working on plots or the report (see also decision 9 in `decisions.md`).

## Plots: grouped legend (2026-09-19)

`ggplot_emissions()` shows one legend block per sector, with the sector as title and the subsectors
without the pasted sector (was "Sektor / Subsektor"); the subsector names in the lookup table were
shortened accordingly by the user. Generic functions, since 2026-09-21 in `airquality.methods`
(decision 10):
* `grouped_key(group, key, order, group_order)` – unique key `"group::key"` as factor; its levels set
  the order of stack and legend ("verschiedene" exists in several sectors)
* `add_grouped_legend(plot, aesthetic, sep, spacing, subtitle, key_spacing)` – sets
  `legendry::guide_legend_group(key_group_split(sep))` plus the theme (block titles from the plot's
  `legend.text`, group spacing 3 mm, `legend.key.spacing.y` 0). The plot stays an ordinary ggplot.

The NH3 special case (agriculture last) is the argument `ggplot_emissions(sectors_last = )` instead
of `%+%` on the finished plot. **Rejected: blocks in two columns** (restore point, commit `3f759b1`):
`legendry` arranges blocks only in one row or column, so the columns had to be drawn separately and
placed with `guide_custom()` – legend twice as wide, fixed when drawn, and building grobs needed a
temporary `ragg` device (a pdf device leaves `Rplots.pdf` and does not know Arial from `theme_ts`).
With the native legend, NMVOC (6 blocks, 11 entries) just fits a 5 in high figure – check when
figures get smaller. Findings: `longpollutant()` is called without the `airquality.methods::` prefix
in several functions of `R/plot.R` (works only while the package is attached; fixed in
`ggplot_emissions()`); neighbouring subsectors of a sector can get similar shades because the colour
ramp is assigned over all pollutants (e.g. PM2.5 Haushalte).

## Rework of the plot code, phase 2a (2026-09-21)

Scope: the plots of emissions, monitoring and exposition (outcomes and trends left untouched on the
user's decision, colour shades of the subsectors postponed). Regression: all **295 figures
byte-identical** (`tests/regression/run_plots.R`, see `regression.md`), the candidate run in its own
environment, i.e. no function reads a global any more.

* `R/plot.R` (863 lines) split by page: `R/plot.R` keeps the shared building blocks
  (`ggplot_timeseries_bars()`, `plotlist_to_tibble()`, `get_plot()`, `build_panel()`) and, at first unchanged,
  the functions of outcomes and trends (reworked afterwards, see below); `R/plot_emissions.R`, `R/plot_monitoring.R`,
  `R/plot_exposition.R` hold the rest.
* Globals became arguments: `theme`, `pointsize`, `jitter_seed`, `colour_scale`/`fill_scale`/
  `shape_scale`, `threshold_values`, `crs`. Presentation settings stay in `_plot_setup.R` (decision 7);
  new there: `plot_emissions_sectors_last` (was a helper function inside `_plot_emissions.R`).
* The ggplot code written out in the scripts became functions: `plot_rsd_per_norm()`,
  `plot_rsd_per_yearmodel()`, `plot_rsd_per_yearmeas()`, `plot_threshold_comparison()`,
  `plot_ndep_sites()`, `plot_ndep_sites_vs_cln()`, `plot_population_over_thresholds()`,
  `plot_population_over_thresholds_share()`, `ggplot_expo_cumulative_years()` (the "alle" plot of
  pollutants and Ndep, before twice in the script). Data preparation as pure, tested functions:
  `prepare_plot_airquality()`, `prepare_plot_ndep()`, `prepare_plot_ndep_components()`,
  `threshold_comparison_data()`, `population_over_thresholds()`. The three scripts shrank from 575 to
  182 lines.
* Style: `purrr::map()` instead of `lapply()`, `.by`, `pivot_longer(cols_vary = "slowest")`/
  `pivot_wider()` instead of `gather()`/`spread()`, `join_by()`, prefixes everywhere
  (`longpollutant()`, `round_off()`, `immissionscale()`, `waiver()`). To stay byte-identical, row
  orders were kept where they matter: `combine_thresholds()` still joins from the sorted thresholds
  (the jitter of the threshold comparison follows the row order), the doughnut slices are sorted like
  `group_by()` did.
* Removed as dead code: `ggplot_timeseries_lines()` and the `subareas` branch of
  `plot_pars_popmean_timeseries()`; the plot `ndep_mean_sources_fractions` (built, never put into
  `plots_monitoring`); `aggregate_map()` and `map_canton` in `_setup.R` (clipped the rasters in the old exposition readers, unused since the exposition rework `696ec49`, which assigns cells via `map_municipalities`). Moved:
  `prepare_ressources()` → `R/helpers.R`, `calc_population_weighted_mean()` → `R/exposition.R`;
  `R/aggregate_helpers.R` deleted. `prepare.R`, `aggregate.R`, `read.R`, `prepare_helpers.R`,
  `read_helpers.R` now hold outcome/trend code only. Left in place although unused, because it is
  outcome code: `get_base_scenario_year()`.
* Tests: `test-plot.R` (shared), `test-plot-emissions.R`, `test-plot-monitoring.R`,
  `test-plot-exposition.R`, helpers in `helper-plot.R`; 75 expectations.

Findings, not changed (content, for the user to decide):
* The doughnut plot `rel_various` is built but commented out in `Belastungsverteilung.qmd`.
* The commented snippet "für Umweltbericht" in `_plot_exposition.R` that used the intermediate `d`
  was dropped; the other two snippets now use `data_population_over_thresh`.

**Jittered points keep their values** (user decision 2026-09-21, own commit): `position_jitter(width = )`
without `height` also shifted the points along the value axis, by up to 40 % of the data's resolution
(smallest gap between distinct values). With the real data that was at most 0.0002 kg N/ha/a (ndep
2019+) or 0.02 % of the critical load, i.e. invisible, but with few distinct values it is large
(synthetic test: 10 → 13.3). Now `height = 0` in the threshold comparison, `plot_ndep_sites()` and
`plot_ndep_sites_vs_cln()` (tested); only these 3 of the 295 figures changed.

## Plots of outcomes and trends (2026-09-21)

The user released the plot part of outcomes and trends for the rework (their data scripts stay WIP).
Same pattern; the 9 figures **byte-identical**, and all 304 figures of the five topics once more
together in a fresh session.

* `R/plot_outcomes.R`: `plot_pars_prelim_deaths_timeseries()` with `theme` and `covid_years`
  (default 2020:2022) as arguments; the parameters are a setting, `plot_parameters_outcomes` in
  `_settings.R`.
* `R/plot_trends.R`: `plot_timeseries_trend_relative()` (magrittr lambdas `. %>% …` in
  `geom_*(data = )` replaced by `\(d) …`; no `%>%` is left in `R/`, `scripts/` or `docs/`), new
  `plot_emission_trends_relative()` (was written out in the script) and the data functions
  `emission_trends_relative()`, `trend_data_overview()`, `trend_data_detailed()`,
  `recode_trend_types()`, `label_ozone_metric()`. `prepare_emission_trends()` (trend data, WIP) is
  called unchanged.
* `_plot_setup.R` attaches only `airquality.methods` now (was dplyr, tidyr, ggplot2, scales, sf too),
  because `prepare_emission_trends()` calls `longpollutant()` without prefix; `viridis_pal()` got its
  prefix. The R code of all Quarto pages runs without the other packages attached (checked with
  `knitr::purl()`, no render).
* Open (content): the detailed trend plot keeps the trends per site up to the current calendar year
  (`lubridate::year(Sys.Date())`), not `year_last` (decision 7).

## Plot catalog and page check (2026-09-21, P1 of the second round)

Plan of the second round (user decision 2026-09-21): P1 plot catalog, P2 year slider instead of year
tabsets, P3 analysis logic out of the pages, P5 consistent names, P6 clean-up; not P4 (parameter
table), but `timeseriespars()`/`expositionpars()` move into the plot settings.

**P1.** The plot scripts deliver a catalog: `plot_catalog(figures, plot, names_to)` gives one row per
figure with `plot`, `parameter`, `year` (character, `NA` if not applicable) and `figure`; the caller says
what the list names mean (`"parameter"`, `"year"`, both), nothing is guessed any more
(`plotlist_to_tibble()` recognised years by a regex on the first name and misused `pollutant` for plot
names such as `timeseries_various`). `get_plot(catalog, plot, parameter, year)` replaces the filter
strings (`parse_expr()`, silently the first match) and stops with class `airquality_plot_error` unless
exactly one plot matches, listing the available ones. Renamed plot ids: `population_over_thresh`
(was pollutant `timeseries_various`), `population_over_thresh_share` (`rel_various`). The 32
`get_plot()` calls of the pages were converted mechanically. 304 figures byte-identical (compared by
content, `compare_plots_content()`, because the file names changed with the catalog).

`tests/regression/check_pages.R` runs the code chunks and inline expressions of all pages without
rendering (7 pages OK, about 4 min while the tabsets are still knitted inline). Pitfall: `knitr::knit()`
in inline code writes its figures to `docs/figure/` unless `fig.path` points elsewhere.

**P2 (year slider, decision 11).** `build_panel()`, the `id`/`markdown` columns, the `arrange()` per
page and the 21 inline `knitr::knit(text = )` calls are gone; `print_year_slider()` (17 sliders on
`Belastungsverteilung.qmd`) and `print_tabset()` (3 on `Gesundheitsfolgen.qmd`). Check: render of both
pages, all 264 + 6 figures byte-identical to the baseline render, headless Edge DOM: 17 slider
controls, exactly one visible panel each (241 hidden), start "alle" or newest year. Found on the way:
the tabset "Ndep / population_weighted_mean_map" was silently empty (there is no ndep map; the old
filter matched nothing) – removed; `print_year_slider()` would have stopped. Figure file names changed
(`slider-<parameter>-<plot>-<n>.png`); no contract. Pitfall for `check_pages()`: plots printed to a pdf
device fail on the Arial font of `theme_ts`; it draws with ragg into a temporary directory.
