# Emissions: analysis decisions and findings

Phase 2a, 2026-09-18/19. Read before working on emissions.

`scripts/_compile_emission_data.R`: read → prepare → aggregate → write; functions in `R/emissions.R`,
92 expectations in `tests/testthat/test-emissions.R`. Refactoring (commit `f4d941f`) byte-identical;
the reference was byte-identical to the committed outputs.

| old | new |
|---|---|
| `prepare_emmissions(data, filter_args = <quoted expression>)` | `prepare_emissions(data, canton, exclude_subsectors, year_max)` |
| `aggregate_emmissions()` incl. plot colours and an unused, with 0.4.0 no longer working automatic regrouping `groups_emission_subsector()` | `aggregate_emissions()` + `group_minor_subsectors()` + `add_emission_colours()` |
| `prepare_rsd(data, rsd_auxiliary)` with hidden `year(Sys.Date())` as newest vehicle model year | `prepare_rsd(data, meta, filters, model_year_max)`, script passes `emis_year_max` |
| `aggregate_rsd_nox(data, rsd_auxiliary, groups)` + `aggregate_rsd()` | `aggregate_rsd_nox(data, meta, filters, groups)` |
| `source` hard-coded, `tidyr::spread()`, `group_by()`/`ungroup()` | `source` from the data, `pivot_wider()`, `.by` |

**Subsector grouping: lookup table + per-pollutant rule** (user decision 2026-09-19). Criteria: at
most 4 subsectors per pollutant and sector **including** "verschiedene"; subsectors with a mean
yearly share < 5 % of the pollutant's canton total go into "verschiedene"; the lookup table lists all
subsectors.
* `emikat_subsector_new.csv` lists all 35 subsectors and only merges/renames thematically; no entry
  maps to "verschiedene" any more. `aggregate_emissions()` warns about missing subsectors.
* `group_minor_subsectors(min_share = emis_subsector_min_share, max_per_sector =
  emis_subsectors_max)` (settings 0.05 and 4): mean of the **yearly** shares (years without emission
  count as 0), so a pollutant keeps the same groups over its whole time series; if an "verschiedene"
  group exists or there are more than 4 subsectors, only the 3 largest keep their name.
* Result: `data_emissions.csv` 3996 → 2772 rows, totals per year, pollutant and sector unchanged
  (≤ 4e-16). Now named e.g. Zonenverkehr (CO 28 %), Flächenquellen (SO2 18 %); now in
  "verschiedene" e.g. Feuerungen Öl & Gas for PM/eBC/NH3/CO. Some sector/pollutant combinations
  consist of "verschiedene" only (e.g. NOx Industrie 11 %).
* Why a lookup table alone cannot do it: "small" depends on the pollutant (39 named combinations
  stayed below 5 % in every year, while 14 subsectors in "verschiedene" reached ≥ 5 %).
* **Restore point** of the lookup-only state: commit `f4d941f`, git tag `emissions-lookup-only`.
* Plot order and colours are ranked **without** the projections 2030/2040/2050
  (`prepare_emissions(year_max = )`); with the current data the ranking is the same either way.

Findings: the RSD aggregation completes all factor-level combinations with `n = 0` (two empty rows
today). An experiment that read `emikat_subsector_new.csv` without `locale(encoding = "UTF-8")`
matched only 19 of 32 entries (umlauts) – always pass the locale.
