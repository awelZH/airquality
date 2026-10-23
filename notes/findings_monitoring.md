# Monitoring: analysis decisions and findings

Phase 2a, 2026-09-19. Read before working on monitoring.

`scripts/_compile_monitoring_data.R` is 33 lines (was 127 with 7 helper functions inside); functions
in `R/monitoring.R`, 40 expectations in `tests/testthat/test-monitoring.R`, cut along the later
sub-analyses `mon_aq_` and `mon_ndep_`. Inputs are package datasets of `airquality.data` 0.1.3 (no
network). **Two bugs were fixed first (commit `c390948`), then the refactoring (commit `e77e627`)
reproduced the fixed outputs byte-identically.**

* `prepare_monitoring_airquality(data, cantons = mon_cantons)`; the extra NABEL site names were
  redundant (both carry `canton = "ZH"`).
* ndep: `prepare_ndep_site_meta()` → `prepare_ndep_parameters()` → `aggregate_ndep()`, plus
  `recode_ecosystems()`, `derive_source_category()` and the `classify_*()` functions (thresholds as
  documented defaults). Since 2026-09-25 (0.5.0) these classes live in `airquality.methods`
  (decision 10), called with the `airquality.methods::` prefix.
* Fixes: the factor levels `c("hoch", "mittel", "tiel")` turned every "tief" into `NA` (14 of 132
  rows, 98 of 924); ecosystem "Siedlungen" (only site WIE, `cln = NA`) now counts as
  "kein empf. Ökosys." (user decision). No value changed, and the plots are unaffected (they drop
  `NA`/"Siedlungen" ecosystems and rows without `cln` and overwrite the site class).
* Note: the committed ndep outputs had been written with an older code version than the script.
* Removed as dead code: `aggregate_nitrogen_deposition()`, `simplify_nitrogen_parameters()`.
* **`datasource` cleaned up** (user decision 2026-09-21, own commit): the column mixed spellings and
  duplicates, because the input already contains combined entries such as "FUB, Ostluft" and the old
  code only pasted the distinct *entries* in row order. `combine_sources()` now splits at the comma,
  trims, deduplicates and sorts, so the column no longer depends on the row order of the input. The
  132 rows of `data_ndep_monitoring_y1.csv` went from 6 spellings (`FUB,Ostluft` 24, `Ostluft,FUB`
  22, `FUB,FUB, Ostluft,Ostluft` 3, `FUB,Ostluft,FUB, Ostluft` 5, `Ostluft,FUB, Ostluft,FUB` 2,
  `Ostluft` 76) to 2 (`FUB,Ostluft` 56, `Ostluft` 76). Only this column changed; no value, and the
  other two outputs stay byte-identical.

## Pollutant maps of the canton and model verification (2026-09-25)

Replaces `wip/_validate_rasterdata.R` (a fragment that did not run: `data_raster_aq` and `years` were
undefined). Sub-analysis `pipelines/monitoring_maps.R` (`mon_maps_`), functions in `R/monitoring.R`
and `R/plot_monitoring.R`, shown on the page "Luftqualität" after each time series as a tabset "Karte"
(year slider) / "Modellverifikation". **No output file**: the report reads the targets from the store
(extension of decision 9). User decisions 2026-09-25: data in the store; O3 peak season as a derived map,
its verification marked in-sample; robust regression with `MASS::rlm`. Revised the same day (user): no
prediction band, no equation and no n in the plot; year as continuous colour; one panel and one
regression line per traffic influence (verkehrsbelastet / Hintergrund), no site class symbols; 1:1
line and regression line as legend entries; derived
PM2.5 maps before 2015; maps centred like the municipality maps of the exposition; a map of the
exceedance of the critical loads for nitrogen (no verification).

* **Rasters** (BAFU, data.geo.admin.ch), years in `plot_years` (1995–2025): NO2 31 (200 m until 2019,
  20 m from 2020), O3_max_98p_m1 31 (200 m / 100 m from 2020), PM10 28 (from 1998, 200 m / 100 m from 2020),
  PM2.5 11 (from 2015, 100 m). Maps averaged onto 100 m (`mon_maps_cellsize`, grid 478 × 596 over the
  canton bbox) and masked by the union of the municipalities; the values at the sites come from the
  native cells.
* **PM10 1998–2001 without EPSG code**: these four files carry the LV95 projection without EPSG code
  and without `towgs84` (projection parameters identical to EPSG:2056; the STAC metadata say
  `proj:epsg = 2056`); all other 97 rasters of 1995–2025 carry EPSG:2056. `airquality.methods` 0.5.1
  refused them, so they were skipped at first (variant C). Since 0.5.2 (commit `2e1b0ff`,
  2026-09-25) `resolve_tif_crs()` reads them as EPSG:2056 with a message (both the STAC metadata and
  the projection parameters agree). `read_pollutant_maps()` still skips a year that cannot be read,
  with a warning, as a general guard.
* **O3 peak season**: `offset + slope * NO2` with `expo_pop_coefs_o3` (decision 6), on the 100 m NO2
  grid (linear, so averaging first is exact) and on the NO2 values at the sites; 31 years. In-sample,
  the coefficients come from the same (and further) monitoring sites; noted in the caption.
* **PM2.5 before 2015**: PM10 map times the yearly PM2.5:PM10 ratio of the NABEL sites
  (`expo_pop_ratios_pm`, as in the exposition), for the PM10 map years without a PM2.5 map: 2000–2014
  (15 maps; the ratio starts in 2000, so PM10 1998/1999 give none). Maps only; the
  verification uses the BAFU maps from 2015 (the derived years would be in-sample like the O3 peak season).
* **Nitrogen**: map of the exceedance of the critical loads from `expo_eco_ndep` (the target of the
  exposition, 1 km cells with a sensitive ecosystem, nothing read again); model years in `plot_years`:
  2000, 2005, 2010, 2015, 2020; scale `immissionscale("Ndep")`.
* **Swapped coordinates in `airquality.data` 0.1.3**: the NABEL sites Dübendorf-EMPA
  (x = 1250900, y = 2688675) and Zürich-Kaserne (x = 1247990, y = 2682450) have x and y swapped, so
  they fell outside the rasters (194 of 20952 site values `NA`). `extract_at_sites()` swaps coordinates
  with `x < y` for the lookup (unambiguous in LV95) and returns them as given; now 0 `NA`. The error is
  also in `data_airquality_monitoring_y1.csv` (the contract) and was **not** changed there – open item
  for `airquality.data` / a user decision.
* **Pairs** (measured value and map value of the same site and year): NO2 1850 at 214 sites,
  O3_max_98p_m1 275/27, O3 peak season 270/27, PM10 214 (1998–2025), PM2.5 74/17. 532 NO2 rows of the
  monitoring data have no measured value (`NA`) and drop out.
* **Robust regression** map ~ measured, all years, one per parameter and traffic influence (user
  2026-09-25: two panels "verkehrsbelastet" / "Hintergrund", one line each; year as continuous colour,
  no site class symbols). Intercept, slope, robust scale s (µg/m3), n:

  | parameter | Hintergrund | verkehrsbelastet |
  |---|---|---|
  | NO2 | 3.22, 0.898, 2.41, 976 | 9.22, 0.527, 4.12, 874 |
  | O3_max_98p_m1 | 29.1, 0.819, 4.53, 208 | 47.3, 0.814, 7.34, 67 |
  | O3 peak season (in-sample) | 21.2, 0.738, 2.55, 206 | 51.3, 0.373, 4.45, 64 |
  | PM10 (from 1998) | 0.94, 0.939, 0.87, 149 | 6.78, 0.613, 1.54, 65 |
  | PM2.5 | 1.44, 0.855, 0.36, 53 | 4.32, 0.643, 0.57, 21 |

  At background sites the maps follow the measurements closely (NO2 slope 0.90, PM10 0.94); at traffic
  sites the slope is much flatter (NO2 0.53, PM10 0.61, PM2.5 0.64) and the spread larger: the maps
  underestimate high traffic values. The single line over all sites (first version: NO2 7.33, 0.655)
  mixed both groups. A prediction band was shown first and dropped (user); it assumed a constant
  spread and was too wide at low and too narrow at high NO2 values.
* **Verification per year** (user 2026-09-28): a third tab "Modellverifikation pro Jahr" with a year slider
  (a nested tabset would exceed the six heading levels inside the O3 tabs). Same plot as over all years,
  with the fit of the year and its 95 % prediction band (fit ± t · sqrt(s² + se²), robust scale s); axes
  and colour scale shared by all years of a parameter. Only years with at least `mon_maps_min_pairs` = 5
  pairs ("more than 4"), and a line and band only in panels with at least 5 pairs: NO2 31 years (lines:
  Hintergrund 31, verkehrsbelastet 27), O3_max_98p_m1 31 (31 / 1), O3 peak season 31 (30 / 1), PM10 26
  from 1998 (19 / 1), PM2.5 8 from 2018 (7 / 0). Targets `mon_maps_validation_years`, `mon_maps_fit_years`.
* **Cost**: `mon_maps_raster` 189 s (97 rasters streamed), the whole sub-analysis 3.6 min on the first
  run; store 7.5 MB (maps) + 3.7 MB (O3 maps). Rerun only when an asset changes or a site is added
  (`mon_maps_sites` depends on `mon_aq_data`).
* **From 1990, derived PM2.5 verified, a plot for every year** (user 2026-10-23): `plot_years <- 1990:year_last`
  (also the time series, the threshold comparison from "1990–1992" and the nitrogen maps); maps NO2 36,
  O3_max_98p_m1 36, O3 peak season 36 (the O3 model covers 1990 since `expo_o3_nmin_sites` 4, exposition
  decision 12), PM10 28, PM2.5 11 + 15 derived. The derived PM2.5 maps 2000–2014 are now verified as well
  (`add_pm25_sites()`: PM10 value at the site × ratio of the year), noted "in-sample 2000–2014" in the
  caption and, per year, in the derived years only (`plot_map_validation(in_sample = <note>)`,
  `plot_map_validation_years(in_sample_years = )`). They add 1–2 pairs a year (2000–2002, 2008–2014; none
  2003–2007), all at background sites: PM2.5 Hintergrund over all years 0.53, 0.962, n 67 (was 1.44, 0.855,
  53), verkehrsbelastet unchanged. Per year a plot needs `mon_maps_min_pairs_plot` = 1 pair, a line and band
  still `mon_maps_min_pairs` = 5 per panel: plots NO2 36, O3 98p 36, O3 peak season 36, PM10 28, PM2.5 21
  (was 8, from 2018: 2015–2017 had 2, 2 and 4 pairs).
