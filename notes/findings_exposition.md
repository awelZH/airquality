# Exposition: analysis decisions and findings

Step 1 (exposition rework, commits 889a80b, 696ec49, 39ebaa8 on `dev`). Read before working on exposition.

* **Double counting of exclave and lake cells** (fixed): `merge_statpop_with_subareas()` joined by
  `bfs`, but the geolion map has several features for some `bfs` numbers (Glattfelden 58,
  Mönchaltorf 196, `bfs = 0` three features). Their cells were counted twice resp. three times in the
  weighted means (Glattfelden 2023: 10'862 instead of ~5'400 inhabitants); distributions unaffected.
* **Cropping**: the old readers cropped every raster to the canton polygon at its native resolution
  before warping to 100 m; now rasters are read for the bounding box, warped, and cells are selected
  by their 100 m centre. Affects only cells at the canton border (2010–2019 ~600 inhabitants/year in
  17 border municipalities, up to 2.4 %; from 2020 identical).
* Regression 2026-09-18 vs. the old pipeline (2024, NO2, canton): population 1'634'927 → 1'624'157
  (double counting −0.66 %, collector pixels a further −0.43 %), weighted mean 12.3841 → 12.3973.
  The refactoring alone (`legacy`) reproduced the old results (≤ 0.01 %). Details and the variant
  scripts: `tests/regression/run_exposition_variants.R`, `compare_exposition.R`.

## Censuses 1990 and 2000 (2026-10-23, decision 12)

Sandbox prototype first (own reader beside `read_statpop_ha()`, removed again), then `airquality.methods`
0.11.0 (`read_statpop_ha()` reads both datasets) and `expo_years <- c(1990, 2000, 2010:year_last)`.

| | STATPOP 2010– | census 1990 | census 2000 |
|---|---|---|---|
| assets | ha parquet/csv, noloc csv (EPSG 2056) | `…_1990_ha_21781.csv` (125 MB) | `…_2000_ha_21781.csv` (192 MB) |
| corner | `E_KOORD`/`N_KOORD` (LV95) | `KX`/`KY` (LV03) | `X`/`Y` (LV03) |
| inhabitants | `BBTOT` | `P9BTOT` | `P00BTOT` |
| collector pixels | yes | none | none |
| rows with 0 inhabitants (CH) | none (all 16 years) | 33'581 | 34'628 |

* Grid: all corners on 100 m, `RELI` = lower-left corner (all rows). For 2010 `E_KOORD`/`N_KOORD` equal the
  LV03 corner + 2'000'000 / 1'000'000 in all 338'457 rows, so the shift is exact; the canton window of all
  years is `100 x 100 m | 478 x 596 cells | origin 2669200 / 1283400`.
* Anonymisation as in STATPOP (no values 1–2, minimum 3). Sums CH 1990 6'955'789, 2000 7'351'935.
* Canton (cells with a municipality): 1990 1'182'737 inhabitants in 33'118 cells, 2000 1'252'256 in 34'552,
  2010 1'377'228 in 35'783; all 160 municipalities inhabited in every year.
* Inhabitants without a base-scenario value (cell not inhabited in 2015): 1990 2.6 %, 2000 1.2 %
  (2010 0.8 %, as before). The base-scenario mean of these years covers the other inhabitants only.
* Pollutant rasters: NO2 and O3 98p from 1990, PM10 from 1998, PM2.5 from 2015.

Population-weighted canton means (µg/m3):

| parameter | 1990 | 2000 | 2010 | 2015 |
|---|---|---|---|---|
| NO2 | 37.3 | 27.8 | 25.0 | 24.3 |
| O3 98p | 182.4 | 153.1 | 170.5 | 170.5 |
| O3 peak season (model from 4 sites) | 89.0 | 88.5 | 87.0 | 94.0 |
| PM10 | – | 22.3 | 19.7 | 15.7 |
| PM2.5 (2000, 2010 from PM10) | – | 16.5 | 14.8 | 10.4 |

O3 98p 2000 is low in the measurements too: median of the ZH sites (`mon_aq_raw`) 1990 170 (n = 11),
2000 152 (n = 24), 2010 165 (n = 33).

Check on frozen inputs (`run_pipeline("census")` vs `"methods011"`, 2026-10-23): in all 12 outputs the rows
from 2010 on are byte-identical and the headers unchanged; new are 1990/2000 in the three population
exposition files (+7 canton, +1120 municipality, +220 distribution rows) and 2000 in
`data_health_outcomes.csv` (+6 rows, premature deaths only: no life table before 2010).

Open: whether `P9BTOT`/`P00BTOT` count the population by the same definition as `BBTOT` (census: possibly
the economic residence, STATPOP the permanent resident population). Not verified; the BFS census 2000
geodata documentation (dam-api.bfs.admin.ch asset 14607398) is the place to check. A difference would be a
break between 2000 and 2010 that cannot be corrected, only documented.

O3 peak season with `expo_o3_nmin_sites` 4 instead of 7 (user decision 2026-10-23): offsets 1990 114.4
(4 sites, sd of the residuals 9.5), 1991 116.6 (5 sites, 11.1); slope −0.6642 (was −0.6621); the offsets
1992–2025 move by at most 0.08. Outputs against the commit before: only the O3 peak-season rows change
(canton +1 row 1990, municipalities +160, distribution classes shifted, outcomes O3), at most 0.1 % relative
(canton mean 2010 87.001 → 86.998 µg/m3). Canton 1990: 89.0 µg/m3.
