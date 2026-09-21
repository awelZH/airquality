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
