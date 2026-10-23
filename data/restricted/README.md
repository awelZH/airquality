# Restricted input data

Non-public inputs of the analysis. Everything in this folder except this README is gitignored and must
be placed here by hand before a run.

| File | Content | Used by |
|---|---|---|
| `tod_nat_gatu.csv` | natural deaths per year, sex, age (single years from 30, category 290 = aged 0–29) and cause (`krankheitsbedingt`, `andere`) in the Canton of Zurich, Amt für Statistik und Daten Kanton Zürich & BFS; cells with fewer than 4 deaths are suppressed (`NA`) | health outcomes (source `mortality`, ID 29, in `data/meta/ressources.csv`) |

The mortality data may become open government data later; then the source `mortality` points to opendata.swiss and
this file is no longer needed.
