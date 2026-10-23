# Input metadata

Hand-written inputs of the analysis. `ressources.csv` is the register of all data sources; the other files are
metadata of single sub-analyses and are themselves listed in the register (`SOURCE_TYPE` `repo_file`).

| File | Content |
|---|---|
| `ressources.csv` | register of the data sources: documentation and the endpoints the code reads (below) |
| `airquality_threshold_values.csv` | LRV limit values and WHO guideline values |
| `emikat_subsector_new.csv` | grouping of the subsectors of the emission inventory |
| `outcomes_metadata.csv` | exposure-response functions and cut-off concentrations of the health outcomes |
| `rsd_auxiliary.csv` | vehicle type, fuel and Euro norm for the RSD analysis |
| `rsd_filters.csv` | filter criteria of the RSD data |

## `ressources.csv`

`;`-separated, UTF-8 with byte order mark (opens in Excel), one row per data source. It serves two purposes:
it documents every source (shown as the table "Datengrundlagen" on the website), and the code takes the
endpoint of a source from it by its key: `filter_ressources(ressources, "emikat")` returns `ENDPOINT` and stops
unless exactly one row has that key. `tests/testthat/test-ressources.R` checks the schema and that code and
register agree (keys, geo.admin collections, files in `data/meta/`, `airquality.data` objects, `USED_BY`).

Only **stable facts** are written here. What changes with each data release – the years available, the
spatial resolution per year, the version – is derived by the pipeline on every run and is not edited by hand.

| Column | Content |
|---|---|
| `ID` | stable number of the source (referred to in notes and READMEs); never reused |
| `KEY` | unique key used by the code (`[a-z0-9_]`) |
| `TITLE` | short title |
| `DESCRIPTION` | content, coverage and particularities |
| `PROVIDER` | data owner or author |
| `ACCESS` | `public` or `restricted` (not public, kept in `data/restricted/`) |
| `SOURCE_TYPE` | how the code gets it: `opendataswiss` (CKAN package), `geoadmin_stac` (collection of data.geo.admin.ch), `geolion_wfs` (WFS of the Canton of Zurich), `r_package` (dataset of an R package), `repo_file` (file in this repository), `restricted_file` (file in `data/restricted/`), `reference` (documentation only, not read) |
| `ENDPOINT` | exactly what the code uses: CKAN package-show url, STAC collection id, WFS url, repository path, `package::object`; empty for `reference` |
| `LANDING_PAGE` | page of the source for humans |
| `FORMAT` | format of the data read (csv, parquet, tif, wfs, R, …) |
| `LICENSE` | terms of use |
| `UNIT` | unit of the values, empty where not applicable |
| `TEMPORAL` | temporal resolution of the values (e.g. annual values) |
| `USED_BY` | files of the analysis using the source, `, `-separated; empty exactly for `reference` |

A new source gets the next free `ID`, a `KEY`, and a row before it is used in the code; the test fails
otherwise.
