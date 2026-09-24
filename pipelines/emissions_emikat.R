# emissions: emission inventory (EMIKAT) of the Canton of Zurich per year, pollutant, sector and subsector
# (functions in R/emissions.R) -> data_emissions.csv

pipeline_emissions_emikat <- tar_plan(
  # emission budgets per sector group and subgroup from opendata.swiss, checked on every run
  tar_target(
    emis_emikat_raw,
    airquality.methods::read_opendataswiss(filter_ressources(setup_ressources, 1), source = "Ostluft & BAFU"),
    cue = tar_cue("always")
  ),
  # lookup table merging and renaming subsectors thematically
  tar_target(emis_emikat_file_subsectors, filter_ressources(setup_ressources, 27), format = "file"),
  emis_emikat_subsectors = airquality.methods::read_local_csv(emis_emikat_file_subsectors, locale = readr::locale(encoding = "UTF-8")),

  # latest inventory version without projections beyond emis_year_max, zero emissions and redundant subsectors
  emis_emikat_prepared = prepare_emissions(emis_emikat_raw, year_max = emis_year_max),
  # emissions per year, pollutant, sector and grouped subsector
  emis_emikat_aggregated = aggregate_emissions(emis_emikat_prepared, emis_emikat_subsectors),
  # small subsectors into "verschiedene", at most emis_subsectors_max per sector (readable plots)
  emis_emikat_grouped = group_minor_subsectors(emis_emikat_aggregated, min_share = emis_subsector_min_share,
                                               max_per_sector = emis_subsectors_max),
  # plot order and colour per subsector
  emis_emikat_emissions = add_emission_colours(emis_emikat_grouped),

  tar_target(emis_emikat_out, write_output(emis_emikat_emissions, "data_emissions.csv", dir = path_output), format = "file")
)
