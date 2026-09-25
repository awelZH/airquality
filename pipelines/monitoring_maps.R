# monitoring: BAFU pollutant maps of the canton and their verification against the measured values
# (functions in R/monitoring.R, plots in R/plot_monitoring.R) -> no output file; the report reads the maps,
# the paired values and the fits from the targets store (decision 9, extended 2026-09-25)
#
# the rasters are read again only when an asset changed or a site was added

pipeline_monitoring_maps <- tar_plan(
  # state of the raster assets of the pollutant maps, checked on every run
  tar_target(mon_maps_assets, geo_admin_asset_state(exposition_collections[-1]), cue = tar_cue("always")),

  mon_maps_boundary = sf::st_sf(geometry = sf::st_union(setup_map_municipalities)),
  mon_maps_sites = dplyr::distinct(mon_aq_data, site = as.character(site), x, y),

  # maps averaged onto mon_maps_cellsize and masked to the canton, and the values of the native rasters at the
  # sites, for every year of plot_years with a map
  mon_maps_raster = {
    mon_maps_assets
    read_pollutant_maps(exposition_collections[-1], years = plot_years, boundary = mon_maps_boundary,
                        sites = mon_maps_sites, cellsize = mon_maps_cellsize)
  },

  # O3 peak season derived from NO2 with the model of the exposition (decision 6); its verification is in-sample
  mon_maps_o3 = derive_o3_peakseason_map(mon_maps_raster$maps, expo_pop_coefs_o3),
  # PM2.5 before the first PM2.5 map (2015) from the PM10 maps and the PM2.5:PM10 ratio of the exposition
  # (decision 6); shown as maps only, the verification uses the BAFU maps
  mon_maps_pm25 = derive_pm25_map(mon_maps_raster$maps, expo_pop_ratios_pm, years = plot_years),

  # measured and map values of the same site and year, and the robust regression per parameter
  mon_maps_validation =
    mon_maps_raster$sites |>
    add_o3_peakseason_sites(expo_pop_coefs_o3) |>
    map_validation_data(mon_aq_data, parameters = mon_maps_parameters),
  mon_maps_fit = fit_map_validation(mon_maps_validation)
)
