# report: renders the Quarto website of report/ into docs/; the pages build their plots from the output CSVs
# while rendering (decision 9), so the report depends on the output files, not on plot targets

pipeline_report <- tar_plan(
  # sources of the report (pages, plot scripts, styles) and what the plot scripts source themselves (the
  # functions in R/, settings.R), listed on every run so that new files count too
  tar_target(
    report_sources,
    c(list.files(c("report", "R"), recursive = TRUE, full.names = TRUE), "settings.R"),
    format = "file",
    cue = tar_cue("always")
  ),
  # threshold values of the plots
  tar_target(report_file_thresholds, filter_ressources(setup_ressources, 10), format = "file"),

  # outputs of the work-in-progress trends (wip/trends.R, outside the pipeline) and a warning when they are
  # older than the pipeline outputs they are computed from
  tar_target(
    report_wip_trends,
    file.path(path_output, c("data_airquality_trends_relative_y1.csv", "data_airquality_trends_relative_aggregated_y1.csv")),
    format = "file"
  ),
  report_wip_check = check_wip_outputs(report_wip_trends, based_on = c(mon_aq_out, emis_emikat_out)),

  tar_target(
    report_site,
    {
      list(report_sources, report_file_thresholds, report_wip_trends, report_wip_check,
           emis_emikat_out, emis_rsd_out_per_norm, emis_rsd_out_per_yearmodel, emis_rsd_out_per_yearmeas,
           mon_aq_out, mon_ndep_out_parameters, mon_ndep_out_total,
           expo_pop_out_means_canton, expo_pop_out_means_municipalities, expo_pop_out_distribution,
           expo_eco_out_distribution, outcomes_out,
           # read from the store by the plot script (no output file)
           mon_maps_raster, mon_maps_o3, mon_maps_pm25, mon_maps_boundary, mon_maps_validation, mon_maps_fit, expo_eco_ndep)
      render_report("report", output_dir = "docs")
    },
    format = "file"
  )
)
