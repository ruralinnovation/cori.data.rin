library(targets)

targets::tar_option_set(
  packages = c(
    "cori.data.cisco",
    "coriverse",
    "cori.db",
    "cori.data",
    "cori.utils", # <= functions imported automatically by coriverse
    "DBI",
    "dplyr",
    "readxl",
    "rlang",
    "tidyr",
    "stringr",
    "sf"  
  )
)

targets::tar_source(files = "R")

# setup_dir()
if (! file.exists("data/")) dir.create("data/")

list(
  targets::tar_target(rin_service_areas_url, c("https://docs.google.com/spreadsheets/d/1Qv3nyQ4GrkhIxVs1uEOgN5tfFLtdt_MA71BquPQDGmw")),
  targets::tar_target(rin_service_areas, load_rin_service_areas(rin_service_areas_url)),
  targets::tar_target(rin_service_areas_sf, load_rin_service_areas_sf(rin_service_areas)),
  targets::tar_target(rin_service_areas_package, save_data_to_package(rin_service_areas_sf)),

  targets::tar_target(rin_service_areas_db, (function (schema_name, table_name, dta) {
    con <- cori.db::connect_to_db(schema_name)
    on.exit(DBI::dbDisconnect(con))
    result <- cori.db::write_db(con, table_name, dta, spatial = TRUE)
    if (result == table_name) return(dta)
    else return(NULL)
  })("rin", "rin_service_areas",rin_service_areas_sf)),

  ## This is handled in impact_metrics project (ruralinnovation/proj_cori_impact_metrics)
  # targets::tar_target(rin_service_areas_tableau_db, save_data_to_db_instance("tableau", "rin", "rin_service_areas", rin_service_areas_db)),

  targets::tar_target(rin_service_areas_geojson, write_data_to_geojson(rin_service_areas_sf, here::here("data/rin_service_areas.geojson"))),
  targets::tar_target(rin_service_areas_s3, write_data_to_s3("cori-risi-apps", "rin_service_areas.geojson", rin_service_areas_geojson))
)
