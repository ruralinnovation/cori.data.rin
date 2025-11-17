library(targets)

tar_option_set(
  packages = c(
    "cori.data.cisco",
    "coriverse",
    "cori.db",
    "cori.data",
    "cori.utils", # <= functions imported automatically by coriverse
    "DBI",
    "dplyr",
    "googlesheets4",
    "purrr",
    "readxl",
    "rlang",
    "tidyr",
    "stringr",
    "sf",
    "sfarrow"
  )
)

tar_source(files = "R/load_data.R")

# setup_dir()
if (! file.exists("data/")) dir.create("data/")

list(
  ### OLD ---
  # tar_target(rin_service_areas_url, c("https://docs.google.com/spreadsheets/d/1Qv3nyQ4GrkhIxVs1uEOgN5tfFLtdt_MA71BquPQDGmw")),
  # tar_target(rin_service_areas, load_rin_service_areas(rin_service_areas_url)),

  ### NEW ---
  tar_target(params_file, command = "params.yml", format = "file"),
  tar_target(global_params, cori.utils::get_params("global", params_file)),
  tar_target(sheet_id, googlesheets4::as_sheets_id(global_params$sheet_url)),
  tar_target(all_sheet_names, googlesheets4::sheet_names(sheet_id)),

  tar_target(rin_service_areas, load_rin_service_areas(global_params, cori.data.rin::rin_service_areas)),
  tar_target(rin_service_areas_sf, load_rin_service_areas_sf(rin_service_areas)),
  tar_target(rin_service_areas_package, save_data_to_package(rin_service_areas_sf)),

  tar_target(rin_service_areas_db, (function (schema_name, table_name, dta) {
    con <- cori.db::connect_to_db(schema_name)
    on.exit(DBI::dbDisconnect(con))
    result <- cori.db::write_db(con, table_name, dta, spatial = TRUE)
    if (result == table_name) return(dta)
    else return(NULL)
  })("rin", "rin_service_areas", rin_service_areas_sf)),

  ## This is handled in impact_metrics project (ruralinnovation/proj_cori_impact_metrics)
  # tar_target(rin_service_areas_tableau_db, save_data_to_db_instance("tableau", "rin", "rin_service_areas", rin_service_areas_db)),

  tar_target(rin_service_areas_geojson, write_data_to_geojson(rin_service_areas_sf, here::here("data/rin_service_areas.geojson"))),
  tar_target(rin_service_areas_geojson_file, command = rin_service_areas_geojson, format = "file"),

  # tar_target(rin_service_areas_s3, write_data_to_s3("cori-risi-apps", "rin_service_areas.geojson", rin_service_areas_geojson_file)),

  # Write rin_service_areas_sf out to .parquet file using arrow::write_parquet
  tar_target(
    rin_service_areas_parquet,
    (function (df, file_path) {
    
      # Write sf object directly to GeoParquet
      st_write_parquet(
        obj = rin_service_areas_sf,
        dsn = file_path
      )
      
      return(file_path)

    })(rin_service_areas_sf, "data/rin_service_areas.parquet")
  ),
  tar_target(rin_service_areas_parquet_file, command = rin_service_areas_parquet, format = "file")

  # # Read GeoParquet Ex.
  # library(arrow)
  # library(sf)
  # library(targets)

  # geoparquet_file <- tar_read(rin_service_areas_parquet_file)

  # dta <- arrow::read_parquet(geoparquet_file)

  # class(dta)
  # # [1] "tbl_df"     "tbl"        "data.frame"

  # class(dta$geometry)
  # # [1] "arrow_binary"  "blob"          "vctrs_list_of" "vctrs_vctr"    "list"

  # library(geoarrow)  # <- This registers new S3 methods
  # class(dta$geometry)  
  # # [1] "arrow_binary"  "blob"          "vctrs_list_of" "vctrs_vctr"    "list" 

  # dta_sf <- sf::st_as_sf(dta)
  # # ^^ Calls: st_as_sf.geoarrow_vctr() method (from geoarrow) on dta$geometry
  # class(dta_sf)
  # # [1] "sf"         "tbl_df"     "tbl"        "data.frame"

  # class(dta_sf$geometry)
  # # [1] "sfc_MULTIPOLYGON" "sfc" 

  # library(sfarrow)
  # dta_sf <- st_read_parquet(geoparquet_file)

  # class(dta_sf)
  # # [1] "sf"         "data.frame"

  # class(dta_sf$geometry)
  # # [1] "sfc_MULTIPOLYGON" "sfc" 
)
