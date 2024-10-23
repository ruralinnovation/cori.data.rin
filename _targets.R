library(targets)

targets::tar_option_set(
  packages = c(
    "cori.data.cisco",
    "coriverse",
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
  targets::tar_target(rin_service_areas_geojson, write_places_geojson(rin_service_areas, here::here("data/rin_service_areas.geojson"))),
  targets::tar_target(rin_service_areas_package, save_data_to_package(rin_service_areas))
)
