
# Install required packages if not already installed
if (!requireNamespace("googlesheets4", quietly = TRUE)) {
  install.packages("googlesheets4")
}

library(googlesheets4)
library(dplyr)
library(purrr)

# # Optional: Save the token for future use
# # This allows the script to run without user intervention
# gs4_auth_configure(path = credentials) # <= THIS DOES NOT WORK... fow now

#' A function to generate RIN service areas (county level) from an XLSX extract from Monday (see params.yml)
#'
#' @param params An object containing values for the $current_year and $monday_network_communities_file_name parameters
#' @param old_rin_service_areas data.frame of previous RIN service areas build
#'
#' @return data.frame of all counties associated with each RIN community
#' 
#' @importFrom dplyr %>%
#' @importFrom dplyr bind_rows
#' @importFrom dplyr distinct
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stats filter
#' @importFrom tidyr separate_rows
load_rin_service_areas <- function (params, old_rin_service_areas) {

  ghost_communities <- c("Grinnell", "Montgomery County", "North Iowa", "Pittsburg")

  stopifnot(file.exists("data"))

  data_dir <- "./data"

  ### functions ------
  get_county_geoid_name_lookup <- function(year = 2024) {
    
    counties <- cori.data::tiger_line_counties(year) |>
      sf::st_drop_geometry()
    
    states <- cori.data::tiger_line_states(year) |> 
      sf::st_drop_geometry()
    
    # state_names <- states %>%
    #   dplyr::select(GEOID, STUSPS)
    
    county_geoid_name_lookup <- counties |>
      dplyr::left_join(
        states,
        by = c("STATEFP" = "GEOID")
      ) |>
      dplyr::mutate(
        name_co = paste0(`NAMELSAD`, ", ", `STUSPS`)
      ) |>
      dplyr::select(geoid_co = `GEOID`, `name_co`) |>
      dplyr::distinct()
    
    return(county_geoid_name_lookup)
    
  }

  # Load a county geoid_co name_co lookup
  county_geoid_name_lookup <- get_county_geoid_name_lookup()

  ## read in
  rin <- readxl::read_excel(paste0(data_dir, "/", params$monday_network_communities_file_name), skip = 2)
  
  names(rin) <- snakecase::to_snake_case(names(rin))

  rin_only <- rin |> 
    dplyr::filter(!is.na(`name`)) |> 
    dplyr::filter(`name` != "Subitems")

  rin_primary_co <- rin_only |>
    dplyr::select(
      `name`,
      `county` = `primary_county`
    )

  areas <- rin_only |> 
    dplyr::select(
      `name`,
      `county` = `other_counties`
    ) |> 
    dplyr::filter(
      !is.na(`county`)
    ) |> 
    tidyr::separate_rows(`county`, sep = "(?<=,\\s[A-Z]{2}),\\s*") |> 
    dplyr::bind_rows(`rin_primary_co`) |> 
    dplyr::left_join(county_geoid_name_lookup, by = c('county' = 'name_co')) |>
    dplyr::mutate(
      `geoid_co` = ifelse(`county` == 'Harrisonburg County, VA', '51660', `geoid_co`),
      `geoid_co` = ifelse(`county` == 'Independent City', '51590', `geoid_co`),  # Danville city, VA
      `geoid_co` = ifelse(`county` == 'Natchitoches County, LA', '22069', `geoid_co`)  # Natchitoches Parish, LA
    ) |> 
    dplyr::mutate(
      primary_county_flag = "No",
      year = params$current_year
    ) |>
    dplyr::select(
      `geoid_co`,
      `rin_community` = `name`,
      `county`,
      `primary_county_flag`,
      `year`
    )
  
  check_primary_county <- function (county, rin_community_name, rin_primary_counties) {
    
    primary_county <- (rin_primary_counties |> dplyr::filter(`name` == rin_community_name))$county
  
    if (length(primary_county) > 0) {
      if (county %in% primary_county) return("Yes")
      else return("No")
    } else {
      return("No")
    }
  }
  
  for (r in c(1:nrow(areas))) {
    name <- areas[r, ]$rin_community
    county <- areas[r, ]$county
  
    areas[r, ]$primary_county_flag <- check_primary_county(county, name, rin_primary_co)
  }

  # STEP 1: Today's snapshot (from Monday XLSX) is simply `areas`
  todays_snapshot <- dplyr::distinct(areas)

  # STEP 2: Preserve existing records from package data (directly from old_rin_service_areas)
  has_latest_version_col <- "latest_version" %in% names(old_rin_service_areas)

  base_cols <- c("geoid_co", "rin_community", "county", "primary_county_flag", "data_run_date", "year")
  if (has_latest_version_col) base_cols <- c(base_cols, "latest_version")

  preserved_old <- old_rin_service_areas |>
    sf::st_drop_geometry() |>
    dplyr::filter(
      !(rin_community %in% ghost_communities & year %in% c(2024, 2025))
    ) |>
    dplyr::select(dplyr::all_of(base_cols))

  if (!has_latest_version_col) {
    preserved_old <- preserved_old |>
      dplyr::mutate(latest_version = "No")
  }

  # STEP 3: Check if params.yml has uncommitted changes to monday_network_communities_file_name
  git_diff_output <- system("git diff HEAD params.yml", intern = TRUE)
  has_uncommitted_filename_change <- any(grepl("monday_network_communities_file_name", git_diff_output))

  if (has_uncommitted_filename_change) {
    new_only <- todays_snapshot |>
      dplyr::mutate(data_run_date = Sys.Date()) |>
      dplyr::anti_join(preserved_old, by = c("rin_community", "county", "year", "data_run_date"))
    has_new_records <- nrow(new_only) > 0
  } else {
    has_new_records <- FALSE
    new_only <- dplyr::slice(todays_snapshot, 0) |>
      dplyr::mutate(data_run_date = as.Date(NA))
  }

  # STEP 4: Only if there are new records, update latest_version flags
  if (has_new_records) {
    preserved_old <- preserved_old |>
      dplyr::mutate(latest_version = "No")
    new_only <- new_only |>
      dplyr::mutate(latest_version = "Yes")
  }

  # STEP 5: Final combination
  final_result <- dplyr::bind_rows(preserved_old, new_only) |>
    dplyr::distinct() |>
    dplyr::arrange(year, rin_community, county, data_run_date)

  return(final_result |> as.data.frame())
}


load_rin_service_areas_sf <- function (rin_service_areas) {

  counties <- cori.data::tiger_line_counties(2024)

  county_pop_centers <- cori.data::county_pop_centroids(2020) |>
    sf::st_as_sf(
      coords = c("lon", "lat"),
      crs = 4326
    )

  rin_service_areas_sf <- rin_service_areas |>
    dplyr::arrange(`rin_community`, desc(`primary_county_flag`)) |>
    dplyr::left_join(counties |>
        # dplyr::mutate(geom = geometry) |>
        sf::st_drop_geometry(), 
      by = c("geoid_co"="GEOID")
    ) |>
    dplyr::left_join(county_pop_centers |>
        dplyr::mutate(geom = geometry) |>
        sf::st_drop_geometry(), 
      by = c("geoid_co"="geoid")
    ) |>
    dplyr::mutate(
      temp_centroid = sf::st_centroid(geom),
      lon = sf::st_coordinates(temp_centroid)[, 1],
      lat = sf::st_coordinates(temp_centroid)[, 2]
    ) |>
    dplyr::select(-geom, -temp_centroid) |>
    dplyr::left_join(counties |> dplyr::select(GEOID, geometry), by = c("geoid_co"="GEOID")) |>
    sf::st_as_sf() |>
    sf::st_as_sf(crs = 4269)

  names(rin_service_areas_sf) <- snakecase::to_snake_case(names(rin_service_areas_sf))

  # Make sure to use "geometry" as the st_geometry column
  sf::st_geometry(rin_service_areas_sf) <- "geometry"

  # message(paste(class(rin_service_areas_sf), collapse = " "))
  # message(paste(names(rin_service_areas_sf), collapse = " "))

  message("Missing values in")
  cori.utils::find_missing(rin_service_areas_sf)

  message("Ready to package rin_service_areas...")

  return(rin_service_areas_sf)
}

load_zips_to_counties <- function () {
  data_source <- "https://www.huduser.gov/apps/public/uspscrosswalk/home"
  data_file <- "data/ZIP_COUNTY_122020.xlsx"

  message(paste0("TODO: Import ", data_file, "and output zip_to_county_crosswalk"))

  if (file.exists(data_file)) {
    message(paste0("Loading ", data_file))

    zip_county_122020 <- readxl::read_excel(data_file, sheet = "ZIP_COUNTY_122020")

    return(zip_county_122020[, c(1:2)])

  } else {
    message("Manually download XLSX...")
    message(paste0("From : ", data_source))
    message(paste0("To : ", data_file))
    message("... then rerun tar_make()")
  }
}

save_data_to_db_instance <- function (db_instance, schema_name, table_name, df) {

  dest <- paste0('"', schema_name, '"."', table_name, '"')
  rin_service_areas <- df

  message(class(rin_service_areas))

  if (is.null(df)) {
    stop(paste0(dest, " is NULL"), call. = FALSE)
  } else {
    con <- cori.db::connect_to_db(schema_name, dbname = db_instance)
    message(paste0("Writing data frame to tableau db as ", dest))
    # DBI::dbWriteTable(con, table_name, df, overwrite = TRUE)
    cori.db::write_db(con, table_name, rin_service_areas , overwrite = TRUE, spatial = TRUE)  # TODO: this should take optional
    #       args to set roles/permissions
    tryCatch({
      message(paste0("Set access permissions on ", dest))
      # Grant access to read_only_access group role (for testing):
      DBI::dbExecute(con, paste0("GRANT SELECT ON TABLE ", dest, " TO read_only_access;"))
      # Grant access to r_team role:
      DBI::dbExecute(con, paste0("GRANT SELECT ON TABLE ", dest, " TO r_team;"))
    }, error = function (e) {
      stop(paste0("Failed to set permissions on ", dest, "\n", e))
    })

    # df <- cori.db::read_db(con, table = table_name) # <= Error: ...
    df <- DBI::dbReadTable(con, table_name)
    DBI::dbDisconnect(con)
  }

  return(df)
}

save_data_to_package <- function (df) {
  rin_service_areas <- df
  usethis::use_data(rin_service_areas, overwrite = TRUE)
}


write_data_to_geojson <- function (df, file_path) {

  rin_service_areas <- df |>
    dplyr::filter(
      `primary_county_flag` == "Yes"
    )
  
  message(class(rin_service_areas))

  rin_service_areas |>
    sf::st_drop_geometry() |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4269) |>
    sf::st_write(
      file_path,
      append = FALSE,
      delete_dsn = TRUE
    )

  return(file_path)
}

# write_data_to_parquet <- function (dt, file_path, grouping) {
#   if (missing(grouping)) {
#     dt |>
#       write_dataset(path = file_path, format = "parquet")
#   } else {
#     dt |>
#       group_by_at(grouping) |>
#       write_dataset(path = file_path, format = "parquet")
#   }
# }

write_data_to_s3 <- function (bucket_name, file_name, file_path, s3_prefix = "dev/data/") {
  s3_key_path <- paste0(s3_prefix, file_name)
  return(cori.db::put_s3_object(bucket_name, s3_key_path, file_path))
}


#' Load RIN communities from Comms Google Sheet
#'
#' @param params Parameters containing comms_sheet_id and comms_sheet_name
#' @return Transformed tibble with rin_community, community_name, geocode_column, url
load_comms_communities <- function(params = cori.utils::get_params("global")) {

  comms_data <- googlesheets4::read_sheet(
    params$comms_sheet_id,
    sheet = params$comms_sheet_name
  )

  transformed <- comms_data |>
    dplyr::transmute(
      rin_community = `rin_community`,
      community_name = `Community`,
      geocode_column = `Primary Place`,
      url = `Link to RIN page on site`
    ) |>
    dplyr::filter(!is.na(rin_community))

  return(transformed)
}


#' Geocode RIN map data using OpenStreetMap
#'
#' @param communities_data Tibble from load_comms_communities()
#' @return Tibble with lat and long columns added
geocode_rin_map_data <- function(communities_data) {

  geocoded <- communities_data |>
    tidygeocoder::geocode(
      address = geocode_column,
      method = "osm",
      full_results = TRUE
    )

  return(geocoded)
}


#' Write RIN map data to JSON format
#'
#' @param geocoded_data Geocoded tibble from geocode_rin_map_data()
#' @param file_path Output path for JSON file
#' @return File path (for targets file tracking)
write_rin_map_json <- function(geocoded_data, file_path) {

  map_json <- geocoded_data |>
    dplyr::transmute(
      rin_community = rin_community,
      name = community_name,
      coordinates = Map(c, long, lat),
      url = dplyr::if_else(is.na(url) | url == "", "null", url)
    ) |>
    jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)

  write(map_json, file_path)

  return(file_path)
}
