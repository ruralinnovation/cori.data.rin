
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
#'
#' @export
load_rin_service_areas <- function (params = cori.utils::get_params("global"), old_rin_service_areas) {

  # ### OLD ----
  
  # data_file <- "data/RIN Community Service Areas (Updated July 2023) [COPY] - RIN Community Lookup (DO NOT EDIT).csv"

  # if (data_uri == "https://docs.google.com/spreadsheets/d/1Qv3nyQ4GrkhIxVs1uEOgN5tfFLtdt_MA71BquPQDGmw" && file.exists(data_file)) {
    
  #   message(paste0("Loading ", data_file))

  #   rin_service_areas_csv <- readr::read_csv(data_file, col_names = TRUE)

  # ### TODO: Invert this process so that we always start with Newest data and then fill gaps (missing communities) using previous
  
  # sheet_url <- params$sheet_url
  # sheet_name <- params$sheet_name

  # # Set up authentication with a service account
  # # 1. Create a Google Cloud project
  # # 2. Enable the Google Sheets API
  # # 3. Create a service account and download JSON key
  # # 4. Place the JSON key file in a secure location

  # credentials <- Sys.getenv("GOOGLE_API_CREDENTIALS")

  # # Point to your service account key file
  # googlesheets4::gs4_auth(path = credentials)

  # sheet_id <- googlesheets4::as_sheets_id(sheet_url)

  # # # Get the sheet names
  # all_sheet_names <- googlesheets4::sheet_names(sheet_id)

  # stopifnot(sheet_name %in% all_sheet_names)

  # sheet_data <- googlesheets4::read_sheet(sheet_url, sheet_name)

  # old_rin_data <- sheet_data |>
  #   dplyr::mutate(
  #     # `geoid_co` = `geoid_co`,
  #     # `rin_community` = `rin_community`,
  #     `primary_county` = paste0(`primary_county_name`, " County, ", state_abbr)
  #   ) |>
  #   dplyr::select(
  #     `geoid_co`,
  #     `rin_community`,
  #     `primary_county`
  #   )
  
  ### NEW ----

  ### RIN data downloaded as of 2025-05-06
  #### Monday board: https://ruralinnovation-group.monday.com/boards/6951894369
  #### Monday group: Current
  #### When downloading new data, run:
  # usethis::use_build_ignore(params$monday_network_communities_file_name, escape = TRUE)

  old_rin_data <- old_rin_service_areas |>
    sf::st_drop_geometry() |>
    dplyr::filter(
      `primary_county_flag` == "Yes"
    ) |>
    dplyr::mutate(
      `primary_county` = `county`
    ) |> 
    dplyr::select(
      `geoid_co`,
      `rin_community`,
      `primary_county`
    )

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
  county_geoid_name_lookup <- get_county_geoid_name_lookup(params$current_year)

  ## read in
  rin <- readxl::read_excel(paste0(data_dir, "/", params$monday_network_communities_file_name), skip = 2)
  
  names(rin) <- snakecase::to_snake_case(names(rin))

  rin_only <- rin |> 
    dplyr::filter(!is.na(`name`)) |> 
    dplyr::filter(`name` != "Subitems")

  ## Check previous data set for additional records not contained in latest Monday extract
  for (r in c(1:nrow(old_rin_data))) {
    community <- old_rin_data[r, ]

    if (community$rin_community %in% rin_only$name) {
      print(paste0(r, ": found ", community$rin_community))
    } else {
      print(paste0(r, ": add ", community$rin_community))
      
      rin_only[(nrow(rin_only) + 1), ] <- data.frame(
        matrix(
          rep(NA, ncol(rin_only)), 
          nrow = 1,
          dimnames = list(NULL, names(rin_only))
        )
      ) |>
        dplyr::mutate(
          `name` = community$rin_community,
          `primary_county` = community$primary_county
        )

      print(rin_only[(nrow(rin_only)), ])
    }
  }

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
      `geoid_co` = ifelse(`county` == 'Harrisonburg County, VA', '51660', `geoid_co`)
    ) |> 
    dplyr::mutate(
      primary_county_flag = "No",
      data_run_date = Sys.Date()
    ) |> 
    dplyr::select(
      `geoid_co`,
      `rin_community` = `name`,
      `county`,
      `primary_county_flag`,
      `data_run_date`
  ### THIS IS A BUG! Not all RIN communities in this set are valid for the specified year...
  #   ) |>
  #   dplyr::mutate(
  #       `year` = params$current_year
  #   )
  
  ## ... so, we're going to hand code the list for each year, based on data gathered during
  ## the compilation of the Impact dashboard dataset; see https://docs.google.com/spreadsheets/d/1R_UccunBsg6TiKD_lAsj37wI5_1H4TKCCd25q914p9U/edit?gid=1698657911#gid=1698657911
  ###
    ) |>
    dplyr::mutate(
      `year` = ifelse(
        `rin_community` %in% c( # 2025 list...
          "Helena-West Helena, AR",
          "Newport, AR"
        ),
        2025,
        ifelse(
          `rin_community` %in% c( # 2024 list ... remaining network communities will need to move up to 2025 list at end-of-year
            "Ada",
            "Aberdeen",
            "The Berkshires",
            "Cape Girardeau",
            "Central Wisconsin",
            "Chambers County",
            "Cochise County",
            "The Dalles",
            "Durango",
            "Eastern Kentucky",
            "Emporia",
            "Greenfield",
            "Independence",
            "Indiana County",
            "Kirksville",
            "Manitowoc County",
            "Marquette",
            "Nacogdoches",
            "NEK",
            "Norfolk",
            "Paducah",
            "Pine Bluff",
            "Platteville",
            "Portsmouth",
            "Pryor Creek",
            "Randolph",
            "Red Wing",
            "Rutland",
            "Seward County, NE",
            "Shenandoah Valley",
            "Springfield",
            "Taos",
            "Traverse City",
            "Waterville",
            "Wilkes County",
            "Wilson",
            "Windham County"
          ),
          2024,
          2023 # Fall back to 2023 for non-recent communities
        )
      )
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

  # } else {
  #   message("Manually download CSV...")
  #   message(paste0("From : ", data_uri))
  #   message(paste0("To : ", data_file))
  #   message("... then rerun tar_make()")
  # }

  return(areas |> as.data.frame())
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
      centroid = sf::st_centroid(geom),
      lon = sf::st_coordinates(centroid)[, 1],
      lat = sf::st_coordinates(centroid)[, 2]
    ) |>
    dplyr::select(-geom) |>
    dplyr::left_join(counties |> dplyr::select(GEOID, geometry), by = c("geoid_co"="GEOID")) |>
    sf::st_as_sf() |>
    sf::st_as_sf(crs = 4269)

  names(rin_service_areas_sf) <- snakecase::to_snake_case(names(rin_service_areas_sf))

  # Make sure to use "geometry" as the st_geometry column
  st_geometry(rin_service_areas_sf) <- "geometry"

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

save_data_to_package <- function (df) {
  rin_service_areas <- df
  usethis::use_data(rin_service_areas, overwrite = TRUE)
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


write_data_to_geojson <- function (df, file_path) {

  rin_service_areas <- df |>
    dplyr::filter(
      `primary_county_flag` == "Yes"
    )
  
  # Use the centroid as the geometry for json

  sf::st_geometry(rin_service_areas) <- "centroid"

  message(class(rin_service_areas))

  rin_service_areas |>
    dplyr::select(-c(geometry, centroid)) |>
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

write_data_to_s3 <- function (bucket_name, file_name, file_path) {
  s3_key <- paste0("examples/cori.data.rin/", file_name)
  return(cori.db::put_s3_object(bucket_name, s3_key, file_path))
}
