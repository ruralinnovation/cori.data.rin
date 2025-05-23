
# Since geocoding results come back as GeoJSON objects, need to define function that can turn array of GeoJSON objects (i.e. the "features" property of a FeatureCollection) into an sf data.fame
geojson_array_to_sf <- function (geo_json_list) { # <- geo_json_list: array of list representation of GeoJSON
  sf::st_sf(geo_json_list |>
    lapply(function (x) {
      geojson_properties <- as.data.frame(x[["properties"]])
      # creates row of class "sf tbl_df tbl data.frame" with "geometry" column, but ...
      # read_sf can *only* operate on n the "geometry" property of the GeoJSON feature
      geojson_table <- sf::read_sf(jsonlite::toJSON(x[["geometry"]], auto_unbox = TRUE))
      # enriches geojson_properties data.frame with "geometry" column
      sf::st_geometry(geojson_properties) <- geojson_table$geometry
      geojson_properties
    }) |>
    dplyr::bind_rows()
  )
}

get_test_address_df <- function () {
  test_record <- data.frame(
    ref_id = 10857503,
    cleaned_address = "115 Cold Spg",
    cleaned_city = "Appalachia",
    cleaned_state_abbr = "VA",
    zip = "24216"
  )
}

geocode_address_df <- function(geocoder_cache_filename, state_id_crosswalk, address_df, address_col, city_col, state_col, zip_code_col) {
  library(dplyr)
  library(httr)

  api_key <- Sys.getenv("GEOCODE_EARTH_KEY")

  geocoded_address_list <- list()

  # head(address_df)

  for (r in c(1:nrow(address_df))) {

    address_record <- address_df[r,]

    # print(address_record)

    if (!file.exists(paste0("./data/", geocoder_cache_filename))) {
      geocoder_hits <- list()
    } else {
      geocoder_hits <- readRDS(paste0("./data/", geocoder_cache_filename))
      Sys.sleep(0.33)
    }

    print(paste0("Length of geocoder hits: ", length(geocoder_hits)))

    if (!is.na(address_record[[state_col]]) && !is.na(address_record[zip_code_col])) {

      input_address <- address_record[[address_col]]
      street_address <- stringr::str_replace_all(input_address, " ", "+")
      city_town <- stringr::str_replace_all(address_record[[city_col]], " ", "+")
      st_abbr <- toupper(address_record[[state_col]])
      zip_code <- stringr::str_pad(as.character(address_record[[zip_code_col]]), 5, pad = "0")

      geocoder_key <- paste0(street_address,city_town,st_abbr,zip_code)

      if(!is.null(geocoder_hits) && geocoder_key %in% names(geocoder_hits)) {

        print(paste0("Found geocoder hit for: ", geocoder_key))

        address_point <- geocoder_hits[[geocoder_key]]
        geocoded_address_list[[length(geocoded_address_list)+1]] <- address_df[r,] |> cbind(address_point)

      } else {
        
        geocode_url <- sprintf("https://api.geocode.earth/v1/search?api_key=%s&text=%s,+%s,+%s+%s", api_key, street_address, city_town, st_abbr, zip_code)
        print(geocode_url)

        geocode_req <- httr::GET(geocode_url)
        address_code <- httr::status_code(geocode_req) # 200 => good response, *but* not necessarily perfect match

        if (address_code != 200) {
          # Error
          print("ERROR:")
          print(httr::content(geocode_req))

          print(paste0("Could not geocode record: ", address_df[r, c(1:8)]))
          geocoded_address_list[[length(geocoded_address_list)+1]] <- address_df[r,]

        } else {

          # print("Geocode Earth exact match geocode response code is:")
          # print(address_code)

          geocode_res <- httr::content(geocode_req)

          # print("Geocode Earth exact match geocode response is:")
          # print(geocode_res)

          # print("start first check")

          if (!is.null(geocode_res$features[[1]])  
            && "country" %in% names(geocode_res$features[[1]]$properties) 
            && geocode_res$features[[1]]$properties$country == "United States" 
            && geocode_res$features[[1]]$properties$region_a == st_abbr
          ) {
            # First check is good
            print("First check is good")

            print(geocode_res$features[[1]]$properties$accuracy)

            print(input_address)
            print(tolower(geocode_res$features[[1]]$properties$name))

            if (#geocode_res$features[[1]]$properties$match_type == "exact" &&
                (geocode_res$features[[1]]$properties$accuracy == "point" || geocode_res$features[[1]]$properties$accuracy == "centroid")
                ## Match first 4 characters of user-input address to geocoded address
                # && grepl(substring(
                #         stringr::str_match(input_address, "[0-9A-Za-z|\\.]+\\s+[0-9A-Za-z|\\.]+\\s*[0-9A-Za-z|\\.]*"),
                #         1, 4),
                #     tolower(geocode_res$features[[1]]$properties$name),
                #     ignore.case = TRUE
                #   )
            ) {
              # Close enough check is good

              # print("Geocode exact match:")
              address_point <- geojson_array_to_sf(geocode_res$features)[1,]
              geocoded_address_list[[length(geocoded_address_list)+1]] <- address_df[r,] |> cbind(address_point)
              
              print(paste0("Saving geocoder hit for: ", geocoder_key))
              geocoder_hits[[geocoder_key]] <- address_point
              saveRDS(geocoder_hits, paste0("./data/", geocoder_cache_filename))
              geocoder_hits <- readRDS(paste0("./data/", geocoder_cache_filename))

              print(paste0("Current length of geocoder hits: ", length(geocoder_hits)))
              
              Sys.sleep(0.33) 

            } else {
              # Second check is bad, now match focal point on st_abbr + zip_code, then
              # fuzzy (autocomplete) match on street address with proximity to focal point

              # Get state focal point
              # Example for st_abbr = "VA"
              # https://api.geocode.earth/v1/search?api_key={{api_key}}&text=text=VA+24216
              # "geometry": {
              #   "type": "Point",
              #   "coordinates": [
              #     -82.799398,
              #     36.948303
              #   ]
              # }
              # st_abbr <- "VA"
              # zip_code <- "24216"

              state <- state_id_crosswalk |> dplyr::filter(`state_abbr` == st_abbr)

              state_point_url <- sprintf("https://api.geocode.earth/v1/search?api_key=%s&text=%s+%s", api_key, st_abbr, zip_code)
              print(state_point_url)

              state_point_request <- httr::GET(state_point_url)
              state_point_request_code <-httr::status_code(state_point_request)

              if (state_point_request_code != 200) {
                # Error
                print("ERROR:")
                print(httr::content(state_point_request))

                print(paste0("Could not geocode record: ", address_df[r, c(1:8)]))
                geocoded_address_list[[length(geocoded_address_list)+1]] <- address_df[r,]

              } else {

                # print("Geocode response code is:")
                # print(state_point_request_code)

                state_point_data <- httr::content(state_point_request)

                # print("Geocode response is:")
                # print(state_point_data)

                if (!is.null(state_point_data$features[[1]]) &&
                    state_point_data$features[[1]]$properties$country == "United States" &&
                    state_point_data$features[[1]]$properties$region == state$state_name
                ) {

                  if (state_point_data$features[[1]]$geometry$type == "Point") {
                    state_point_longitude <- state_point_data$features[[1]]$geometry$coordinates[[1]]
                    state_point_latitude <- state_point_data$features[[1]]$geometry$coordinates[[2]]
                  }
                }

                # Do fuzzy geocoding (user-input) with respect to state focal point
                # https://api.geocode.earth/v1/autocomplete?api_key={{api_key}}&focus.point.lat=36.948303&focus.point.lon=-82.799398&text=115+Cold+Spring
                # geocode_req <- httr::GET(sprintf("https://api.geocode.earth/v1/search?api_key=%s&text=%s,+%s,+%s+%s", api_key, street_address, city_town, st_abbr, zip_code))
                
                address_next_url <- sprintf("https://api.geocode.earth/v1/autocomplete?api_key=%s&focus.point.lat=%s&focus.point.lon=%s&text=%s,+%s,+%s+%s", api_key, state_point_latitude, state_point_longitude, street_address, city_town, st_abbr, zip_code)
                print(address_next_url)

                address_next <- httr::GET(address_next_url)

                # print("Geocode Earth fuzzy (autocomplete) geocode response code is:")
                # print(address_next_code)

                address_next_result <- httr::content(address_next)

                # print("Geocode Earth fuzzy (autocomplete) geocode response is:")
                # print(address_next_result)
                # print(length(address_next_result$features))

                print("last check +1")

                print(length(address_next_result$features))
                print(length(address_next_result$features) > 0)

                if (length(address_next_result$features) > 0) {
                  print(address_next_result$features[[1]]$properties$name)
                  print((address_next_result$features[[1]]$properties$accuracy == "centroid" || address_next_result$features[[1]]$properties$accuracy == "point"))
                  print(address_next_result$features[[1]]$properties$region_a == st_abbr)
                  print(address_next_result$features[[1]]$properties$postalcode == zip_code)
                  print(grepl(substring(
                          stringr::str_match(input_address, "[0-9A-Za-z|\\.]+\\s+[0-9A-Za-z|\\.]+\\s*[0-9A-Za-z|\\.]*"),
                          1, 4),
                      address_next_result$features[[1]]$properties$name,

                      ignore.case = TRUE
                  ))
                }
                
                if (length(address_next_result$features) > 0
                  && (address_next_result$features[[1]]$properties$accuracy == "centroid" || address_next_result$features[[1]]$properties$accuracy == "point")
                  && address_next_result$features[[1]]$properties$region_a == st_abbr
                  && address_next_result$features[[1]]$properties$postalcode == zip_code
                    # Match first 4 characters of user-input address to geocoded address
                  && grepl(substring(
                            stringr::str_match(input_address, "[0-9A-Za-z|\\.]+\\s+[0-9A-Za-z|\\.]+\\s*[0-9A-Za-z|\\.]*"),
                            1, 4),
                        address_next_result$features[[1]]$properties$name,

                        ignore.case = TRUE
                    )
                ) {
                  # Match check is good

                  print("Get geojson")

                  address_point <- geojson_array_to_sf(address_next_result$features)[1,]
                  geocoded_address_list[[length(geocoded_address_list)+1]] <- address_df[r,] |> cbind(address_point)
                  
                  print(paste0("Saving geocoder hit for: ", geocoder_key))
                  geocoder_hits[[geocoder_key]] <- address_point
                  saveRDS(geocoder_hits, paste0("./data/", geocoder_cache_filename))
                  geocoder_hits <- readRDS(paste0("./data/", geocoder_cache_filename))

                  print(paste0("Current length of geocoder hits: ", length(geocoder_hits)))
                  
                  Sys.sleep(0.33) 

                } else {

                  print(paste0("Could not geocode record: ", address_df[r, c(1:8)]))
                  geocoded_address_list[[length(geocoded_address_list)+1]] <- address_df[r,]
      
                }
              }

            }
          } else {

            state <- state_id_crosswalk |> dplyr::filter(`state_abbr` == st_abbr)

            state_point_request <- httr::GET(sprintf("https://api.geocode.earth/v1/search?api_key=%s&text=%s+%s", api_key, st_abbr, zip_code))
            state_point_request_code <-httr::status_code(state_point_request)

            if (state_point_request_code != 200) {
              # Error
              print("ERROR:")
              print(httr::content(state_point_request))

              print(paste0("Could not geocode record: ", address_df[r, c(1:8)]))
              geocoded_address_list[[length(geocoded_address_list)+1]] <- address_df[r,]

            } else {

              # print("2) Geocode response code is:")
              # print(state_point_request_code)

              state_point_data <- httr::content(state_point_request)

              # print("2) Geocode response is:")
              # print(state_point_data)

              if (!is.null(state_point_data$features[[1]]) &&
                  state_point_data$features[[1]]$properties$country == "United States" &&
                  state_point_data$features[[1]]$properties$region == state$state_name
              ) {

                if (state_point_data$features[[1]]$geometry$type == "Point") {
                  state_point_longitude <- state_point_data$features[[1]]$geometry$coordinates[[1]]
                  state_point_latitude <- state_point_data$features[[1]]$geometry$coordinates[[2]]
                }
              }

              # Do fuzzy geocoding (user-input) with respect to state focal point
              # https://api.geocode.earth/v1/autocomplete?api_key={{api_key}}&focus.point.lat=36.948303&focus.point.lon=-82.799398&text=115+Cold+Spring
              # geocode_req <- httr::GET(sprintf("https://api.geocode.earth/v1/search?api_key=%s&text=%s,+%s,+%s+%s", api_key, street_address, city_town, st_abbr, zip_code))
              
              address_next_url <- sprintf("https://api.geocode.earth/v1/autocomplete?api_key=%s&focus.point.lat=%s&focus.point.lon=%s&text=%s,+%s,+%s+%s", api_key, state_point_latitude, state_point_longitude, street_address, city_town, st_abbr, zip_code)
              print(address_next_url)

              address_next <- httr::GET(address_next_url)

              # print("2) Geocode Earth fuzzy (autocomplete) geocode response code is:")
              # print(address_next_code)

              address_next_result <- httr::content(address_next)

              # print("2) Geocode Earth fuzzy (autocomplete) geocode response is:")
              # print(length(address_next_result))
              # print(address_next_result)

              # print("2) last check")

              if (length(address_next_result$features) > 0) {
                print(address_next_result$features[[1]]$properties$name)
                print((address_next_result$features[[1]]$properties$accuracy == "centroid" || address_next_result$features[[1]]$properties$accuracy == "point"))
                print(address_next_result$features[[1]]$properties$region_a == st_abbr)
                print(address_next_result$features[[1]]$properties$postalcode == zip_code)
                print(grepl(substring(
                  stringr::str_match(input_address, "[0-9A-Za-z|\\.]+\\s+[0-9A-Za-z|\\.]+\\s*[0-9A-Za-z|\\.]*"),
                    1, 4),
                    address_next_result$features[[1]]$properties$name,
                    ignore.case = TRUE
                ))
              }
              
              if (length(address_next_result$features) > 0
                && (address_next_result$features[[1]]$properties$accuracy == "centroid" || address_next_result$features[[1]]$properties$accuracy == "point")
                && address_next_result$features[[1]]$properties$region_a == st_abbr
                && address_next_result$features[[1]]$properties$postalcode == zip_code
                  # Match first 4 characters of user-input address to geocoded address
                && grepl(substring(
                          stringr::str_match(input_address, "[0-9A-Za-z|\\.]+\\s+[0-9A-Za-z|\\.]+\\s*[0-9A-Za-z|\\.]*"),
                          1, 4),
                      address_next_result$features[[1]]$properties$name,
                      ignore.case = TRUE
                  )
              ) {
                # Match check is good

                # print("2) Get geojson")

                address_point <- geojson_array_to_sf(address_next_result$features)[1,]
                geocoded_address_list[[length(geocoded_address_list)+1]] <- address_df[r,] |> cbind(address_point)

                print(paste0("Saving geocoder hit for: ", geocoder_key))
                geocoder_hits[[geocoder_key]] <- address_point
                saveRDS(geocoder_hits, paste0("./data/", geocoder_cache_filename))
                geocoder_hits <- readRDS(paste0("./data/", geocoder_cache_filename))

                print(paste0("Current length of geocoder hits:  ", length(geocoder_hits)))
                
                Sys.sleep(0.33) 

              } else {
                print(paste0("Could not geocode record: ", address_df[r, c(1:8)]))
                geocoded_address_list[[length(geocoded_address_list)+1]] <- address_df[r,]
              }
            }
          }
        }
        
      }
    } else {
      print(paste0("Could not geocode record: ", address_df[r, c(1:8)]))
      geocoded_address_list[[length(geocoded_address_list)+1]] <- address_df[r,]
    }
  }

  print(paste0("Length of geocoded address list: ", length(geocoded_address_list)))

  return(dplyr::bind_rows(geocoded_address_list) |>
    dplyr::mutate(
      centroid = sf::st_centroid(geometry),
      lon = sf::st_coordinates(centroid)[, 1],
      lat = sf::st_coordinates(centroid)[, 2]
    )
  )

}


load_rin_service_areas <- function () {

  ### OLD ----
  # data_file <- "data/RIN Community Service Areas (Updated July 2023) [COPY] - RIN Community Lookup (DO NOT EDIT).csv"

  # if (data_uri == "https://docs.google.com/spreadsheets/d/1Qv3nyQ4GrkhIxVs1uEOgN5tfFLtdt_MA71BquPQDGmw" && file.exists(data_file)) {
    
    # message(paste0("Loading ", data_file))

    # rin_service_areas_csv <- readr::read_csv(data_file, col_names = TRUE)
  
  ### NEW ----
  ### RIN data downloaded as of 2025-05-06
  #### Monday board: https://ruralinnovation-group.monday.com/boards/6951894369
  #### Monday group: Current
  #### When downloading new data, run:
  # usethis::use_build_ignore("data/Network_Communities_Current_1746543710.xlsx", escape = TRUE)

    global_params <- cori.utils::get_params("global")

    stopifnot(file.exists("data"))

    data_dir <- "./data"

    ### functions ------
    get_county_geoid_name_lookup <- function(year = 2024) {
      
      counties <- cori.data::tiger_line_counties(year) |>
      sf::st_drop_geometry()
      
      states <- cori.data::tiger_line_states(year) |> 
        sf::st_drop_geometry()
      
      # state_names <- states %>%
      #   select(GEOID, STUSPS)
      
      county_geoid_name_lookup <- counties |>
        left_join(
          states,
          by = c("STATEFP" = "GEOID")
        ) %>%
        mutate(
          name_co = paste0(NAMELSAD, ", ", STUSPS)
        ) |>
        select(geoid_co = GEOID, name_co) |>
        distinct()
      
      return(county_geoid_name_lookup)
      
    }

    # Load a county geoid_co name_co lookup
    county_geoid_name_lookup <- get_county_geoid_name_lookup(global_params$current_year)

    ## read in
    rin <- readxl::read_excel(paste0(data_dir, "/", global_params$monday_network_communities_file_name), skip = 2)

    rin_only <- rin |> 
      dplyr::filter(!is.na(Name)) |> 
      dplyr::filter(Name != "Subitems")


    rin_places <- rin_only %>% 
      select(
        Name,
        place = `Primary Place`
      )

    rin_primary_co <- rin_only |>
      select(
        Name,
        county = `Primary County`
      )

      rin_service_areas <- rin_only %>% 
        select(
          `Name`,
          county = `Other Counties`
        ) %>% 
        filter(
          !is.na(county)
        ) %>% 
        separate_rows(county, sep = "(?<=,\\s[A-Z]{2}),\\s*") %>% 
        bind_rows(rin_primary_co) %>% 
        left_join(county_geoid_name_lookup, by = c('county' = 'name_co')) %>% 
        mutate(
          geoid_co = ifelse(county == 'Harrisonburg County, VA', '51660',geoid_co)
        ) %>% 
        ## TODO: Something is going on from this mutate on, that is causing a double counting of primary county...
        mutate(
          # primary_county_flag = ifelse(county %in% rin_primary_co$county, 'Yes', 'No'),
          primary_county_flag = "No",
          data_run_date = Sys.Date()
          ) %>% 
        select(
          geoid_co,
          rin_community = Name,
          county,
          primary_county_flag,
          data_run_date
        )
      
      check_primary_county <- function (county, name, rin_primary_counties) {
        
          primary_county <- (rin_primary_counties |> dplyr::filter(Name == name))$county
        
          if (length(primary_county) > 0) {
            if (county %in% primary_county) return("Yes")
            else return("No")
          } else {
            return("No")
          }
        }
      
      for (r in c(1:nrow(rin_service_areas))) {
        name <- rin_service_areas[r, ]$rin_community
        county <- rin_service_areas[r, ]$county
      
        rin_service_areas[r, ]$primary_county_flag <- check_primary_county(county, name, rin_primary_co)
      }
    
     return(rin_service_areas |> as.data.frame())

    # } else {
    #   message("Manually download CSV...")
    #   message(paste0("From : ", data_uri))
    #   message(paste0("To : ", data_file))
    #   message("... then rerun tar_make()")
    # }
}


load_rin_service_areas_sf <- function (rin_service_areas) {

  counties <- cori.data::tiger_line_counties(2024)

  rin_service_areas_sf <- rin_service_areas |>
    dplyr::left_join(counties |> dplyr::mutate(geom = geometry) |> sf::st_drop_geometry(), by = c("geoid_co"="GEOID")) |>
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

  rin_service_areas <- df

  message(class(rin_service_areas))

  rin_service_areas |>
    dplyr::select(-c(geometry, centroid)) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4269) |>
    sf::st_write(
      file_path,
      append = FALSE
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
