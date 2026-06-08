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

  # Remove these ghost communities that were never in Monday exports; keep 2023 entries from Google sheet: RIN Community Service Areas (Updated July 2023)
  ghost_communities <- c("Grinnell", "Montgomery County", "North Iowa", "Pittsburg")

  # Create mapping of community+county → years from package data
  # Keep ALL records to preserve year assignments for all counties
  old_rin_data_with_years <- old_rin_service_areas |>
    sf::st_drop_geometry() |>
    dplyr::filter(
      !(rin_community %in% ghost_communities & year %in% c(2024, 2025))
    ) |>
    dplyr::select(geoid_co, rin_community, county, year, primary_county_flag)

  # Extract just primary counties for recovery loop (existing logic)
  old_rin_data <- old_rin_data_with_years |>
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
    ) |>
    dplyr::distinct()

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
      `geoid_co` = ifelse(`county` == 'Harrisonburg County, VA', '51660', `geoid_co`),
      `geoid_co` = ifelse(`county` == 'Independent City', '51590', `geoid_co`),  # Danville city, VA
      `geoid_co` = ifelse(`county` == 'Natchitoches County, LA', '22069', `geoid_co`)  # Natchitoches Parish, LA
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
    )

  # Build year mapping from preserved package data to preserve existing year assignments
  year_mapping <- old_rin_data_with_years |>
    dplyr::select(rin_community, county, year_preserved = year) |>
    dplyr::distinct()

  # Join with year_mapping to preserve years, then apply hardcoded assignments only for NEW communities
  areas <- areas |>
    dplyr::left_join(
      year_mapping,
      by = c("rin_community", "county")
    ) |>
    dplyr::mutate(
      # Use preserved year if available, otherwise apply hardcoded assignment
      year = ifelse(
        !is.na(year_preserved),
        year_preserved,  # Keep existing year from package
        ifelse(
          # 2026 list...
          # ... remaining network communities will need to move up to 2025 list at end-of-year
          `rin_community` %in% c( # 2025 list...
            "Helena-West Helena, AR",
            "Newport, AR"
          ),
          2025,
          ifelse(
            `rin_community` %in% c( # 2024 list...
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
            params$current_year # New community not previously in package data
          )
        )
      )
    ) |>
    dplyr::select(-year_preserved)  # Remove temp column
  
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

  # Duplicate records for current_year

  # Define communities to EXCLUDE from duplication into current_year cohort
  excluded_communities <- c(
    "Paso Robles",
    "Cedar City",
    "Central Wisconsin",
    "Platteville",
    "Wilkes County",
    # Dropped for 2026
    "Randolph",         
    "Liberal",
    # Never in network?
    # ... from: https://docs.google.com/spreadsheets/d/1Qv3nyQ4GrkhIxVs1uEOgN5tfFLtdt_MA71BquPQDGmw
    "Grinnell",      
    "Montgomery County",
    "North Iowa",
    "Pittsburg"
  )

  # Identify which community+county combinations already have year=current_year records
  existing_current_year <- areas |>
    dplyr::filter(year == params$current_year) |>
    dplyr::select(rin_community, county) |>
    dplyr::distinct() |>
    dplyr::mutate(has_current_year = TRUE)

  # Get rows that should be duplicated to current_year
  # ONLY duplicate records from the previous year (current_year - 1)
  rows_to_duplicate <- areas |>
    dplyr::filter(!rin_community %in% excluded_communities) |>
    dplyr::filter(year == params$current_year - 1) |>  # ONLY previous year
    dplyr::left_join(existing_current_year, by = c("rin_community", "county")) |>
    dplyr::filter(is.na(has_current_year)) |>  # Only rows without existing current_year records
    dplyr::select(-has_current_year)

  # Create duplicated rows with year set to current_year
  areas_current_year <- rows_to_duplicate
  areas_current_year$year <- params$current_year

  # Combine original data with duplicated rows
  areas_updated <- rbind(
    areas,
    areas_current_year
  ) |>
    dplyr::distinct() |>
    dplyr::arrange(`year`)

  # } else {
  #   message("Manually download CSV...")
  #   message(paste0("From : ", data_uri))
  #   message(paste0("To : ", data_file))
  #   message("... then rerun tar_make()")
  # }

  return(areas_updated |> as.data.frame())
}
