library(dplyr)
library(tidyr)
library(here)
library(jsonlite)
library(tidygeocoder)

i_am("R/create_map_dta.R")

# Commns tracks RIN community updates in the Google sheet [RIN Communties on Website](https://docs.google.com/spreadsheets/d/1fDGclR7GqncaLdkEipOJLLwDn3h4gYHVRcJtP_K1wcU)
comms_sheet_id <- "1fDGclR7GqncaLdkEipOJLLwDn3h4gYHVRcJtP_K1wcU"
comms_csv_path <- here("data/[RIN Communties on Website - Current_Communities.csv")
readr::write_csv(googlesheets4::read_sheet(comms_sheet_id, sheet = "Current_Communities"), comms_csv_path)

# This list is maintained in the Google sheet [RIN Map Dataset](https://docs.google.com/spreadsheets/d/1xNuoDexYblr2WGTsTUD5M3lbwWrLNhpmUqp0yXJyGrM)
mda_sheet_id <- "1xNuoDexYblr2WGTsTUD5M3lbwWrLNhpmUqp0yXJyGrM"
mda_csv_path <- here("data/RIN Map Dataset - rin_communities.csv")
readr::write_csv(googlesheets4::read_sheet(mda_sheet_id, sheet = "rin_communities"), mda_csv_path)

rin_dta <- readr::read_csv(here("data/RIN Map Dataset - rin_communities.csv"))

geocoded_rin_dta <- rin_dta |>
  geocode(address = geocode_column, method = "osm", full_results = TRUE)

map_json <- geocoded_rin_dta |>
  transmute(
    rin_community = rin_community,
    name = community_name,
    coordinates = Map(c, long, lat),
    url = if_else(is.na(url) | url == "", "null", url)
  ) |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(map_json, here("data/rin_map.json"))
