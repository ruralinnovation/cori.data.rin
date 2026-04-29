library(dplyr)
library(tidyr)
library(here)
library(jsonlite)
library(tidygeocoder)

i_am("R/create_map_dta.R")

# This list is maintained in the Google sheet [RIN Map Dataset](https://docs.google.com/spreadsheets/d/1xNuoDexYblr2WGTsTUD5M3lbwWrLNhpmUqp0yXJyGrM)
sheet_id <- "1xNuoDexYblr2WGTsTUD5M3lbwWrLNhpmUqp0yXJyGrM"
csv_path <- here("data/RIN Map Dataset - rin_communities.csv")
readr::write_csv(googlesheets4::read_sheet(sheet_id, sheet = "rin_communities"), csv_path)

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
