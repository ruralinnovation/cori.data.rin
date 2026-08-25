#' RIN Service Areas (County level)
#'
#' @name rin_service_areas
#' 
#' @format ## `rin_service_areas`
#' A data.frame with at least these 5 columns:
#' \describe{
#'   \item{geoid_co}{County geoid (STATEFP + COUNTYFP)}
#'   \item{rin_community}{CORI name for the RIN community}
#'   \item{county}{County name}
#'   \item{primary_county_flag}{"Yes"|"No"}
#'   \item{data_run_date}{Date of data ingestion from Monday}
#'   ...
#' }
#' @source <https://docs.google.com/spreadsheets/d/1Qv3nyQ4GrkhIxVs1uEOgN5tfFLtdt_MA71BquPQDGmw>
#' @source <https://ruralinnovation-group.monday.com/boards/6951894369>
"rin_service_areas"

# Column names referenced via NSE (dplyr verbs, tidygeocoder) that R CMD check
# cannot statically resolve as data.frame columns rather than global variables.
utils::globalVariables(c(
  "Community (NAME ON RIN MAP)",
  "community_name",
  "connect_boards2__1",
  "connect_boards24__1",
  "data_run_date",
  "geocode_column",
  "GEOID",
  "geoid_co",
  "geom",
  "geometry",
  "item_id",
  "lat",
  "latest_version",
  "Link to RIN page on site",
  "long",
  "monday_id",
  "name_co",
  "NAMELSAD",
  "other_counties",
  "Primary Place",
  "primary_county",
  "primary_county_flag",
  "rin_community",
  "STUSPS",
  "temp_centroid",
  "year"
))
