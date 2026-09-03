#' Build Economic Development Dashboard Link
#'
#' Generates a URL for the Rural Economic Development Dashboard
#' filtered to RIN service area counties.
#'
#' @param primary_only Logical. If TRUE, include only primary counties.
#'   If FALSE (default), include all service area counties.
#'
#' @return A character string URL for the dashboard.
#'
#' @examples
#' # Link for all service area counties
#' build_econ_dev_link()
#'
#' # Link for primary counties only
#' build_econ_dev_link(primary_only = TRUE)
#'
#' @export
build_econ_dev_link <- function(primary_only = FALSE) {

  service_areas <- cori.data.rin::rin_service_areas |>
    sf::st_drop_geometry() |>
    dplyr::filter(latest_version == "Yes")

  if (primary_only) {
    service_areas <- service_areas |>
      dplyr::filter(primary_county_flag == "Yes")
  }

  prefix <- "https://rural-economic-development-dashboard.ruralinnovation.us/?primary="
  suffix <- "&show_united_states=false"

  url <- paste0(
    prefix,
    paste0(service_areas$geoid_co, collapse = "&primary="),
    suffix
  )

  return(url)
}
