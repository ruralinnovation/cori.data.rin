#' Calculate Rural RIN Metrics
#'
#' Aggregates various metrics (population, employment, etc.) across rural
#' RIN service area counties.
#'
#' @param metric Character. The metric to aggregate. One of:
#'   - `"population"` (default): Total population from Census PEP
#'   - `"employment"`: Total employment from BLS QCEW
#'   Additional metrics can be added as data sources become available.
#' @param primary_only Logical. If TRUE, include only primary counties.
#'   If FALSE (default), include all service area counties.
#' @param year Integer. Year for the metric data. If NULL (default),
#'   uses the most recent available year.
#' @param rural_only Logical. If TRUE (default), include only rural counties.
#'   If FALSE, include all counties regardless of rural status.
#'
#' @return A tibble with columns:
#'   - `metric`: Name of the aggregated metric
#'   - `year`: Data year
#'   - `value`: Aggregated value
#'
#' @details
#' Rural status is determined by joining with `ruraldefinitions::cbsa_2023`.
#' Counties are classified as rural if they are outside a CBSA (metropolitan

#' or micropolitan statistical area).
#'
#' @examples
#' # Total population of rural RIN service areas
#' calculate_rural_rin_metrics()
#'
#' # Employment in rural primary counties only
#' calculate_rural_rin_metrics(metric = "employment", primary_only = TRUE)
#'
#' # Population for a specific year
#' calculate_rural_rin_metrics(metric = "population", year = 2020)
#'
#' @export
calculate_rural_rin_metrics <- function(
    metric = c("population", "employment"),
    primary_only = FALSE,
    year = NULL,
    rural_only = TRUE
) {

  metric <- match.arg(metric)

 # Build the base RIN county set
  rin_counties <- cori.data.rin::rin_service_areas |>
    sf::st_drop_geometry() |>
    dplyr::filter(latest_version == "Yes")

  if (primary_only) {
    rin_counties <- rin_counties |>
      dplyr::filter(primary_county_flag == "Yes")
  }

  # Join rural definitions
  rin_counties <- rin_counties |>
    dplyr::left_join(
      ruraldefinitions::cbsa_2023 |> dplyr::select(geoid, is_rural),
      by = c("geoid_co" = "geoid")
    ) |>
    dplyr::filter(!is.na(is_rural))

  if (rural_only) {
    rin_counties <- rin_counties |>
      dplyr::filter(is_rural == "Rural")
  }

  rin_counties <- rin_counties |>
    dplyr::select(
      geoid = geoid_co,
      rin_community,
      primary_county_flag,
      is_rural
    )

  # Fetch metric data based on type
  metric_data <- .fetch_metric_data(metric, year)

  # Join and aggregate
  result <- rin_counties |>
    dplyr::left_join(metric_data$data, by = "geoid") |>
    dplyr::summarize(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    )

  # Return structured result
  dplyr::tibble(
    metric = metric,
    year = metric_data$year,
    n_counties = nrow(rin_counties),
    primary_only = primary_only,
    rural_only = rural_only,
    value = result$value
  )
}


#' Fetch metric data from appropriate source
#'
#' @param metric Character. Metric name.
#' @param year Integer or NULL. Target year.
#'
#' @return A list with `data` (tibble with geoid/value) and `year` (integer).
#'
#' @keywords internal
.fetch_metric_data <- function(metric, year) {

 switch(metric,

    population = {
      # Determine year: use provided or most recent available
      if (is.null(year)) {
        year <- cori.data.pep::get_population("nation", variables = "population") |>
          dplyr::pull(year) |>
          max()
      }
      data <- cori.data.pep::get_population(year = year, variables = "population") |>
        dplyr::select(geoid, value)
      list(data = data, year = year)
    },

    employment = {
      # Determine year: use provided or most recent available
      if (is.null(year)) {
        year <- cori.data.qcew::get_employment(geography = "county") |>
          dplyr::pull(year) |>
          max()
      }
      data <- cori.data.qcew::get_employment(geography = "county", years = year) |>
        dplyr::select(geoid, value)
      list(data = data, year = year)
    },

    stop("Unknown metric: ", metric, ". Available: population, employment")
  )
}


#' Get Rural RIN Counties
#'
#' Returns the set of rural RIN service area counties used for metric calculations.
#' Useful for inspecting the county set before aggregation.
#'
#' @inheritParams calculate_rural_rin_metrics
#'
#' @return A tibble with columns: `geoid`, `rin_community`, `primary_county_flag`, `is_rural`.
#'
#' @examples
#' # All rural RIN counties
#' get_rural_rin_counties()
#'
#' # Primary counties only
#' get_rural_rin_counties(primary_only = TRUE)
#'
#' @export
get_rural_rin_counties <- function(primary_only = FALSE, rural_only = TRUE) {

  rin_counties <- cori.data.rin::rin_service_areas |>
    sf::st_drop_geometry() |>
    dplyr::filter(latest_version == "Yes")

  if (primary_only) {
    rin_counties <- rin_counties |>
      dplyr::filter(primary_county_flag == "Yes")
  }

  rin_counties <- rin_counties |>
    dplyr::left_join(
      ruraldefinitions::cbsa_2023 |> dplyr::select(geoid, is_rural),
      by = c("geoid_co" = "geoid")
    ) |>
    dplyr::filter(!is.na(is_rural))

  if (rural_only) {
    rin_counties <- rin_counties |>
      dplyr::filter(is_rural == "Rural")
  }

  rin_counties |>
    dplyr::select(
      geoid = geoid_co,
      rin_community,
      primary_county_flag,
      is_rural
    )
}
