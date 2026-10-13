#' RIN Service Areas (County level) -- moved to S3
#'
#' This name no longer resolves to anything. `cori.data.rin::rin_service_areas`
#' raises an error, on plain assignment as much as on use, because an installed
#' copy of the data goes stale as soon as the pipeline publishes again. The
#' dataset it used to return is published to S3 and read with
#' [get_rin_service_areas()] or [rin_service_areas_tbl()], so consumers get the
#' current vintage rather than whatever snapshot their installed copy was built
#' from. The same pointer is printed on every `library(cori.data.rin)`.
#'
#' The dataset itself is unchanged and still authoritative, bundled as
#' `data/rin_service_areas.rda` and rewritten by the targets pipeline on every
#' run, which appends a complete new snapshot of the current set each time. If
#' you specifically want that bundled snapshot rather than the published
#' vintage, read it with
#' `utils::data("rin_service_areas", package = "cori.data.rin")`.
#'
#' [get_rin_service_areas()] returns the columns this name once did:
#' \describe{
#'   \item{geoid_co}{County geoid (STATEFP + COUNTYFP)}
#'   \item{rin_community}{CORI name for the RIN community}
#'   \item{county}{County name}
#'   \item{primary_county_flag}{"Yes"|"No"}
#'   \item{data_run_date}{Date of data ingestion from Monday}
#'   \item{year_joined}{Integer year the community joined RIN ("Year Joined RIN" on the Monday board); NA if unknown}
#'   \item{latest_version}{"Yes" on current records}
#'   ...
#' }
#'
#' @source <https://docs.google.com/spreadsheets/d/1Qv3nyQ4GrkhIxVs1uEOgN5tfFLtdt_MA71BquPQDGmw>
#' @source <https://ruralinnovation-group.monday.com/boards/6951894369>
#' @name rin_service_areas
NULL


.rin_service_areas_moved_msg <- paste(
  "`cori.data.rin::rin_service_areas` is no longer available as a dataset,",
  "because an installed copy goes stale as soon as the pipeline publishes",
  "again. Read the current data from S3 instead:",
  "",
  "  get_rin_service_areas()                    # full version history",
  "  get_rin_service_areas(latest_only = TRUE)  # current records only",
  "  rin_service_areas_tbl()                    # lazy dbplyr table; filters",
  "                                             # push down into the S3 read",
  "",
  "Reading requires valid IAM credentials configured locally, in",
  "~/.aws/credentials or in AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY.",
  sep = "\n"
)

# `rin_service_areas` is deliberately absent from this package: not exported,
# not an object in the namespace, and -- with LazyData off -- not a lazydata
# entry either. So BOTH access paths stop at the point of access, on plain
# assignment and argument passing as much as on use:
#
#   cori.data.rin::rin_service_areas
#   #> Error: 'rin_service_areas' is not an exported object from
#   #>   'namespace:cori.data.rin'
#   cori.data.rin:::rin_service_areas
#   #> Error: object 'rin_service_areas' not found
#
# The message is R's own and cannot carry the pointer to the replacements,
# which is why .onAttach() below states it on every library() call and
# ?rin_service_areas documents it. Two alternatives were tried and both are
# dead ends, so do not reach for them again:
#
#   * An active binding that stop()s. This is the natural fit -- it stops on
#     access, with our own message -- but R CMD INSTALL's test load forces
#     every binding in the namespace, exported or not, so the package cannot
#     be installed at all ("ERROR: loading failed"). Confirmed on a clean
#     full install; curiously, library() on an already-installed copy is fine,
#     so the breakage is specific to installation.
#   * A classed stub object with methods that stop(). This installs, but a
#     plain binding is silent on `x <- ...` and on being passed as an
#     argument, which is how nearly all calling code reaches it -- it only
#     stopped once something touched the value, i.e. too late and far from
#     the cause.
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(.rin_service_areas_moved_msg)
}


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
  "year",
  "year_joined",
  "year_joined_monday",
  "year_joined_stored"
))
