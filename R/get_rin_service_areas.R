#' @importFrom dbplyr remote_con
NULL

# Internal: SQL clause that makes a vintage's two flags logical, "" when they
# already are.
#
# `latest_version` and `primary_county_flag` are logical from the 2026-10-13
# build onward. Vintages published before that (through 20261012) store them as
# "Yes"/"No" strings, and a published vintage is never rewritten, so those are
# converted here to give every vintage the same types. Keyed to the column
# type, so a logical vintage is read untouched and its filters still push down
# into the parquet scan.
.rin_legacy_flag_sql <- function(con, uri) {
  flags  <- c("primary_county_flag", "latest_version")
  probe  <- DBI::dbGetQuery(con, sprintf(
    "SELECT %s FROM read_parquet('%s') LIMIT 0", paste(flags, collapse = ", "), uri
  ))
  legacy <- flags[vapply(probe[flags], is.character, logical(1))]

  if (length(legacy) == 0) return("")
  sprintf(" REPLACE (%s)",
          paste(sprintf("(%s = 'Yes') AS %s", legacy, legacy), collapse = ", "))
}

# dplyr::tbl() on a DBI connection dispatches to dbplyr, which dplyr only
# Suggests, so rin_service_areas_tbl() fails without it. No code here calls
# dbplyr directly (remote_con is what the docs tell callers to use), so the
# importFrom above is what records the hard dependency for R CMD check.

# Internal: DuckDB connection for reading this package's S3 parquet.
#
# `require_local = TRUE`: cori.data.rin is a private bucket and is not on the
# credential-vending endpoint's allowlist, so only valid IAM credentials
# configured locally can read it. Asking for local credentials up front turns
# what would be an opaque S3 access-denied error mid-query into a clear
# message.
.rin_s3_con <- function(s3_bucket) {
  con <- cori.data.s3::connect_to_s3(s3_bucket, require_local = TRUE)
  DBI::dbExecute(con, sprintf("SET temp_directory = '%s';", tempdir()))
  con
}


# Internal: normalize a vintage argument to a `vintage_YYYYMMDD` tag.
.rin_resolve_vintage <- function(vintage, s3_bucket, s3_path_prefix, con) {
  if (identical(vintage, "latest")) {
    latest_rin_vintage(s3_bucket, s3_path_prefix, con = con)
  } else if (startsWith(vintage, "vintage_")) {
    vintage
  } else {
    sprintf("vintage_%s", vintage)
  }
}


# Internal: make `con` able to read `uri`, falling back to a non-geospatial
# read only when DuckDB rejects the file's GeoParquet metadata.
#
# The file is currently written by sfarrow, whose GeoParquet metadata carries
# no `version` key, and DuckDB's GeoParquet reader rejects that outright
# ("Geoparquet metadata does not have a version"). Turning the conversion off
# reads the file as plain parquet, with `geometry` as a BLOB -- harmless here,
# since every caller drops that column.
#
# The normal read is attempted first, so a file with valid GeoParquet metadata
# (a future writer, or a DuckDB that tolerates this one) is read on DuckDB's
# own terms, and the fallback is keyed to the metadata error alone so genuine
# failures (a missing key, denied access) are raised from the first attempt
# rather than retried.
#
# This has to run BEFORE dplyr::tbl(): tbl() reads the schema, and when that
# read fails it reports only "Can't query fields." -- too generic to key a
# fallback on. A LIMIT 0 query reads the same parquet footer and surfaces the
# real error, so the probe happens here and the setting is already in place by
# the time the lazy table is built.
.rin_prepare_con <- function(con, uri) {
  cnd <- tryCatch({
    DBI::dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s') LIMIT 0", uri))
    NULL
  }, error = function(e) e)

  if (is.null(cnd)) return(invisible(FALSE))
  if (!grepl("geoparquet", conditionMessage(cnd), ignore.case = TRUE)) stop(cnd)

  DBI::dbExecute(con, "SET enable_geoparquet_conversion = false;")
  invisible(TRUE)
}


#' Return the current latest vintage of rin_service_areas on S3
#'
#' Reads the `_LATEST` pointer written by `write_rin_service_areas_to_s3()`.
#' Requires locally configured IAM credentials -- see
#' [get_rin_service_areas()].
#'
#' @param s3_bucket Character. S3 bucket. Default: `"cori.data.rin"`.
#' @param s3_path_prefix Character. Optional prefix, e.g. `"test/"`. Must match
#'   the prefix `_LATEST` was written under. Default: `""`.
#' @param con Optional open DuckDB connection to reuse. Default: open and
#'   close one.
#'
#' @return Character, e.g. `"vintage_20261009"`.
#' @seealso [get_rin_vintages()] for every published vintage.
#' @export
#'
#' @examples
#' \dontrun{
#' latest_rin_vintage()
#' }
latest_rin_vintage <- function(s3_bucket = "cori.data.rin",
                               s3_path_prefix = "",
                               con = NULL) {

  if (is.null(con)) {
    con <- .rin_s3_con(s3_bucket)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  }

  uri <- sprintf("s3://%s/%sdata_processed/_LATEST", s3_bucket, s3_path_prefix)

  tag <- tryCatch(
    DBI::dbGetQuery(con, sprintf(
      "SELECT column0 AS tag FROM read_csv('%s', header = false);", uri
    ))$tag,
    error = function(e) stop(sprintf(
      paste0("Could not read _LATEST from %s: %s\n",
             "Has the parquet been published under the same s3_path_prefix?"),
      uri, conditionMessage(e)
    ), call. = FALSE)
  )

  tag <- trimws(tag[1])
  if (!grepl("^vintage_[0-9]{8}$", tag)) {
    stop("Unexpected _LATEST content: '", tag, "' (expected vintage_YYYYMMDD)",
         call. = FALSE)
  }
  tag
}


#' List the published vintages of rin_service_areas on S3
#'
#' One row per vintage whose `rin_service_areas.parquet` exists under
#' `data_processed/`, newest first. Pass a `vintage` value to
#' [get_rin_service_areas()] or [rin_service_areas_tbl()] to read that vintage
#' rather than the one `_LATEST` points at (see [latest_rin_vintage()]).
#'
#' `modified` is when that vintage's parquet was last written to S3, as
#' reported by S3 (`LastModified`), and prints in your session's time zone.
#' Re-publishing a vintage overwrites its parquet, so it reflects the most
#' recent write rather than the first.
#'
#' Requires valid IAM credentials configured locally -- see
#' [get_rin_service_areas()].
#'
#' @inheritParams latest_rin_vintage
#'
#' @return A data.frame with columns `vintage` (character, e.g. `"20261011"`)
#'   and `modified` (`POSIXct`). Zero rows, both columns present, when nothing
#'   has been published.
#' @export
#'
#' @examples
#' \dontrun{
#' get_rin_vintages()
#' get_rin_service_areas(vintage = get_rin_vintages()$vintage[2])
#' }
get_rin_vintages <- function(s3_bucket = "cori.data.rin",
                             s3_path_prefix = "") {

  if (!cori.data.s3::has_local_aws_credentials()) {
    stop("No local AWS credentials found. The ", s3_bucket, " bucket is ",
         "private and is not available through credential vending, so ",
         "listing it requires valid IAM credentials in ~/.aws/credentials or ",
         "in AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY.", call. = FALSE)
  }

  prefix <- sprintf("%sdata_processed/", s3_path_prefix)
  objs   <- cori.data.s3::list_s3_objects(s3_bucket, prefix = prefix)

  rel   <- substring(objs$key, nchar(prefix) + 1)
  hit   <- grepl("^vintage_[0-9]{8}/rin_service_areas\\.parquet$", rel)
  found <- data.frame(
    vintage  = substr(rel[hit], nchar("vintage_") + 1, nchar("vintage_") + 8),
    modified = as.POSIXct(objs$last_modified[hit]),
    stringsAsFactors = FALSE
  )

  found <- found[order(found$vintage, decreasing = TRUE), , drop = FALSE]
  rownames(found) <- NULL
  found
}


#' Lazy table over rin_service_areas on S3
#'
#' Returns a `dbplyr` lazy table over `rin_service_areas.parquet` for one
#' vintage, without the `geometry` column. Nothing is read until the query is
#' collected, and `dplyr` verbs piped onto it are translated to SQL, so
#' `filter()` predicates and the selected columns are pushed down into the
#' parquet scan -- only the column chunks and row groups actually needed are
#' fetched from S3. Prefer this to [get_rin_service_areas()] whenever you want
#' a subset of a vintage.
#'
#' The caller owns the connection. A lazy table stops working once its
#' connection is closed, so this cannot clean up after itself: either pass an
#' open `con` and close it yourself, or close the one this opens, which is
#' reachable with `dbplyr::remote_con()`.
#'
#' Requires valid IAM credentials configured locally -- see
#' [get_rin_service_areas()].
#'
#' @inheritParams get_rin_service_areas
#' @param con Optional open DuckDB connection to reuse. Default: open one,
#'   which the caller must then close.
#'
#' @return A `tbl_dbi` lazy table.
#' @export
#'
#' @examples
#' \dontrun{
#' rin <- rin_service_areas_tbl(latest_only = TRUE)
#' on.exit(DBI::dbDisconnect(dbplyr::remote_con(rin), shutdown = TRUE))
#'
#' rin |>
#'   dplyr::filter(statefp == "46") |>
#'   dplyr::select(rin_community, county, year_joined) |>
#'   dplyr::collect()
#' }
rin_service_areas_tbl <- function(vintage        = "latest",
                                  latest_only    = FALSE,
                                  s3_bucket      = "cori.data.rin",
                                  s3_path_prefix = "",
                                  con            = NULL) {

  owned <- is.null(con)
  if (owned) con <- .rin_s3_con(s3_bucket)

  tryCatch({
    vintage_tag <- .rin_resolve_vintage(vintage, s3_bucket, s3_path_prefix, con)

    uri <- sprintf("s3://%s/%sdata_processed/%s/rin_service_areas.parquet",
                   s3_bucket, s3_path_prefix, vintage_tag)

    .rin_prepare_con(con, uri)

    lazy_tbl <- dplyr::tbl(con, dplyr::sql(sprintf(
      "SELECT * EXCLUDE (geometry)%s FROM read_parquet('%s')",
      .rin_legacy_flag_sql(con, uri), uri
    )))

    if (latest_only) {
      lazy_tbl <- dplyr::filter(lazy_tbl, latest_version)
    }

    lazy_tbl
  }, error = function(e) {
    # Opened here, so it is this function's to clean up on the way out.
    if (owned) DBI::dbDisconnect(con, shutdown = TRUE)
    stop(e)
  })
}


#' Get RIN service areas from S3
#'
#' Reads `rin_service_areas.parquet` for one vintage from the private
#' `cori.data.rin` bucket and returns it without the `geometry` column, with
#' the same columns and types as the `rin_service_areas` data object shipped
#' with this package. The `_LATEST` pointer decides which vintage is read by
#' default.
#'
#' Unlike the packaged `rin_service_areas` object -- a snapshot frozen at the
#' last package build -- this reads whatever the pipeline published most
#' recently.
#'
#' This collects the whole vintage. To pull a subset, pipe `dplyr` verbs onto
#' [rin_service_areas_tbl()] instead and let the filters push down into the
#' parquet scan.
#'
#' `latest_version` and `primary_county_flag` are logical (`TRUE`/`FALSE`). Older
#' vintages store them as `"Yes"`/`"No"`; those are converted on read, so every
#' vintage returns the same types.
#'
#' The bucket is private and is not available through the credential-vending
#' endpoint, so reading it requires valid IAM credentials configured locally,
#' in `~/.aws/credentials` or in the `AWS_ACCESS_KEY_ID` /
#' `AWS_SECRET_ACCESS_KEY` environment variables.
#'
#' @param vintage Character. `"latest"` (default) to follow `_LATEST`, or a
#'   date stamp such as `"20261009"` (or `"vintage_20261009"`).
#' @param latest_only Logical. Keep only current records
#'   (`latest_version` is `TRUE`). Default: `FALSE`, i.e. the full version
#'   history.
#' @param s3_bucket Character. S3 bucket. Default: `"cori.data.rin"`.
#' @param s3_path_prefix Character. Optional prefix, e.g. `"test/"`. Default:
#'   `""`.
#'
#' @return A data.frame with one row per community, county and version.
#' @export
#'
#' @examples
#' \dontrun{
#' rin <- get_rin_service_areas()
#' current <- get_rin_service_areas(latest_only = TRUE)
#' }
get_rin_service_areas <- function(vintage        = "latest",
                                  latest_only    = FALSE,
                                  s3_bucket      = "cori.data.rin",
                                  s3_path_prefix = "") {

  con <- .rin_s3_con(s3_bucket)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  vintage_tag <- .rin_resolve_vintage(vintage, s3_bucket, s3_path_prefix, con)

  lazy_tbl <- rin_service_areas_tbl(
    vintage        = vintage_tag,
    latest_only    = latest_only,
    s3_bucket      = s3_bucket,
    s3_path_prefix = s3_path_prefix,
    con            = con
  )

  df <- as.data.frame(dplyr::collect(lazy_tbl))

  message(sprintf("cori.data.rin: %s | %s rows", vintage_tag,
                  format(nrow(df), big.mark = ",")))
  df
}
