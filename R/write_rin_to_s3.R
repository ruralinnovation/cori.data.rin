#' Write rin_service_areas.parquet to S3 as a dated vintage
#'
#' Uploads a local `rin_service_areas.parquet` to
#' `s3://<s3_bucket>/<s3_path_prefix>data_processed/vintage_<YYYYMMDD>/rin_service_areas.parquet`
#' and then updates the `<s3_path_prefix>data_processed/_LATEST` pointer file
#' so `latest_rin_vintage()` resolves to the new vintage.
#'
#' The data is uploaded first; `_LATEST` is only updated once the upload has
#' succeeded, so the pointer never refers to a vintage that is not there.
#'
#' @param file_path Character. Local path to the parquet file to upload.
#' @param vintage Character. Date stamp for the vintage, `YYYYMMDD`. Default:
#'   today.
#' @param s3_bucket Character. S3 bucket. Default: `"cori.data.rin"`.
#' @param s3_path_prefix Character. Optional prefix for all keys, e.g.
#'   `"test/"` for a trial run. Default: `""`.
#' @param overwrite Logical. Replace the parquet if this vintage already exists
#'   (a second upload on the same day). Default: `FALSE`.
#'
#' @return Invisibly, a list with `vintage`, `uri` and `latest_uri`.
#'
#' @keywords internal
write_rin_service_areas_to_s3 <- function(
    file_path,
    vintage        = format(Sys.Date(), "%Y%m%d"),
    s3_bucket      = "cori.data.rin",
    s3_path_prefix = "",
    overwrite      = FALSE
) {

  if (!file.exists(file_path)) {
    stop("File not found: ", file_path, call. = FALSE)
  }
  if (!grepl("^[0-9]{8}$", vintage)) {
    stop("`vintage` must be a YYYYMMDD date stamp, got: ", vintage, call. = FALSE)
  }

  vintage_tag <- sprintf("vintage_%s", vintage)
  s3_key      <- sprintf("%sdata_processed/%s/rin_service_areas.parquet",
                         s3_path_prefix, vintage_tag)
  s3_uri      <- sprintf("s3://%s/%s", s3_bucket, s3_key)

  message(sprintf("Uploading %s (%s bytes) to %s",
                  file_path, format(file.size(file_path), big.mark = ","), s3_uri))

  if (overwrite) {
    # put_s3_object() refuses to replace an existing key, so a same-day
    # re-upload goes through the CLI. Only this single key is touched.
    exit_code <- system2("aws", c("s3", "cp", file_path, s3_uri))
    if (exit_code != 0) {
      stop("aws s3 cp failed writing ", s3_uri, call. = FALSE)
    }
  } else {
    cori.data.s3::put_s3_object(s3_bucket, s3_key, file_path)
  }

  # _LATEST is a pointer overwritten on every run, so it also bypasses
  # put_s3_object(). It is one line, so stage it in tempdir().
  latest_file <- file.path(tempdir(), "_LATEST")
  writeLines(vintage_tag, latest_file)
  on.exit(unlink(latest_file), add = TRUE)

  latest_uri <- sprintf("s3://%s/%sdata_processed/_LATEST", s3_bucket, s3_path_prefix)
  exit_code <- system2("aws", c("s3", "cp", latest_file, latest_uri))
  if (exit_code != 0) {
    stop("aws s3 cp failed writing _LATEST to ", latest_uri, call. = FALSE)
  }
  message("_LATEST updated to: ", vintage_tag)

  invisible(list(vintage = vintage_tag, uri = s3_uri, latest_uri = latest_uri))
}

