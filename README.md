# Rural Innovation Network data

Framework for defining and capturing metrics to monitor and report out on RIN projects

## Accessing `rin_service_areas`

`cori.data.rin::rin_service_areas` is no longer available as a dataset, because an installed copy goes stale as soon as the pipeline publishes again. Reaching for that name now raises an error, on plain assignment as much as on use. Read the current data from S3 instead:

```r
get_rin_service_areas()                    # full version history
get_rin_service_areas(latest_only = TRUE)  # current records only
rin_service_areas_tbl()                    # lazy dbplyr table; filters push down into the S3 read
```

Prefer `rin_service_areas_tbl()` for anything but a whole-table read — `dplyr` verbs piped onto it translate to SQL, so predicates and column selection are pushed into the parquet scan and only the bytes you need come over the wire. The caller owns its connection; close it with `DBI::dbDisconnect(dbplyr::remote_con(x), shutdown = TRUE)`.

Reading requires valid IAM credentials configured locally, in `~/.aws/credentials` or in `AWS_ACCESS_KEY_ID` / `AWS_SECRET_ACCESS_KEY`. The `cori.data.rin` bucket is private and is not available through the credential-vending endpoint, so vended read-only credentials will not work.

Vintages are published to `s3://cori.data.rin/data_processed/vintage_<YYYYMMDD>/`, with `data_processed/_LATEST` naming the current one. `latest_rin_vintage()` resolves that pointer, `get_rin_vintages()` lists every published vintage with the time its parquet was written to S3, and `get_rin_service_areas(vintage = "20261011")` pins a specific one.

```r
get_rin_vintages()
#>     vintage            modified
#> 1 20261012 2026-10-12 20:07:54
#> 2 20261011 2026-10-11 08:27:35
#> 3 20261009 2026-10-09 16:31:05
```

`modified` is a `POSIXct` that prints in your session's time zone. Every vintage carries the full version history, so you rarely need an old one: filter `data_run_date` to see the state on a given run. Pin a vintage to reproduce an analysis exactly as it was published.

The bundled `data/rin_service_areas.rda` remains the authoritative copy that the pipeline appends to, but it is no longer reachable through the package namespace. If you specifically need that snapshot rather than the published vintage, read it with `utils::data("rin_service_areas", package = "cori.data.rin")`.

## Updating RIN Map Data

The [RIN map](https://github.com/ruralinnovation/rin-map) displays community locations sourced from the Google Sheet named **[RIN Communities on Website](https://docs.google.com/spreadsheets/d/1fDGclR7GqncaLdkEipOJLLwDn3h4gYHVRcJtP_K1wcU)** that is maintained by Comms.

### Data Flow

1. **Source**: Comms updates the "Current_Communities" tab in the Google Sheet which has a final column named `rin_community` that is acting as a foreign key (and therefore **must** be kept in sync with the other columns in a given record).
2. **Pipeline**: MDA runs `targets::tar_make()` to process the data:
   - Fetches communities from Google Sheet
   - Geocodes addresses using OpenStreetMap
   - Generates `data/rin_map.json`
   - Uploads to S3 buckets
3. **Frontend (dev)**: Immediately after running the pipeling, the map deployed to **[development](https://development.d3u96j8olq0rzp.amplifyapp.com)** application fetches `rin_map.json` from the [`dev/cori.data.rin`](https://us-east-1.console.aws.amazon.com/s3/buckets/cori-risi-apps?region=us-east-1&prefix=dev/cori.data.rin/&showversions=false) prefix on S3.
4. **Frontend (prod)**: Once the map update is verified on the map deployed to **[development](https://development.d3u96j8olq0rzp.amplifyapp.com)**, MDA needs to manually copy the `cori.data.rin` folder in `dev/` on S3 to the [`prod/`](https://us-east-1.console.aws.amazon.com/s3/buckets/cori-risi-apps?region=us-east-1&prefix=prod/&showversions=false) prefix. Then verfiy that the map deployed to [production](https://main.d3u96j8olq0rzp.amplifyapp.com/) is showing the updated RIN data.


### Deployment Steps

After Comms updates the Google Sheet:

1. Open an R console in this project directory and run the targets pipeline:
   ```r
   targets::tar_make()
   ```

2. Verify the JSON was generated:
   ```r
   targets::tar_read(rin_map_json_file)
   ```

3. Verify S3 uploads succeeded:
   ```bash
   aws s3 ls s3://cori-risi-apps/dev/cori.data.rin/rin_map.json
   ```

4. Once verified, copy from `dev/` to `prod/` on S3:
   ```bash
   aws s3 cp --recursive s3://cori-risi-apps/dev/cori.data.rin s3://cori-risi-ap
   ```

The frontend automatically fetches from S3 on page load - no frontend redeployment is required.

### S3 Destinations

The pipeline uploads `rin_map.json` to four locations:

| Bucket | Target (environemnt) |
|--------|---------|
| `cori.agent.kb` | Agent knowledge base (dev) |
| `cori.agent.kb-test` | Agent knowledge base (test) |
| `cori-risi-apps` | RIN map (dev) |

## Setup for Development

### Monday API Token

The `rin_service_areas` pipeline fetches RIN community and county data directly from the Monday.com board (ID `6951894369`) via GraphQL API. This requires a personal or admin API token, set as `MONDAY_API_TOKEN` in a project-root `.Renviron` file. This file is gitignored — never commit your token.

To set it up:

1. In Monday.com, go to your avatar → Admin → API (or your profile → Developers, if you don't have admin access) and generate a personal API token
2. Add it to `.Renviron` in this project's root directory:
   ```
   MONDAY_API_TOKEN="<your_token>"
   ```
3. Restart your R session (or run `readRenviron(".Renviron")`) so the variable is picked up

If `MONDAY_API_TOKEN` is not set, `load_rin_service_areas()` falls back to reading the XLSX file named in `monday_network_communities_file_name` (in `params.yml`) from the `data/` directory — the pipeline's original manual-export workflow.

Once you have all the dependencies installed, to build and install this
package from the local project directory, run:

```r
targets::tar_make(); 
pkgbuild::clean_dll(); pkgbuild::compile_dll(); devtools::document(); devtools::check()
devtools::install()
```

```bash
npm run build
npm run preview
```

## Addendum: `load_rin_service_areas` ETL Refactor (2026-06-03)

The `load_rin_service_areas` function in [R/load_data.R](R/load_data.R) has been refactored from a year-derivation/duplication model into a snapshot-based preserve-and-append model. The previous implementation has been archived at [R/archive/old_load_data.R](R/archive/old_load_data.R) for reference.

### What Changed

The previous ETL would, on every pipeline run:

- Re-derive a `year` for each community using hardcoded community-to-year lists plus a fallback to 2023
- Duplicate previous-year records into `current_year` to refresh the "active cohort"
- Stamp every record (including historical ones) with `data_run_date = Sys.Date()`

This destroyed historical snapshot dates on each run and made the dataset effectively a single moving window rather than a versioned series.

The new ETL treats each read of the Monday board as an immutable snapshot:

- All records from the board are assigned `year = params$current_year` only — no historical years are inferred
- Historical records are preserved verbatim with their original `data_run_date` values
- A field `latest_version` ("Yes"/"No") marks the current snapshot

### Snapshot Rule

Every pipeline run appends a complete new snapshot of the current set. There is no trigger and no condition: each run reads the Monday board, writes **every** row it returns with `data_run_date = Sys.Date()` and `latest_version = "Yes"`, and demotes every previously current row to `latest_version = "No"`. The workflow is simply:

1. Run `targets::tar_make()`
2. Run `devtools::install()` if you need the new bundled snapshot in your installed copy

So `data_run_date` means "the run that observed this state", which is what lets any run's view be reconstructed: filter to a date to see exactly what the board said then. `latest_version = "Yes"` always identifies the current set, and all of its rows always share one `data_run_date`.

Two consequences worth knowing:

- A county removed from a community on the board loses its current version on the next run, as it should. Nothing carries forward implicitly.
- Re-running on the same day appends another snapshot and demotes the previous one. From the third run in a day the two demoted batches become indistinguishable — same date, same values, both `"No"` — and are collapsed, so a day retains at most two batches. Only intermediate same-day state is lost; the final snapshot of each day is always kept.

### Note: history before 2026-10-12

Between 2026-08-31 (commit `b4ca51f`) and 2026-10-12, the function only appended rows that were **new or renamed**, so unchanged rows kept the `data_run_date` of whichever earlier run last touched them. Rows dated on or before 2026-10-11 therefore mean "last changed", not "last observed" — for example 94 of the then-current 198 rows carried August dates through three later runs that all saw them as current. This cannot be backfilled, because past Monday board states are not recoverable. Treat `data_run_date` as an observation date only from 2026-10-12 onward.

### Note: community renames fragment the series

Because records are keyed by the `rin_community` name as it appears on the board, a rename produces two distinct community names in the dataset — the old name on historical rows marked `latest_version = "No"`, the new name on current rows. `Greenfield` → `Greenfield MA` and `Tahlequah, OK` → `Cherokee Nation | Tahlequah, OK` are both in the data this way. The `rin_community_alias_to_item_id` map in `params.yml` reconciles the historical names to `monday_id`, which is the stable key; prefer joining on `monday_id` rather than on `rin_community`.

### Archived Implementation

The original ETL is preserved at [R/archive/old_load_data.R](R/archive/old_load_data.R) and includes:

- The hardcoded 2024/2025 community-to-year lists
- The recovery loop that re-injected communities absent from the current XLSX
- The `excluded_communities` list controlling year-duplication carry-forward

Refer to that file when reviewing how historical `year` values were assigned in records still present in the installed package.

## Addendum: Monday API Integration (2026-08-25)

The pipeline no longer depends on a manually downloaded XLSX export from Monday as its primary data source. `R/monday_api.R` adds `fetch_monday_board()`, which pulls all columns — including system fields like `item_id`, `created_at`, and `updated_at` — directly from the Monday board via GraphQL.

### What Changed

- `load_rin_service_areas()` in [R/load_data.R](R/load_data.R) checks for `MONDAY_API_TOKEN` first; if present, it fetches live data from Monday instead of reading `data/Network_Communities_Current_*.xlsx`
- Monday's `item_id` for each community record becomes the `monday_id` column, prepended to `rin_service_areas`
- A one-time migration target, `rin_service_areas_with_monday_id`, backfilled `monday_id` onto the previously-installed `cori.data.rin::rin_service_areas` package data using a `rin_community_alias_to_item_id` lookup table in `params.yml`. That mapping covers historical community name aliases (e.g. `Aberdeen` → `Aberdeen, SD`) as well as renames on the Monday board itself (e.g. `Tahlequah, OK` → `Tahlequah, OK | Cherokee Nation`)
- The XLSX path remains as a fallback for anyone without a Monday API token, and uses the same `rin_community_alias_to_item_id` mapping to populate `monday_id`

### Setup Required

Anyone running this pipeline needs their own `MONDAY_API_TOKEN` in a local `.Renviron` file — see "Monday API Token" under Setup for Development, above.
