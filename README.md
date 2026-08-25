# Rural Innovation Network data

Framework for defining and capturing metrics to monitor and report out on RIN projects

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

The new ETL treats each Monday board export (`data/Network_Communities_Current_*.xlsx`, declared via `monday_network_communities_file_name` in `params.yml`) as an immutable snapshot:

- All records derived from the Monday XLSX are assigned `year = params$current_year` only — no historical years are inferred from the XLSX
- Historical records (years 2023, 2024, 2025) are preserved verbatim from the installed `cori.data.rin::rin_service_areas` package data, with their original `data_run_date` values
- A new boolean field, `latest_version` ("Yes"/"No"), marks the most recent snapshot

### Snapshot Trigger via Git

A new snapshot is created only when `git diff HEAD params.yml` shows an uncommitted change to `monday_network_communities_file_name`. The workflow is:

1. Export a new XLSX from the Monday board into `data/`
2. Update `monday_network_communities_file_name` in `params.yml`
3. Run `targets::tar_make()` — the function detects the uncommitted filename change and appends new records with `data_run_date = Sys.Date()` and `latest_version = "Yes"`, demoting all prior records to `latest_version = "No"`
4. Run `devtools::install()` to bake the new snapshot into the package
5. Commit `params.yml`

Once `params.yml` is committed, subsequent pipeline runs produce no new records. The anti-join key `(rin_community, county, year, data_run_date)` ensures same-day re-runs after install do not create duplicates.

### Note: Greenfield → Greenfield MA Rename

The Monday board record formerly named `Greenfield` was renamed to `Greenfield MA`. Because the ETL keys records by the `rin_community` name as given in the XLSX, this rename produces two distinct community names in the dataset:

- `Greenfield` — historical records only (years 2024, 2025, and the May 14 2026 snapshot), `latest_version = "No"`
- `Greenfield MA` — current snapshot (year 2026, `data_run_date = 2026-06-03`), `latest_version = "Yes"`

Both map to Franklin County, MA. The 44 unique communities in the latest XLSX therefore appear as only 43 distinct names under `latest_version = "Yes"`. A future fix should normalize the name (mapping `Greenfield MA` → `Greenfield`) before the snapshot is keyed, so the rename does not fragment the historical series.

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
