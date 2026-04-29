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
3. **Frontend (dev)**: Immediate the map deployed to **[development](https://development.d3u96j8olq0rzp.amplifyapp.com)** application fetches `rin_map.json` from the [`dev/cori.data.rin`](https://us-east-1.console.aws.amazon.com/s3/buckets/cori-risi-apps?region=us-east-1&prefix=dev/cori.data.rin/&showversions=false) prefix on S3.
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

| Bucket | Path | Purpose |
|--------|------|---------|
| `cori.agent.kb` | `dev/data/rin_map.json` | Agent knowledge base (dev) |
| `cori.agent.kb-test` | `test/data/rin_map.json` | Agent knowledge base (test) |
| `cori-risi-apps` | `dev/cori.data.rin/rin_map.json` | Frontend map (dev) |
| `cori-risi-apps` | `test/cori.data.rin/rin_map.json` | Frontend map (test) |

## Setup for Development

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
