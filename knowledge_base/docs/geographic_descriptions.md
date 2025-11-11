# FIPS Codes (geoid)

Federal Information Processing Standards (FIPS) codes are standardized identifiers for geographic entities in the United States. The geoid_co field contains 5-digit county FIPS codes.

**Identification**: geoid_co field contains 5-digit FIPS codes
**Format**: First 2 digits = state code, Last 3 digits = county code
**Example**: 04005 = Arizona (04) + Coconino County (005)
**Usage**: Primary key for geographic lookups and data joins

# RIN Communities (rin_community)

Rural Innovation Network (RIN) communities are counties and regions that participate in CORI's network of rural innovation initiatives.

**Identification**: rin_community field contains community names
**Flags**: primary_county_flag indicates primary vs secondary counties
**Coverage**: 150+ communities across rural America

## Querying RIN Communities by Name

When users ask about RIN communities by name (e.g., "What data do you have for the Dalles?"), the system needs to handle:

1. **Case-insensitive matching**: "The Dalles", "the dalles", "THE DALLES" should all match
2. **Partial/fuzzy matching**: Users may omit articles like "the" or use variations
3. **Leading article removal**: Strip common articles ("the", "a", "an") before matching

**Query Pattern**: Use ILIKE (PostgreSQL) or LIKE (case-insensitive) with wildcards
**Example**: `rin_community ILIKE '%Dalles%'` matches "The Dalles"
**Best Practice**: Strip leading "the/a/an" from user input, then use `%keyword%` pattern

**Common RIN Community Name Queries:**
- "The Dalles" → Search for "Dalles" → `rin_community ILIKE '%Dalles%'`
- "Hood River" → `rin_community ILIKE '%Hood River%'`
- "Cochise County" → `rin_community ILIKE '%Cochise%'`

# Geographic Boundaries

Land and water area measurements provide context for county characteristics:
- aland: Land area in square meters
- awater: Water area in square meters
- intptlat/intptlon: Interior point coordinates representing centroid of county boundary
- lat/lon: Population-weighted geographic center point coordinates
- centroid: Population-weighted geographic center point geometry
