# kg_viewer

**Köppen-Geiger climate of pest occurrences.** A Shiny app that shows, for one
plant pest at a time, where it has been recorded (GBIF), which Köppen-Geiger
climate classes those records fall in, and how much of non-alpine mainland
Norway lies in the same classes today (1991–2020) and under 2041–2070
SSP5-8.5.

It is background evidence for manual climate-suitability (EST1) judgements in
pest risk ranking, not a species distribution model.

Live app: <https://danielapp.shinyapps.io/kg-viewer/>

> Status: **draft**. Thresholds and uncertainty ratings are judgement values
> and are documented as such in `kg_db.R`.

## What you see

- **Species picker.** Type a scientific name or EPPO code (63 species in the
  current build).
- **Map.** Cleaned GBIF records, one per ~1 km cell. Filled red points lie in
  an *occupied* climate class. Hollow points lie in classes that fall below
  the occupancy rule. Shading shows the occupied classes on the chosen period's
  map (Beck et al. 2023, 0.1°).
- **Summary line.** The share of non-alpine Norway in the occupied classes,
  now and in 2041–2070.
- **Class table.** Every class with records: cell count, whether it counts as
  occupied, and Norway's share of that class.

A class is **occupied** when it holds at least 5 of the species' 1 km cells
*and* at least 0.1 % of them. The absolute floor removes single misplaced
records. The share floor stops a few residual errors from qualifying for very
widespread species.

## Repository layout

| Path | What |
|---|---|
| `app.R` | The Shiny app. Reads `kg_climate.db` read-only. |
| `kg_db.R` | Builds `kg_climate.db`: GBIF download, cleaning, KG lookup, Norway profile, summary statistics. |
| `kg_climate.db` | SQLite database the app reads (built 2026-10-14). |
| `koppen_geiger_0p1.tif` | Beck et al. 2023, 1991–2020, 0.1°, used for map shading. |
| `koppen_geiger_2041_2070_ssp585_0p1.tif` | Same, 2041–2070 SSP5-8.5. |
| `gbif_cache/` | One `.rds` per species with raw GBIF records. Lets `kg_db.R` re-run without re-downloading. |
| `vignettes/kg_viewer.qmd` | Method vignette: data, pipeline, worked examples, limitations, references. |
| `rsconnect/` | shinyapps.io deployment record. |

## Run the app

```r
install.packages(c("shiny", "bslib", "leaflet", "DT", "terra", "png", "DBI",
                   "RSQLite", "dplyr", "rnaturalearth", "rnaturalearthdata",
                   "xfun", "htmltools"))
shiny::runApp(".")   # from the repository root
```

The app draws country outlines itself and needs no web map tiles. It works
where tile servers are blocked.

## Rebuild the database

`kg_db.R` needs these packages in addition to the app's: `rgbif`,
`CoordinateCleaner`, `readxl` and `ggplot2` (`ggplot2` only for the example
plot at the end).

You also need inputs that are not in this repository:

1. **Species list.** An Excel sheet with columns `eppocode`, `fullname`,
   `kingdom` and `batch`. Set `SPECIES_FILE` and `SPECIES_SHEET`.
2. **1 km Köppen-Geiger rasters** from <https://www.gloh2o.org/koppen/>,
   1991–2020 and 2041–2070/ssp585. Set `KG_DIR`. The raster is too large to
   commit.

The paths `OUT_DB` and `GBIF_CACHE_DIR` in `kg_db.R` still point to
`./scripts/kg_viewer/…` from the project this app came from. Change them to `.`
when running from this repository. GBIF downloads are cached per species, so
delete `gbif_cache/` to refresh everything.

Pipeline, in short:

1. Read the KG legend and compute Norway's area per class (non-alpine:
   ET/EF excluded) for both periods.
2. Match species to the GBIF backbone. Only exact or variant matches are kept,
   synonyms are mapped to the accepted key, and pathovars are skipped.
3. Download occurrences in latitude bands of ≤ 9,000 records. This avoids
   GBIF's slow deep paging.
4. Clean the records. Drop fossil specimens, living specimens and managed
   records, and coordinate uncertainty above 10 km. Then run CoordinateCleaner
   tests: capitals, centroids, equal, gbif, institutions and zeros.
5. Look up the KG class per record and thin to one row per 1 km cell.
6. Summarise per species. This gives class counts, presence in Norway, the
   Nordic countries and Europe, the 95th percentile of absolute latitude, the
   share of records since 2000, and a resampling **class stability** with an
   uncertainty rating (low / medium / high).

## Database

| Table | Content |
|---|---|
| `species` | One row per species with at least one occupied class: GBIF match, record counts, Nordic/European presence, `lat95`, `classStability`, `uncertainty`. |
| `occurrences` | One row per species × cell × basisOfRecord × country, with lon/lat, KG class and years. |
| `pestKgClasses` | Cells and records per species × KG class (ET/EF excluded). |
| `occupiedKgClasses` | *View*: the `pestKgClasses` rows that pass the occupancy rule. |
| `kgNorway`, `kgNorwayFuture` | km² and share of non-alpine Norway per class, 1991–2020 and 2041–2070 SSP5-8.5. |
| `kgLegend` | The 30 KG classes: value, code, label, colour. |
| `buildInfo` | Build date, sources and every threshold used. |

Example: the share of Norway in each species' occupied classes.

```sql
SELECT p.eppoCode, ROUND(SUM(n.proportion) * 100, 1) AS pctNorway
FROM occupiedKgClasses p JOIN kgNorway n USING (kgCode)
GROUP BY p.eppoCode ORDER BY pctNorway DESC;
```

## Deploy

```r
rsconnect::deployApp(".", appName = "kg-viewer")
```

The app builds map images itself instead of calling
`leaflet::addRasterImage()`, which keeps peak memory well under shinyapps.io's
1 GB.

## Data sources and licences

- Beck, H.E. et al. (2023). High-resolution (1 km) Köppen-Geiger maps for
  1901–2099 based on constrained CMIP6 projections. *Scientific Data* 10, 724.
  <https://doi.org/10.1038/s41597-023-02549-6>. CC BY 4.0.
- GBIF.org occurrence data via `rgbif`. Cite the GBIF downloads per GBIF's
  citation guidelines if you publish results.
- Natural Earth country boundaries (public domain) via `rnaturalearth`.
