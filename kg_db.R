# kg_db.R  (DRAFT)
# Builds kg_climate.db: GBIF occurrences for every species in
# master_species.xlsx, cleaned,
# thinned to one point per Köppen-Geiger raster cell, with the KG class of
# each point, plus Norway's KG profile.
#
# Used by:
#   - app.R in this folder: choose a species -> map of occurrences on the
#     KG map + table of classes occupied vs. Norway
#   - populate_finnprio_justifications.py (planned): EST1 evidence lookup
#     by eppoCode
#
# Data:
#   - Beck et al. 2023, High-resolution (1 km) Köppen-Geiger maps for
#     1901-2099, Scientific Data. Period 1991-2020. Download manually from
#     https://www.gloh2o.org/koppen/ and unzip; set KG_DIR below. CC BY 4.0.
#   - GBIF occurrence search API (rgbif::occ_data), no account needed.
#     Queried in latitude bands of < 10,000 records (see section 4); largest
#     in master_species.xlsx: Lycorma delicatula, ~76,000 records in 2026-10.
#
# Tables written to kg_climate.db:
#   kgLegend        (kgValue, kgCode, kgLabel, hex)
#   kgNorway        (kgCode, areaKm2, proportion)          mainland Norway
#   kgNorwayFuture  (kgCode, areaKm2, proportion)          same, 2041-2070 SSP5-8.5
#   species         (eppoCode, scientificName, kingdom, gbifTaxonKey,
#                    acceptedKey, gbifName, gbifRank, gbifMatch,
#                    nDownloaded, nClean, nCells,
#                    nCellsNonAlpine, nCellsNorway, nCellsNordic,
#                    nordicCountries, nCellsEurope, pctCellsSince2000,
#                    classStability, lat95, borealOccupied, uncertainty)
#   occurrences     (eppoCode, cell, lon, lat, kgCode, basisOfRecord,
#                    countryCode, nRecords, firstYear, lastYear)
#                                                          one row per cell,
#                                                          basisOfRecord, country
#   pestKgClasses   (eppoCode, kgCode, nCells, nRecords, nCellsSince2000)
#   occupiedKgClasses (view: eppoCode, kgCode, nCells)     pestKgClasses rows that
#                                                          pass the occupied rule
#   buildInfo       (key, value)                           build date, sources, settings

library(DBI)
library(RSQLite)
library(dplyr)
library(terra)
library(rgbif)
library(CoordinateCleaner)
library(rnaturalearth)

# -------------------------------------------------------------------------
# Configuration
# -------------------------------------------------------------------------

# Species list (columns used: eppocode, fullname, kingdom)
SPECIES_FILE  <- "C:/Users/dafl/Downloads/master_species.xlsx"
SPECIES_SHEET <- "Master species"

# Beck et al. 2023, 1991-2020 folder (rasters + legend.txt)
KG_DIR    <- "C:/Users/dafl/OneDrive - Folkehelseinstituttet/Daniel jobb/Climat layers files/High-resolution (1 km) Köppen-Geiger/1991_2020"
KG_RASTER <- file.path(KG_DIR, "koppen_geiger_0p00833333.tif")  # 1 km
KG_LEGEND <- file.path(KG_DIR, "legend.txt")

# Same dataset, 2041-2070 under SSP5-8.5 (worst case), for Norway's future
# climate profile. From koppen_geiger_tif.zip at
# https://doi.org/10.6084/m9.figshare.21789074 (2041_2070/ssp585).
KG_FUTURE_RASTER <- file.path(dirname(KG_DIR), "2041_2070", "ssp585",
                              "koppen_geiger_0p00833333.tif")

OUT_DB <- "./scripts/kg_viewer/kg_climate.db"   # next to the viewer app

# One .rds per species; a re-run only fetches species not cached yet.
# Delete the folder to refresh all GBIF data.
GBIF_CACHE_DIR <- "./scripts/kg_viewer/gbif_cache"   # gitignored (*_cache/)
GBIF_FIELDS    <- c("decimalLongitude", "decimalLatitude",
                    "coordinateUncertaintyInMeters", "basisOfRecord",
                    "establishmentMeans", "year", "countryCode")

# Cleaning
MAX_UNCERTAINTY_M <- 10000                     # drop worse; NA uncertainty kept
DROP_BASIS        <- c("FOSSIL_SPECIMEN",      # not current range
                       "LIVING_SPECIMEN")      # botanic gardens, collections
DROP_ESTABLISHMENT <- "MANAGED"                # cultivated / greenhouse

CC_TESTS          <- c("capitals", "centroids", "equal", "gbif",
                       "institutions", "zeros")

# Alpine/polar classes left out of the climate profiles (no plant pests
# there). Occurrence points in these classes are kept for the map.
EXCLUDE_KG <- c("ET", "EF")

# A class counts as "occupied" when it holds at least OCC_MIN_CELLS occupied
# 1 km cells AND at least OCC_MIN_SHARE of the species' cells. The absolute
# floor removes single misplaced or museum records; the share floor stops a
# few hundred residual errors in very large species (10k+ cells) from
# reaching the absolute floor. No published standard exists (EFSA uses
# classes "with records", unthresholded); this is a judgement. Applied as
# the view occupiedKgClasses, so the stored counts stay raw.
OCC_MIN_CELLS <- 5
OCC_MIN_SHARE <- 0.001

# Presence near Norway (GBIF countryCode of the records)
NORDIC <- c("NO", "SE", "FI", "DK")

# Cold-summer (boreal) classes; Dfc alone is ~75 % of non-alpine Norway
BOREAL_KG <- c("Dfc", "Dfd", "Dwc", "Dwd", "Dsc", "Dsd")

# Uncertainty of the climate-class list per species. Stability = share of
# STAB_RUNS random halves of the species' cells that give the same occupied
# Norwegian climate classes as all cells (would more records change them?).
# Rating: low = >= UNC_LOW_CELLS cells and stability >= 0.9; medium =
# >= UNC_MEDIUM_CELLS cells and stability >= 0.5; else high. Cell counts are
# non-alpine 1 km cells. Judgement values.
STAB_RUNS        <- 100
UNC_MEDIUM_CELLS <- 200
UNC_LOW_CELLS    <- 2000

# Mainland Norway (excludes Svalbard and Jan Mayen)
NORWAY_EXT <- ext(4, 32, 57, 71.5)

dir.create(dirname(OUT_DB), recursive = TRUE, showWarnings = FALSE)
dir.create(GBIF_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------------------------
# 1. Köppen-Geiger raster and legend
# -------------------------------------------------------------------------

kg <- rast(KG_RASTER)

# legend.txt lines look like: "    1:  Af   Tropical, rainforest   [0 0 255]"
legend_lines <- grep("^\\s*\\d+:", readLines(KG_LEGEND, warn = FALSE), value = TRUE)

kg_legend <- tibble(
  kgValue = as.integer(sub("^\\s*(\\d+):.*", "\\1", legend_lines)),
  kgCode  = sub("^\\s*\\d+:\\s+(\\S+).*", "\\1", legend_lines),
  kgLabel = trimws(sub("^\\s*\\d+:\\s+\\S+\\s+(.*?)\\s*\\[.*", "\\1", legend_lines)),
  rgbText = sub(".*\\[(.*)\\].*", "\\1", legend_lines)
)

rgb_m <- do.call(rbind, lapply(strsplit(trimws(kg_legend$rgbText), "\\s+"), as.numeric))

kg_legend <- kg_legend %>%
  mutate(hex = rgb(rgb_m[, 1], rgb_m[, 2], rgb_m[, 3], maxColorValue = 255)) %>%
  select(-rgbText)

stopifnot(nrow(kg_legend) == 30)

# -------------------------------------------------------------------------
# 2. Norway profile (km² per KG class, cell areas shrink with latitude)
# -------------------------------------------------------------------------

norway <- ne_countries(scale = "medium", country = "Norway", returnclass = "sf") %>%
  vect() %>%
  crop(NORWAY_EXT)

norway_profile <- function(kg_map) {
  kg_no   <- mask(crop(kg_map, norway), norway)
  area_no <- cellSize(kg_no, unit = "km")
  zonal(area_no, kg_no, fun = "sum", na.rm = TRUE) %>%
    as_tibble() %>%
    setNames(c("kgValue", "areaKm2")) %>%
    filter(kgValue > 0) %>%
    left_join(kg_legend, by = "kgValue") %>%
    filter(!kgCode %in% EXCLUDE_KG) %>%
    transmute(kgCode, areaKm2, proportion = areaKm2 / sum(areaKm2)) %>%   # share of non-alpine Norway
    arrange(desc(areaKm2))
}

kg_norway <- norway_profile(kg)
print(kg_norway)          # expect Dfc, Dfb dominant; Cfb/Cfc on the west coast

kg_norway_future <- norway_profile(rast(KG_FUTURE_RASTER))
print(kg_norway_future)   # 2041-2070 SSP5-8.5: expect Dfb/Cfb to grow at Dfc's expense

# -------------------------------------------------------------------------
# 3. Species list from master_species.xlsx
# -------------------------------------------------------------------------

species <- readxl::read_excel(SPECIES_FILE, sheet = SPECIES_SHEET) %>%
  filter(!is.na(eppocode), !is.na(fullname)) %>%
  filter(batch == 3) %>%                     # first round: batch 3 only (93 species)
  distinct(eppocode, .keep_all = TRUE) %>%
  transmute(eppoCode = toupper(eppocode), scientificName = fullname, kingdom)

# GBIF backbone: exact/variant matches only, synonyms -> accepted key (occurrences
# are indexed under accepted taxa). Pathovars are not in the GBIF backbone
# (e.g. "X. citri pv. aurantifolii" resolves to the whole species), so they
# get no key rather than another pathovar's records.
gbif_match <- lapply(seq_len(nrow(species)), function(i) {
  nm <- species$scientificName[i]
  none <- tibble(gbifTaxonKey = NA_integer_, acceptedKey = NA_integer_,
                 gbifName = NA_character_, gbifRank = NA_character_,
                 gbifMatch = NA_character_)
  if (grepl("\\bpv\\.", nm)) return(mutate(none, gbifMatch = "PATHOVAR_SKIPPED"))
  m <- tryCatch(name_backbone(name = nm, kingdom = species$kingdom[i]),
                error = function(e) NULL)
  # VARIANT = same name, different authorship/spelling
  if (is.null(m) || !m$matchType %in% c("EXACT", "VARIANT")) {
    return(mutate(none, gbifMatch = if (is.null(m)) "ERROR" else m$matchType))
  }
  accepted <- if ("acceptedUsageKey" %in% names(m) && !is.na(m$acceptedUsageKey)) {
    m$acceptedUsageKey
  } else {
    m$usageKey
  }
  tibble(gbifTaxonKey = as.integer(m$usageKey), acceptedKey = as.integer(accepted),
         gbifName = m$scientificName, gbifRank = m$rank, gbifMatch = m$status)
})

species <- bind_cols(select(species, eppoCode, scientificName, kingdom),
                     bind_rows(gbif_match))

print(count(species, gbifMatch))   # review non-EXACT / SYNONYM rows before the download

message(sum(!is.na(species$acceptedKey)), " of ", nrow(species),
        " species have a GBIF key")

# -------------------------------------------------------------------------
# 4. GBIF occurrences (one search per species, cached)
# -------------------------------------------------------------------------

# GBIF's search is fast for the first 10,000 records of a query (300 records
# < 1 s) and very slow beyond (offset >= 10,000: ~16 s for 10 records; a
# 300-record page times out). rgbif pages internally with no timeout, so a
# species with > 10,000 records hung. So: split the query into latitude
# bands of at most BAND_MAX records (halving a band until it fits) and page
# each band from offset 0. Split points are uneven (not 30.0, 40.0) because
# band limits are inclusive: a record exactly on a round boundary would be
# fetched twice. Each page: 30 s limit, 2 attempts, then skipped and counted.
# limit is always the exact number of records wanted: rgbif 3.8.5 errors
# ("invalid subscript type 'list'") on a page asked for more than it holds.
BAND_MAX <- 9000

gbif_filter <- function(key, lat) {
  list(taxonKey = key, hasCoordinate = TRUE, hasGeospatialIssue = FALSE,
       occurrenceStatus = "PRESENT", decimalLatitude = sprintf("%.7f,%.7f", lat[1], lat[2]))
}

fetch_page <- function(key, lat, start, n) {
  for (attempt in 1:2) {
    res <- tryCatch(
      do.call(occ_data, c(gbif_filter(key, lat),
                          list(limit = n, start = start, curlopts = list(timeout_ms = 30000)))),
      error = function(e) e)
    if (!inherits(res, "error")) {
      p <- res$data
      if (is.null(p) || nrow(p) == 0) return(NULL)
      for (f in setdiff(GBIF_FIELDS, names(p))) p[[f]] <- NA
      return(as_tibble(p[, GBIF_FIELDS]) %>%
               mutate(across(c(coordinateUncertaintyInMeters, year), as.numeric),
                      across(c(basisOfRecord, establishmentMeans, countryCode), as.character)))
    }
  }
  message("  lat ", lat[1], "..", lat[2], ": records ", start, "-", start + n - 1,
          " skipped (", conditionMessage(res), ")")
  skipped_records <<- skipped_records + n
  NULL
}

fetch_band <- function(key, lat) {
  n <- do.call(occ_count, gbif_filter(key, lat))
  if (n == 0) return(NULL)
  if (n > BAND_MAX && diff(lat) > 1e-4) {
    mid <- lat[1] + diff(lat) * 0.5013   # uneven split point
    return(bind_rows(fetch_band(key, c(lat[1], mid)),
                     fetch_band(key, c(mid + 1e-7, lat[2]))))
  }
  if (n > 10000) {   # > 10,000 records within 0.0001 deg: cannot page past 10,000
    warning("taxonKey ", key, ": lat ", lat[1], "..", lat[2], " has ", n,
            " records, only 10,000 fetched")
    n <- 10000
  }
  if (n > 3000) message("  lat ", round(lat[1], 2), "..", round(lat[2], 2), ": ", n, " records")
  starts <- seq(0, n - 1, by = 300)
  bind_rows(lapply(starts, function(s) fetch_page(key, lat, s, min(300, n - s))))
}

fetch_gbif <- function(key) {
  skipped_records <<- 0
  d <- fetch_band(key, c(-90, 90))
  if (skipped_records > 0) {
    warning("taxonKey ", key, ": ", skipped_records, " records could not be fetched")
  }
  if (is.null(d) || nrow(d) == 0) NULL else d
}

to_fetch <- filter(species, !is.na(acceptedKey))

occ <- lapply(seq_len(nrow(to_fetch)), function(i) {
  cache <- file.path(GBIF_CACHE_DIR, paste0(to_fetch$eppoCode[i], ".rds"))
  if (!file.exists(cache)) {
    message(i, "/", nrow(to_fetch), " ", to_fetch$scientificName[i])
    # a failed fetch is not cached, so a re-run retries only those species
    d <- tryCatch(fetch_gbif(to_fetch$acceptedKey[i]), error = function(e) {
      warning(to_fetch$eppoCode[i], ": GBIF fetch failed, re-run to retry (",
              conditionMessage(e), ")")
      "failed"
    })
    if (identical(d, "failed")) return(NULL)
    saveRDS(d, cache)
  }
  d <- readRDS(cache)
  if (!is.null(d)) mutate(d, eppoCode = to_fetch$eppoCode[i], .before = 1)
}) %>%
  bind_rows()

n_downloaded <- count(occ, eppoCode, name = "nDownloaded")

# -------------------------------------------------------------------------
# 5. Cleaning
# -------------------------------------------------------------------------

occ_clean <- occ %>%
  filter(!basisOfRecord %in% DROP_BASIS,
         !establishmentMeans %in% DROP_ESTABLISHMENT,
         is.na(coordinateUncertaintyInMeters) |
           coordinateUncertaintyInMeters <= MAX_UNCERTAINTY_M) %>%
  as.data.frame() %>%
  clean_coordinates(lon = "decimalLongitude", lat = "decimalLatitude",
                    species = "eppoCode", tests = CC_TESTS, value = "clean")

n_clean <- count(occ_clean, eppoCode, name = "nClean")

# -------------------------------------------------------------------------
# 6. KG class per point, thin to one row per raster cell
# -------------------------------------------------------------------------

xy <- as.matrix(occ_clean[, c("decimalLongitude", "decimalLatitude")])
occ_clean$cell    <- cellFromXY(kg, xy)
occ_clean$kgValue <- kg[occ_clean$cell][, 1]

occurrences <- occ_clean %>%
  filter(!is.na(kgValue), kgValue > 0) %>%          # sea / outside raster
  group_by(eppoCode, cell, kgValue, basisOfRecord, countryCode) %>%
  summarise(nRecords  = n(),
            firstYear = suppressWarnings(min(year, na.rm = TRUE)),
            lastYear  = suppressWarnings(max(year, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(across(c(firstYear, lastYear), ~ ifelse(is.finite(.x), .x, NA))) %>%
  left_join(select(kg_legend, kgValue, kgCode), by = "kgValue") %>%
  select(-kgValue)

cell_xy <- xyFromCell(kg, occurrences$cell)
occurrences$lon <- cell_xy[, 1]
occurrences$lat <- cell_xy[, 2]

# One row per non-alpine cell (occurrences has one per cell, basisOfRecord
# and country); a border cell keeps its first country
cells <- occurrences %>%
  filter(!kgCode %in% EXCLUDE_KG) %>%
  group_by(eppoCode, cell, kgCode, lon, lat) %>%
  summarise(lastYear    = suppressWarnings(max(lastYear, na.rm = TRUE)),
            countryCode = first(countryCode), .groups = "drop") %>%
  mutate(recent = is.finite(lastYear) & lastYear >= 2000)

pest_kg_classes <- occurrences %>%
  filter(!kgCode %in% EXCLUDE_KG) %>%
  group_by(eppoCode, kgCode) %>%
  summarise(nCells = n_distinct(cell), nRecords = sum(nRecords), .groups = "drop") %>%
  left_join(count(filter(cells, recent), eppoCode, kgCode, name = "nCellsSince2000"),
            by = c("eppoCode", "kgCode")) %>%
  mutate(nCellsSince2000 = coalesce(nCellsSince2000, 0L))

# Same rule as the occupiedKgClasses view; n = cells per class of one species
is_occupied <- function(n) n >= OCC_MIN_CELLS & n >= OCC_MIN_SHARE * sum(n)

occupied <- pest_kg_classes %>%
  group_by(eppoCode) %>%
  filter(is_occupied(nCells)) %>%
  ungroup()

# Share of random halves of the cells that give the same occupied classes
# among those found in Norway (what EST1 depends on; a tropical class near
# the threshold does not matter). The absolute floor is halved for a half
# sample, otherwise a 9-cell class would fail half the time from halving
# alone. NA: no occupied class at all.
class_stability <- function(kg_codes) {
  occupied_set <- function(k, floor) {
    t <- table(k)
    n <- as.vector(t)
    names(t)[n >= floor & n >= OCC_MIN_SHARE * sum(n)]
  }
  all_full <- occupied_set(kg_codes, OCC_MIN_CELLS)
  if (length(all_full) == 0) return(NA_real_)
  full <- intersect(all_full, kg_norway$kgCode)
  mean(replicate(STAB_RUNS, {
    half <- occupied_set(sample(kg_codes, length(kg_codes) %/% 2), OCC_MIN_CELLS / 2)
    setequal(intersect(half, kg_norway$kgCode), full)
  }))
}

# Europe = Natural Earth continent; Russia only west of the Urals (60 E)
europe <- with(ne_countries(scale = "medium", returnclass = "sf"),
               iso_a2_eh[continent == "Europe"])

set.seed(1)
species_summary <- cells %>%
  group_by(eppoCode) %>%
  summarise(nCellsNonAlpine   = n(),
            nCellsNorway      = sum(countryCode %in% "NO"),
            nCellsNordic      = sum(countryCode %in% NORDIC),
            nordicCountries   = paste(sort(unique(countryCode[countryCode %in% NORDIC])),
                                      collapse = ", "),
            nCellsEurope      = sum(countryCode %in% europe &
                                      !(countryCode %in% "RU" & lon > 60)),
            pctCellsSince2000 = round(100 * mean(recent), 1),
            classStability    = class_stability(kgCode),
            .groups = "drop") %>%
  left_join(cells %>%
              semi_join(occupied, by = c("eppoCode", "kgCode")) %>%
              group_by(eppoCode) %>%
              # poleward edge, robust to stray points (absolute latitude)
              summarise(lat95 = round(quantile(abs(lat), 0.95, names = FALSE), 1),
                        borealOccupied = as.integer(any(kgCode %in% BOREAL_KG))),
            by = "eppoCode")

species_out <- species %>%
  left_join(n_downloaded, by = "eppoCode") %>%
  left_join(n_clean, by = "eppoCode") %>%
  left_join(count(distinct(occurrences, eppoCode, cell), eppoCode, name = "nCells"),
            by = "eppoCode") %>%
  left_join(species_summary, by = "eppoCode") %>%
  mutate(across(c(nDownloaded, nClean, nCells, nCellsNonAlpine, nCellsNorway,
                  nCellsNordic, nCellsEurope), ~ coalesce(as.integer(.x), 0L)),
         uncertainty = case_when(
           is.na(classStability) ~ "no evidence",   # no occupied class
           nCellsNonAlpine >= UNC_LOW_CELLS & classStability >= 0.9 ~ "low",
           nCellsNonAlpine >= UNC_MEDIUM_CELLS & classStability >= 0.5 ~ "medium",
           TRUE ~ "high"))

# Keep only species with at least one occupied climate class. The rest (no
# GBIF match, or too few records) carry no Köppen-Geiger evidence: absent
# from kg_climate.db = no usable GBIF climate evidence.
dropped     <- filter(species_out, uncertainty == "no evidence")
species_out <- filter(species_out, uncertainty != "no evidence")
occurrences     <- semi_join(occurrences, species_out, by = "eppoCode")
pest_kg_classes <- semi_join(pest_kg_classes, species_out, by = "eppoCode")
message("Dropped ", nrow(dropped), " species without an occupied climate class: ",
        paste(dropped$eppoCode, collapse = ", "))

# -------------------------------------------------------------------------
# 7. Write kg_climate.db
# -------------------------------------------------------------------------

build_info <- tibble(
  key   = c("buildDate", "kgSource", "kgRaster", "gbifSource", "speciesFile",
            "maxUncertaintyM", "droppedBasis", "droppedEstablishment", "ccTests",
            "excludedKg", "occMinCells", "occMinShare", "nordic", "borealKg",
            "stabRuns", "uncMediumCells", "uncLowCells", "kgFuture"),
  value = c(format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
            "Beck et al. 2023, Scientific Data, 1991-2020",
            basename(KG_RASTER),
            "GBIF.org occurrence search API (rgbif::occ_data); fetch date = cache file dates",
            SPECIES_FILE, MAX_UNCERTAINTY_M,
            paste(DROP_BASIS, collapse = ", "),
            paste(DROP_ESTABLISHMENT, collapse = ", "),
            paste(CC_TESTS, collapse = ", "),
            paste(EXCLUDE_KG, collapse = ", "),
            OCC_MIN_CELLS, OCC_MIN_SHARE,
            paste(NORDIC, collapse = ", "), paste(BOREAL_KG, collapse = ", "),
            STAB_RUNS, UNC_MEDIUM_CELLS, UNC_LOW_CELLS,
            "2041-2070, SSP5-8.5")
)

con <- dbConnect(SQLite(), OUT_DB)
dbWriteTable(con, "kgLegend",      kg_legend,       overwrite = TRUE)
dbWriteTable(con, "kgNorway",      kg_norway,       overwrite = TRUE)
dbWriteTable(con, "kgNorwayFuture", kg_norway_future, overwrite = TRUE)
dbWriteTable(con, "species",      species_out,     overwrite = TRUE)
dbWriteTable(con, "occurrences",   occurrences,     overwrite = TRUE)
dbWriteTable(con, "pestKgClasses", pest_kg_classes, overwrite = TRUE)
dbWriteTable(con, "buildInfo",     build_info,      overwrite = TRUE)
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_occ_eppo ON occurrences (eppoCode)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_kgc_eppo ON pestKgClasses (eppoCode)")
dbExecute(con, "DROP VIEW IF EXISTS occupiedKgClasses")
dbExecute(con, sprintf("
  CREATE VIEW occupiedKgClasses AS
  SELECT p.eppoCode, p.kgCode, p.nCells
  FROM pestKgClasses p
  JOIN (SELECT eppoCode, SUM(nCells) AS tot FROM pestKgClasses GROUP BY eppoCode) t
    USING (eppoCode)
  WHERE p.nCells >= %d AND p.nCells >= %f * t.tot", OCC_MIN_CELLS, OCC_MIN_SHARE))
dbDisconnect(con)

message("Wrote ", OUT_DB, ": ", nrow(species_out), " species, ",
        nrow(occurrences), " occurrence cells")

# -------------------------------------------------------------------------
# 8. Inspect kg_climate.db (can be run on its own, read-only)
# -------------------------------------------------------------------------

con <- dbConnect(SQLite(), OUT_DB, flags = SQLITE_RO)

# Row counts per table
sapply(dbListTables(con), function(t) dbGetQuery(con, paste0("SELECT COUNT(*) n FROM ", t))$n)

dbReadTable(con, "buildInfo")
dbReadTable(con, "kgNorway")
dbReadTable(con, "kgNorwayFuture")

# GBIF match and data per species; species without usable points first
dbGetQuery(con, "SELECT eppoCode, scientificName, gbifMatch, nDownloaded, nClean, nCells
                 FROM species ORDER BY nCells, eppoCode")

# EST1 evidence per species: proximity to Norway, poleward edge, recency,
# uncertainty of the climate-class list
dbGetQuery(con, "SELECT eppoCode, nCellsNonAlpine AS cells, nCellsNorway AS NO,
                        nCellsNordic AS nordic, nordicCountries, nCellsEurope AS europe,
                        lat95, borealOccupied AS boreal, pctCellsSince2000 AS pctRecent,
                        classStability AS stability, uncertainty
                 FROM species ORDER BY nCellsNonAlpine DESC")

# Climate classes per species (most cells first)
dbGetQuery(con, "SELECT eppoCode, GROUP_CONCAT(kgCode || ':' || nCells, ' ') AS classes
                 FROM (SELECT * FROM pestKgClasses ORDER BY eppoCode, nCells DESC)
                 GROUP BY eppoCode")

# Share of non-alpine Norway lying in classes the species occupies
dbGetQuery(con, "SELECT p.eppoCode,
                        GROUP_CONCAT(n.kgCode, ', ')       AS sharedClasses,
                        ROUND(SUM(n.proportion) * 100, 1)  AS pctNorway,
                        ROUND(SUM(n.areaKm2))              AS km2Norway
                 FROM occupiedKgClasses p JOIN kgNorway n ON n.kgCode = p.kgCode
                 GROUP BY p.eppoCode ORDER BY pctNorway DESC")

# One species: points and their climate class
dbGetQuery(con, "SELECT * FROM occurrences WHERE eppoCode = 'DACUCT' LIMIT 20")

dbDisconnect(con)

# -------------------------------------------------------------------------
# 9. Example map: one species on the Köppen-Geiger layer
# -------------------------------------------------------------------------

library(ggplot2)

PLOT_EPPO <- "MALAAM"

con <- dbConnect(SQLite(), OUT_DB, flags = SQLITE_RO)
pts <- dbGetQuery(con, "SELECT * FROM occurrences WHERE eppoCode = ?", params = list(PLOT_EPPO))
sp  <- dbGetQuery(con, "SELECT scientificName FROM species WHERE eppoCode = ?", params = list(PLOT_EPPO))
occ <- dbGetQuery(con, "SELECT * FROM occupiedKgClasses WHERE eppoCode = ?", params = list(PLOT_EPPO))
leg <- dbReadTable(con, "kgLegend")
rule <- with(dbReadTable(con, "buildInfo"), setNames(value, key))
dbDisconnect(con)
if (nrow(pts) == 0) stop(PLOT_EPPO, " has no occurrences in ", OUT_DB)

# Whole world, only the occupied classes (view occupiedKgClasses); all points shown
# 0.5 deg version of the same map: a ggplot keeps its data, and at 0.1 deg
# the plot object is ~1.4 GB, which RStudio times out saving to its plot
# history ("reached elapsed time limit"). 0.5 deg: ~70 MB, renders in ~2 s.
kg_df <- rast(file.path(KG_DIR, "koppen_geiger_0p5.tif")) %>%
  as.data.frame(xy = TRUE, na.rm = FALSE) %>%
  setNames(c("x", "y", "kgValue")) %>%
  left_join(select(leg, kgValue, kgCode), by = "kgValue") %>%
  # blank out other classes (not drop: geom_raster needs the full regular grid)
  transmute(x, y, kgCode = if_else(kgCode %in% occ$kgCode, kgCode, NA_character_))

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin != "Antarctica")

ggplot() +
  geom_sf(data = world, fill = "grey92", colour = NA) +
  geom_raster(data = kg_df, aes(x, y, fill = kgCode), na.rm = TRUE) +
  scale_fill_manual(values = setNames(leg$hex, leg$kgCode), name = "Köppen-Geiger",
                    na.value = NA, na.translate = FALSE) +
  geom_sf(data = world, fill = NA, colour = "grey40", linewidth = 0.1) +
  # points in occupied classes filled; points in classes below the rule hollow
  geom_point(data = mutate(pts, status = if_else(kgCode %in% occ$kgCode, "occupied class", "below threshold")),
             aes(lon, lat, shape = status), fill = "red", size = 2, stroke = 0.3) +
  scale_shape_manual(values = c("occupied class" = 21, "below threshold" = 1), name = NULL) +
  coord_sf(ylim = c(-58, 85), expand = FALSE) +
  labs(title = bquote(italic(.(sp$scientificName)) ~ "(" * .(PLOT_EPPO) * ")"),
       subtitle = sprintf("%d occupied 1 km cells (GBIF, cleaned); shaded: KG classes with >= %d cells and >= %.1f%% of cells (Beck et al. 2023, 1991-2020)",
                          n_distinct(pts$cell), as.integer(rule[["occMinCells"]]),
                          100 * as.numeric(rule[["occMinShare"]])),
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ggsave(paste0("kg_", PLOT_EPPO, ".png"), width = 30, height = 18, units = "cm", dpi = 300, bg = "white")

# -------------------------------------------------------------------------
# TODO (draft)
# - Python EST1 lookup: read occupiedKgClasses; a species with no occupied
#   class has no usable GBIF climate evidence.
# - Viewer: app.R in this folder
# -------------------------------------------------------------------------
