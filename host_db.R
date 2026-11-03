# host_db.R  (DRAFT)
# Adds host tree ranges to kg_climate.db (built by kg_db.R, which this script
# does not change). For every species in the db: its EPPO host list, matched
# to the Caudullo et al. 2017 chorological maps by exact species name, and the
# merged natural range of its matched hosts.
# Used by app.R: the chosen host's range as a dashed outline over the
# (unchanged) climate map.
#
# Run from the repo root after kg_db.R. kg_db.R only replaces its own tables,
# so the host tables survive a rebuild; re-run this script when the species
# list changes.
#
# Data:
#   - Caudullo, G., Welk, E., San-Miguel-Ayanz, J., 2017. Chorological maps
#     for the main European woody species. Data in Brief 12, 662-666.
#     CC BY 4.0. One zip per species; set CHO_DIR below.
#   - EPPO Global Database host lists, public CSV per pest (no token).
#     Cached in eppo_cache/; delete a file to fetch it again.
#
# Matching rules:
#   - exact species only: the EPPO host name must equal a Caudullo species
#     name. Genus entries ("Pinus") and subspecies/varieties are not matched.
#   - host types kept: HOST_TYPES (not Experimental, Doubtful host, Non-host)
#   - natural range only: per map part, the coast-clipped polygons
#     (_plg_clip) if present, else _plg, plus isolated populations (_pnt).
#     Planted/naturalised layers (syn_) are not used.
#
# Tables written to kg_climate.db:
#   pestHosts     (eppoCode, hostName, hostEppoCode, hostType)
#   hostRange     (rangeId, layer = "range" | "edge", wkt)  EPSG:4326, section 3.
#                 rangeId: a host name, or "all:<eppoCode>" for all hosts of
#                 a pest with several hosts combined
#   hostInfo      (key, value)   source, citation, DOI, host list date

library(DBI)
library(RSQLite)
library(dplyr)
library(terra)

# -------------------------------------------------------------------------
# Configuration
# -------------------------------------------------------------------------

DB_PATH        <- "kg_climate.db"
CHO_DIR        <- "data/shp/cho"   # Caudullo zips; rpp/ and mhs/ beside it are not used
EPPO_CACHE_DIR <- "eppo_cache"
EPPO_HOSTS_URL <- "https://gd.eppo.int/taxon/%s/download/hosts_csv"

HOST_TYPES <- c("Major host", "Host", "Wild/Weed")   # order = rank when a pair repeats

# Geometry simplification (degrees, ~1 km). Keeps the stored WKT small; the
# ranges are only drawn, not used for statistics.
SIMPLIFY_DEG <- 0.01

POINT_BUFFER_M <- 10000   # isolated populations drawn as discs of this radius
EDGE_COAST_M   <- 3000    # range edge within this distance of the coast not drawn

source("R/hosts.R")   # choose_cho_layers(), host_key()

# -------------------------------------------------------------------------
# 1. EPPO host lists (cached)
# -------------------------------------------------------------------------

con <- dbConnect(SQLite(), DB_PATH)
pests <- dbGetQuery(con, "SELECT eppoCode FROM species")$eppoCode

dir.create(EPPO_CACHE_DIR, showWarnings = FALSE)
for (code in pests) {
  f <- file.path(EPPO_CACHE_DIR, paste0(code, ".csv"))
  if (file.exists(f)) next
  download.file(sprintf(EPPO_HOSTS_URL, code), f, quiet = TRUE,
                headers = c("User-Agent" = "kg_viewer host_db.R"))
  Sys.sleep(1)
}

# References can span lines; read.csv handles the quoted line breaks
eppo_hosts <- bind_rows(lapply(pests, function(code) {
  d <- read.csv(file.path(EPPO_CACHE_DIR, paste0(code, ".csv")),
                stringsAsFactors = FALSE, encoding = "UTF-8")
  stopifnot(all(c("type", "EPPOCode", "Pref_name") %in% names(d)))
  tibble(eppoCode = code, hostType = d$type, hostEppoCode = d$EPPOCode,
         eppoName = d$Pref_name)
}))

# -------------------------------------------------------------------------
# 2. Match to the Caudullo species (exact name)
# -------------------------------------------------------------------------

cho <- tibble(zip      = list.files(CHO_DIR, "_shp[.]zip$", full.names = TRUE),
              hostName = sub("_", " ", sub("_shp[.]zip$", "", basename(zip))),
              key      = host_key(hostName))

pest_hosts <- eppo_hosts %>%
  filter(hostType %in% HOST_TYPES) %>%
  mutate(key = host_key(eppoName)) %>%
  inner_join(select(cho, key, hostName), by = "key") %>%
  # one row per pest-host pair, best-ranked type
  arrange(eppoCode, hostName, match(hostType, HOST_TYPES)) %>%
  distinct(eppoCode, hostName, .keep_all = TRUE) %>%
  select(eppoCode, hostName, hostEppoCode, hostType)

print(count(pest_hosts, eppoCode, sort = TRUE), n = Inf)

# -------------------------------------------------------------------------
# 3. One host range per pest, ready to draw
# -------------------------------------------------------------------------
# The app shows one range at a time: a single host, or all hosts of the pest
# combined (outlines of up to 22 overlapping hosts are unreadable). The
# Caudullo maps are whole natural ranges (some circumpolar), so outside them
# the listed hosts are absent; other hosts of the pest may not be. Per range:
#   range: union of its hosts' natural ranges, clipped to land (hover label)
#   edge:  range boundary on land only; where the range meets the sea the
#          app's coastline already shows the edge
# Isolated populations (points) become small discs of POINT_BUFFER_M.

read_host <- function(zip) {
  dir <- file.path(tempdir(), sub("[.]zip$", "", basename(zip)))
  unzip(zip, exdir = dir)
  parts <- lapply(file.path(dir, choose_cho_layers(list.files(dir, "[.]shp$"))),
                  function(f) {
                    v <- vect(f)
                    values(v) <- NULL
                    if (geomtype(v) == "points") buffer(v, POINT_BUFFER_M) else makeValid(v)
                  })
  unlink(dir, recursive = TRUE)
  aggregate(do.call(rbind, parts))
}

matched    <- filter(cho, hostName %in% pest_hosts$hostName)
host_geoms <- setNames(lapply(matched$zip, read_host), matched$hostName)

# Land in the app's map band (KG maps cropped to 60S-85N)
land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  vect() %>% makeValid() %>% crop(ext(-180, 180, -60, 85)) %>% aggregate() %>%
  simplifyGeom(tolerance = SIMPLIFY_DEG, preserveTopology = TRUE)
coast_zone <- buffer(as.lines(land), EDGE_COAST_M)

to_wkt <- function(v) {
  if (is.null(v) || nrow(v) == 0 || all(is.empty(v))) return(NA_character_)
  sf::st_as_text(sf::st_geometry(sf::st_as_sf(aggregate(v)))[[1]], digits = 5)
}

# One range from one or more host geometries: outline layers
build_range <- function(id, geoms) {
  rng <- aggregate(do.call(rbind, unname(geoms)))
  rng <- simplifyGeom(rng, tolerance = SIMPLIFY_DEG, preserveTopology = TRUE)
  rng <- crop(rng, land)
  coast <- crop(coast_zone, ext(rng) + 0.5)   # only the nearby coast
  tibble(rangeId = id, layer = c("range", "edge"),
         wkt = c(to_wkt(rng), to_wkt(erase(as.lines(rng), coast))))
}

# Ranges the app can show: each host on its own (rangeId = host name), and
# for pests with several hosts all of them combined (rangeId = "all:<eppo>")
codes <- unique(pest_hosts$eppoCode)
multi <- names(which(table(pest_hosts$eppoCode) > 1))
built <- c(lapply(names(host_geoms), function(h) build_range(h, host_geoms[h])),
           lapply(multi, function(code) {
             build_range(paste0("all:", code),
                         host_geoms[pest_hosts$hostName[pest_hosts$eppoCode == code]])
           }))
host_range <- filter(bind_rows(built), !is.na(wkt))

# -------------------------------------------------------------------------
# 4. Checks (values from the 2026-11-03 coverage count; EPPO lists can
#    change, so a failure here means "look", not necessarily "bug")
# -------------------------------------------------------------------------

stopifnot(
  n_distinct(pest_hosts$eppoCode) == 18,
  !"IPSXPI" %in% pest_hosts$eppoCode,                       # hosts only by genus
  setequal(pest_hosts$hostName[pest_hosts$eppoCode == "GNOMUL"],
           c("Ulmus glabra", "Ulmus laevis", "Ulmus minor")),
  # every host and every combined range is drawable
  setequal(host_range$rangeId[host_range$layer == "range"],
           c(unique(pest_hosts$hostName), paste0("all:", multi)))
)

# -------------------------------------------------------------------------
# 5. Write
# -------------------------------------------------------------------------

host_info <- tibble(
  key   = c("hostSource", "hostCitation", "hostDoi", "hostListSource",
            "hostListDate", "hostTypes", "hostMatch"),
  value = c("Caudullo et al. 2017, chorological maps, natural range",
            paste("Caudullo, G., Welk, E., San-Miguel-Ayanz, J. (2017) Chorological",
                  "maps for the main European woody species. Data in Brief 12, 662-666."),
            "10.1016/j.dib.2017.05.007",
            "EPPO Global Database, https://gd.eppo.int",
            format(max(file.mtime(list.files(EPPO_CACHE_DIR, full.names = TRUE))), "%Y-%m-%d"),
            paste(HOST_TYPES, collapse = ", "),
            "exact species name; no genus or subspecies matches")
)

for (old in c("hostRanges", "hostMask", "pestHostRange", "pestHostCells", "hostCells")) {   # earlier layouts
  if (dbExistsTable(con, old)) dbRemoveTable(con, old)
}
dbWriteTable(con, "pestHosts",  pest_hosts, overwrite = TRUE)
dbWriteTable(con, "hostRange",  host_range, overwrite = TRUE)
dbWriteTable(con, "hostInfo",   host_info,  overwrite = TRUE)
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_ph_eppo ON pestHosts (eppoCode)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_hr_id ON hostRange (rangeId)")
dbExecute(con, "VACUUM")
dbDisconnect(con)

message("Wrote ", length(codes), " pests, ",
        n_distinct(pest_hosts$hostName), " hosts, ", length(multi), " combined ranges, ",
        format(sum(nchar(host_range$wkt)) / 1e6, digits = 2), " MB of outlines")
