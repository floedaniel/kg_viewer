# Helpers for host_db.R (host ranges from Caudullo et al. 2017)

# Shapefiles to read from one Caudullo zip: natural range only. A zip can
# hold several parts (subspecies, e.g. Pinus_nigra_laricio_*); per part the
# coast-clipped polygons if present, else the unclipped ones, plus isolated
# populations (_pnt). Planted/naturalised layers (syn_) are skipped.
choose_cho_layers <- function(files) {
  files <- files[!grepl("_syn_", files)]
  stem  <- sub("_(plg_clip|plg|pnt)[.]shp$", "", files)
  keep  <- grepl("_pnt[.]shp$", files) |
    grepl("_plg_clip[.]shp$", files) |
    (grepl("_plg[.]shp$", files) & !paste0(stem, "_plg_clip.shp") %in% files)
  sort(files[keep])
}

# Name key for exact species matching: case and hyphens ignored (Caudullo
# file names drop hyphens, e.g. Abies borisiiregis = Abies borisii-regis).
# Subspecies and varieties keep their extra words, so they never match.
host_key <- function(name) tolower(gsub("-", "", trimws(name)))
