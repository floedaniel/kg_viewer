# Static map for the docs: the classes a species occupies, tinted by data
# support, with GBIF cells, plus a class x support grid legend with n and
# Norway share. Same design as the app. Needs R/support.R sourced.
library(DBI)
library(dplyr)
library(ggplot2)
library(patchwork)
library(terra)
library(rnaturalearth)

support_map <- function(db, kg_tif, eppo, title = TRUE) {
  dash <- intToUtf8(0x2013)

  con <- dbConnect(RSQLite::SQLite(), db, flags = RSQLite::SQLITE_RO)
  on.exit(dbDisconnect(con))
  legend <- dbReadTable(con, "kgLegend")
  sp  <- dbGetQuery(con, "SELECT scientificName FROM species WHERE eppoCode = ?", list(eppo))
  cls <- dbGetQuery(con, "
    SELECT o.kgCode, o.nCells, COALESCE(n.proportion, 0) AS norway
    FROM occupiedKgClasses o LEFT JOIN kgNorway n USING (kgCode)
    WHERE o.eppoCode = ? ORDER BY o.nCells DESC", list(eppo))
  pts <- dbGetQuery(con, "SELECT DISTINCT cell, lon, lat FROM occurrences WHERE eppoCode = ?",
                    list(eppo))

  cls <- cls %>%
    left_join(select(legend, kgCode, kgValue, hex), by = "kgCode") %>%
    mutate(support = support_level(nCells),
           shown   = tint(hex, SUPPORT_TINT[support]),
           row     = factor(kgCode, rev(kgCode)),
           nLabel  = ifelse(norway > 0, scales::percent(norway, 0.1), dash),
           nFace   = ifelse(norway > 0, "bold", "plain"))

  # 0.1 deg map at 0.2 deg (modal keeps the class values)
  kg_df <- rast(kg_tif) %>%
    crop(ext(-180, 180, -58, 85)) %>%
    aggregate(fact = 2, fun = "modal") %>%
    as.data.frame(xy = TRUE) %>%
    setNames(c("x", "y", "kgValue")) %>%
    inner_join(select(cls, kgValue, shown), by = "kgValue")

  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    filter(admin != "Antarctica")

  map <- ggplot() +
    geom_sf(data = world, fill = "grey90", colour = NA) +
    geom_raster(data = kg_df, aes(x, y, fill = shown)) +
    geom_sf(data = world, fill = NA, colour = "grey70", linewidth = 0.05) +
    geom_point(data = pts, aes(lon, lat), shape = 16, size = 0.35, colour = "black") +
    scale_fill_identity() +
    coord_sf(ylim = c(-58, 85), expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 10) +
    theme(panel.grid = element_blank())

  grid <- cross_join(select(cls, row, hex, own = support), tibble(col = 1:3)) %>%
    mutate(fill = tint(hex, SUPPORT_TINT[col]))

  leg <- ggplot(grid, aes(col, row)) +
    geom_tile(aes(fill = fill), colour = "white", linewidth = 1) +
    geom_tile(data = filter(grid, col == own), fill = NA, colour = "black", linewidth = 0.7,
              width = 0.92, height = 0.88) +
    geom_text(data = cls, aes(x = 3.7, y = row, label = nLabel, fontface = nFace),
              hjust = 0, size = 2.8, inherit.aes = FALSE) +
    annotate("text", x = 3.7, y = nrow(cls) + 0.75, label = "Norway",
             hjust = 0, size = 2.8, fontface = "bold") +
    scale_fill_identity() +
    scale_x_continuous(breaks = 1:3, labels = SUPPORT_LABELS,
                       expand = expansion(add = c(0.5, 1.3))) +
    scale_y_discrete(labels = setNames(sprintf("%s  (n = %s)", cls$kgCode,
                                               format(cls$nCells, big.mark = ",", trim = TRUE)),
                                       cls$row)) +
    coord_cartesian(clip = "off") +
    labs(x = "Data support (n = 1 km cells)", y = NULL) +
    theme_minimal(base_size = 9) +
    theme(panel.grid = element_blank(), axis.title = element_text(size = 8.5))

  p <- map + leg + plot_layout(widths = c(4, 1.05))
  if (title) {
    p <- p + plot_annotation(
      title = bquote("Occupied climate classes + data support (1991" * .(dash) * "2020) -" ~
                       italic(.(sp$scientificName))),
      subtitle = paste0("Colour = climate class the species occupies; lighter tint = fewer GBIF cells ",
                        "in the class. Black dots: GBIF cells. Outlined legend cell = the class's ",
                        "level of data support."),
      theme = theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
                    plot.subtitle = element_text(size = 8.5, colour = "grey30", hjust = 0.5)))
  }
  p & theme(plot.background = element_rect(fill = "white", colour = NA))
}
