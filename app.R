# kg_viewer  (DRAFT)
# Choose a species -> its GBIF occurrences on the Köppen-Geiger map, the
# climate classes it occupies and the share of non-alpine Norway in them.
# Background for manual EST1 assessments.
#
# Reads kg_climate.db in this folder, built by kg_db.R (also in this folder).
# Run from the project root:  shiny::runApp("scripts/kg_viewer")

library(shiny)
library(bslib)
library(leaflet)
library(DT)
library(terra)
library(png)    # shading image, see kg_image()
library(DBI)
library(RSQLite)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)   # country outlines; loaded explicitly so deployment includes it

# runApp sets this folder as working directory
DB_PATH <- "kg_climate.db"

# Beck et al. 2023, 0.1 deg versions (CC BY 4.0), copied into this folder so
# the app also runs on shinyapps.io. Names are the period choices in the UI.
KG_MAPS <- c("1991-2020"                       = "koppen_geiger_0p1.tif",
             "2041-2070 (SSP5-8.5, worst case)" = "koppen_geiger_2041_2070_ssp585_0p1.tif")

db_query <- function(sql, params = NULL) {
  con <- dbConnect(SQLite(), DB_PATH, flags = SQLITE_RO)
  on.exit(dbDisconnect(con))
  dbGetQuery(con, sql, params = params)
}

kg_legend <- db_query("SELECT * FROM kgLegend ORDER BY kgValue")
species   <- db_query("SELECT eppoCode, scientificName, gbifMatch, nCells
                       FROM species ORDER BY scientificName")
build     <- with(db_query("SELECT * FROM buildInfo"), setNames(value, key))

rule_text <- sprintf("at least %s cells and at least %.1f%% of the species' cells",
                     build[["occMinCells"]], 100 * as.numeric(build[["occMinShare"]]))

# Web Mercator once at start-up; nearest neighbour keeps the class values.
# Antarctica cropped (no pests, and Mercator blows it up).
to_mercator <- function(f) {
  rast(f) %>%
    crop(ext(-180, 180, -60, 85)) %>%
    project("EPSG:3857", method = "near")
}

# The shading image is built here instead of by leaflet::addRasterImage(),
# which turns all ~7 million cells into colour strings per species (~600 MB
# peak; out of memory on shinyapps.io's 1 GB). Instead: keep each cell's class
# as a column index into a small RGBA lookup (index 1 = no class), and per
# species fill the lookup and index it (~30 MB of raw bytes). Indexes are
# stored as raw (1 byte per cell, ~7 MB per map; classes go up to 30).
kg_index <- function(kg_map) {
  v <- as.integer(values(kg_map))
  v[is.na(v) | v < 1L] <- 0L
  as.raw(v + 1L)
}
# One map at a time keeps the start-up peak low
kg_idx  <- lapply(KG_MAPS, function(f) kg_index(to_mercator(f)))
kg_grid <- to_mercator(rast(rast(KG_MAPS[[1]])))   # geometry only, same for both periods
kg_dim  <- c(4L, ncol(kg_grid), nrow(kg_grid))
kg_ll   <- ext(project(as.points(ext(kg_grid), crs = crs(kg_grid)), "EPSG:4326"))
kg_latlng <- list(list(ymax(kg_ll), xmin(kg_ll)), list(ymin(kg_ll), xmax(kg_ll)))
stopifnot(length(kg_idx[[1]]) == prod(kg_dim[2:3]))

kg_image <- function(occ_values, period) {
  rgba <- matrix(as.raw(0), 4, max(kg_legend$kgValue) + 1)   # transparent
  hex  <- kg_legend$hex[match(occ_values, kg_legend$kgValue)]
  rgba[, occ_values + 1L] <- as.raw(rbind(col2rgb(hex), 255))
  tile <- rgba[, as.integer(kg_idx[[period]])]
  dim(tile) <- kg_dim
  xfun::base64_uri(png::writePNG(tile), "image/png")
}

# Countries drawn by the app itself (no web tiles: they may be blocked)
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin != "Antarctica")

species_choices <- setNames(
  species$eppoCode,
  sprintf("%s (%s) - %s cells", species$scientificName, species$eppoCode,
          format(species$nCells, big.mark = ","))
)

ui <- page_sidebar(
  title = "Köppen-Geiger climate of pest occurrences",
  sidebar = sidebar(
    width = 460,
    # selectize filters the list as you type (scientific name or EPPO code)
    selectizeInput("eppo", "Species (click and type to search name or EPPO code)",
                   choices = species_choices,
                   options = list(placeholder = "Type a name or EPPO code...")),
    radioButtons("period", "Climate map", choices = names(KG_MAPS)),
    uiOutput("summary"),
    DTOutput("classes"),
    tags$small(
      sprintf("Occupied class: %s, from present-day records. ", rule_text),
      "Climate: Beck et al. 2023, 1991-2020 and 2041-2070 (SSP5-8.5). ",
      sprintf("Occurrences: GBIF, cleaned, one point per 1 km cell. Built %s.",
              substr(build[["buildDate"]], 1, 10))
    )
  ),
  tags$style(".leaflet-container { background: #dce9f2; }"),   # ocean
  card(full_screen = TRUE, leafletOutput("map", height = "80vh"))
)

server <- function(input, output, session) {

  pts <- reactive({
    # one marker per cell (occurrences has one row per cell and basisOfRecord)
    db_query("SELECT o.cell, o.lon, o.lat, o.kgCode, l.kgLabel,
                     GROUP_CONCAT(o.basisOfRecord, ', ') AS basisOfRecord,
                     SUM(o.nRecords) AS nRecords,
                     MIN(o.firstYear) AS firstYear, MAX(o.lastYear) AS lastYear
              FROM occurrences o LEFT JOIN kgLegend l USING (kgCode)
              WHERE o.eppoCode = ?
              GROUP BY o.cell",
             params = list(input$eppo))
  })

  # All classes with records; occupied flag from the view; Norway share
  classes <- reactive({
    db_query("SELECT p.kgCode, l.kgLabel, p.nCells,
                     o.kgCode IS NOT NULL AS occupied,
                     ROUND(COALESCE(n.proportion, 0) * 100, 1) AS pctNorway,
                     ROUND(COALESCE(f.proportion, 0) * 100, 1) AS pctNorwayFuture
              FROM pestKgClasses p
              JOIN kgLegend l USING (kgCode)
              LEFT JOIN occupiedKgClasses o
                ON o.eppoCode = p.eppoCode AND o.kgCode = p.kgCode
              LEFT JOIN kgNorway n ON n.kgCode = p.kgCode
              LEFT JOIN kgNorwayFuture f ON f.kgCode = p.kgCode
              WHERE p.eppoCode = ?
              ORDER BY p.nCells DESC",
             params = list(input$eppo)) %>%
      mutate(occupied = occupied == 1)
  })

  output$summary <- renderUI({
    sp  <- filter(species, eppoCode == input$eppo)
    occ <- filter(classes(), occupied)
    if (sp$nCells == 0) {
      return(p(strong("No GBIF occurrences"),
               sprintf(" (GBIF match: %s).", coalesce(sp$gbifMatch, "none"))))
    }
    if (nrow(occ) == 0) {
      return(p(strong("No occupied climate class."),
               " Too few records for usable GBIF climate evidence."))
    }
    p(strong(sprintf("%.1f%%", sum(occ$pctNorway))),
      " of non-alpine Norway lies in the occupied classes",
      if (any(occ$pctNorway > 0)) {
        sprintf(" (%s)", paste(occ$kgCode[occ$pctNorway > 0], collapse = ", "))
      },
      "; ", strong(sprintf("%.1f%%", sum(occ$pctNorwayFuture))),
      " in 2041-2070 (SSP5-8.5).")
  })

  output$classes <- renderDT({
    future <- input$period != names(KG_MAPS)[1]
    classes() %>%
      mutate(pctNorway = if (future) pctNorwayFuture else pctNorway) %>%
      # climate name as hover text on the code (also in the map legend)
      transmute(Class = sprintf('<span title="%s">%s</span>',
                                htmltools::htmlEscape(kgLabel, attribute = TRUE), kgCode),
                Cells = format(nCells, big.mark = ","),
                Occupied = if_else(occupied, "✓", ""),
                Norway = pctNorway) %>%
      rename_with(~ if (future) "Norway % 2041-2070" else "Norway % 1991-2020",
                  Norway) %>%
      datatable(rownames = FALSE, escape = FALSE, class = "compact stripe",
                options = list(dom = "t", paging = FALSE, ordering = FALSE,
                               columnDefs = list(list(className = "dt-center", targets = 0:3))))
  })

  output$map <- renderLeaflet({
    # Layer order: land, KG shading, borders, points
    # canvas instead of SVG: tens of thousands of points stay responsive
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addMapPane("land", zIndex = 300) %>%
      addMapPane("kg", zIndex = 350) %>%
      addMapPane("borders", zIndex = 420) %>%
      addMapPane("points", zIndex = 450) %>%
      addPolygons(data = world, fillColor = "#ebebeb", fillOpacity = 1, stroke = FALSE,
                  options = pathOptions(pane = "land")) %>%
      addPolygons(data = world, fill = FALSE, color = "#666666", weight = 0.5,
                  options = pathOptions(pane = "borders", interactive = FALSE)) %>%
      setView(20, 30, zoom = 2)
  })

  # Shading: the occupied classes on the chosen period's map. Separate from
  # the points so switching period keeps the points and the zoom.
  observe({
    occ <- filter(classes(), occupied)
    occ_values <- kg_legend$kgValue[kg_legend$kgCode %in% occ$kgCode]

    m <- leafletProxy("map") %>%
      clearImages() %>%
      clearControls()

    if (length(occ_values) > 0) {
      leg  <- filter(kg_legend, kgValue %in% occ_values)
      opts <- gridOptions(pane = "kg")
      opts$opacity <- 0.7
      m %>%
        # same call addRasterImage() ends with, given our own PNG
        invokeMethod(NULL, "addRasterImage", kg_image(occ_values, input$period),
                     kg_latlng, NULL, NULL, opts) %>%
        addLegend("bottomright", colors = leg$hex,
                  labels = paste(leg$kgCode, leg$kgLabel),
                  title = paste("Occupied classes,", input$period), opacity = 0.7)
    }
  })

  observe({
    d   <- pts()
    occ <- filter(classes(), occupied)
    m   <- leafletProxy("map") %>% clearMarkers()

    if (nrow(d) > 0) {
      d <- mutate(d,
                  occupied = kgCode %in% occ$kgCode,
                  popup = sprintf("<b>%s</b> %s<br>%s, %.0f record(s)<br>%s-%s",
                                  kgCode, kgLabel, basisOfRecord, nRecords,
                                  coalesce(as.character(firstYear), "?"),
                                  coalesce(as.character(lastYear), "?")))
      m %>%
        # filled: point in an occupied class; hollow: class below the rule
        addCircleMarkers(data = d, lng = ~lon, lat = ~lat, popup = ~popup,
                         radius = 4, fillColor = "red",
                         color = ~ if_else(occupied, "black", "red"),
                         weight = ~ if_else(occupied, 0.5, 1.5),
                         fillOpacity = ~ if_else(occupied, 1, 0),
                         opacity = 1, options = pathOptions(pane = "points")) %>%
        fitBounds(min(d$lon), min(d$lat), max(d$lon), max(d$lat))
    }
  })
}

shinyApp(ui, server)
