  # kg_viewer  (DRAFT)
# Choose a species -> its GBIF occurrences on the Köppen-Geiger map, the
# climate classes it occupies and the share of non-alpine Norway in them,
# with the natural range of a chosen host tree as a dashed outline. With no
# species chosen (at start, or species box cleared): the climate classes of
# non-alpine Norway, shown worldwide.
# Background for manual EST1 assessments.
#
# Reads kg_climate.db in this folder, built by kg_db.R (also in this folder);
# host tables added by host_db.R.
# Run from the project root:  shiny::runApp("scripts/kg_viewer")

library(shiny)
library(bslib)
library(leaflet)
library(terra)
library(png)    # shading image, see kg_image()
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)             # legend grid in the sidebar
library(rnaturalearth)
library(rnaturalearthdata)   # country outlines; loaded explicitly so deployment includes it
library(sf)                  # host ranges (WKT)

# runApp sets this folder as working directory
DB_PATH <- "kg_climate.db"

# Beck et al. 2023, 0.1 deg versions (CC BY 4.0), copied into this folder so
# the app also runs on shinyapps.io. Names are the period choices in the UI.
KG_MAPS <- c("1991-2020"                       = "koppen_geiger_0p1.tif",
             "2041-2070 (SSP5-8.5, worst case)" = "koppen_geiger_2041_2070_ssp585_0p1.tif")

# Data support levels and tints (shared with README and vignette)
source("R/support.R", local = TRUE)
# Shared VKM / FinnPRIO look (colours, header, footer)
source("R/theme.R", local = TRUE)
# Rule and short description per KG class (class box on the map)
source("R/kg_classes.R", local = TRUE)
APP_NAME <- "Köppen-Geiger climate of pest occurrences"

db_query <- function(sql, params = NULL) {
  con <- dbConnect(SQLite(), DB_PATH, flags = SQLITE_RO)
  on.exit(dbDisconnect(con))
  dbGetQuery(con, sql, params = params)
}

kg_legend <- db_query("SELECT * FROM kgLegend ORDER BY kgValue")
species   <- db_query("SELECT eppoCode, scientificName, gbifMatch, nCells
                       FROM species ORDER BY scientificName")
build     <- with(db_query("SELECT * FROM buildInfo"), setNames(value, key))

# Host tree ranges added by host_db.R (absent in older DBs: no host layer).
# One range is drawn at a time (input$host): a single host, or all hosts of
# the pest combined, as an outline over the unchanged climate map. Outlines
# are read per range (hostRange, host_layers()).
has_hosts <- all(c("pestHosts", "hostRange", "hostInfo") %in%
                   db_query("SELECT name FROM sqlite_master")$name)
if (has_hosts) {
  pest_hosts <- db_query("SELECT * FROM pestHosts ORDER BY hostName")
  host_meta  <- with(db_query("SELECT * FROM hostInfo"), setNames(value, key))
}

# Host range edge: a thin dashed line only, so the KG colours stay readable
HOST_EDGE <- list(colour = "#1a1a1a", weight = 1, opacity = 0.7, dash = "4 4")
host_legend <- function(name) HTML(sprintf(
  '<div style="background:rgba(255,255,255,.92);padding:5px 9px;font-size:12px;white-space:nowrap;width:max-content;border-radius:4px;box-shadow:0 1px 4px rgba(0,0,0,.25)">
     <span style="display:inline-block;width:20px;margin-right:6px;vertical-align:middle;border-top:1.5px dashed %s"></span>Natural range of %s
   </div>', HOST_EDGE$colour, name))

# Class box, lower right of the map: the occupied classes with medium or high
# data support, each a <details> row (swatch, code, name, share of the
# species' occupied cells) that opens to the description and rule. Low-support
# classes are named in a last line. occ: occupied() rows, sorted by nCells.
# Events are stopped at the box so scrolling and clicking it leave the map be.
# Also the Norway overview's box: share = formatted % per row, own title and
# note (all rows support 1, so no low-support line).
kg_class_box <- function(occ, share = NULL,
                         title = "Climate classes (medium/high support)",
                         note  = "% = share of the species' occupied cells.") {
  esc <- function(x) htmltools::htmlEscape(x)
  if (is.null(share)) {
    share <- 100 * occ$nCells / sum(occ$nCells)
    share <- if_else(share < 1, "<1%", sprintf("%.0f%%", share))
  }
  occ <- mutate(occ, share = share) %>%
    left_join(KG_CLASS_INFO, by = "kgCode")
  shown <- filter(occ, support <= 2)
  low   <- filter(occ, support == 3)

  rows <- if (nrow(shown) == 0) {
    "<div style=\"color:#555;margin:2px 0\">No class with medium or high support.</div>"
  } else sprintf(
    '<details style="margin:1px 0">
       <summary style="cursor:pointer;display:flex;align-items:center;gap:6px">
         <span class="kg-arrow" style="flex:none;width:8px;color:#777;transition:transform .15s">&#9656;</span>
         <span style="flex:none;width:12px;height:12px;background:%s;border:1px solid rgba(0,0,0,.3)"></span>
         <b style="flex:none;width:30px">%s</b>
         <span style="flex:1;white-space:nowrap;overflow:hidden;text-overflow:ellipsis" title="%s">%s</span>
         <span style="flex:none">%s</span>
       </summary>
       <div style="margin:2px 0 6px 34px;line-height:1.35">%s
         <div style="margin-top:3px;color:#555"><b>Rule:</b> %s</div></div>
     </details>',
    shown$shown, shown$kgCode, esc(shown$kgLabel), esc(shown$kgLabel), shown$share,
    esc(coalesce(shown$description, "")), esc(coalesce(shown$rule, "")))

  low_line <- if (nrow(low) > 0) sprintf(
    '<div style="margin-top:4px;color:#555">+ %d low-support class%s (%s): see sidebar.</div>',
    nrow(low), if (nrow(low) > 1) "es" else "", paste(low$kgCode, collapse = ", "))

  stop_ev <- "event.stopPropagation()"
  HTML(sprintf(
    '<div onwheel="%s" onmousedown="%s" onpointerdown="%s" ontouchstart="%s" ondblclick="%s" onclick="%s"
          class="kg-box" style="background:rgba(255,255,255,.95);padding:6px 9px;font-size:12px;width:300px;max-height:45vh;overflow-y:auto;border-radius:4px;box-shadow:0 1px 4px rgba(0,0,0,.25)">
       <style>.kg-box details[open] > summary .kg-arrow { transform: rotate(90deg); }</style>
       <div style="font-weight:600;margin-bottom:3px">%s</div>
       %s%s
       <div style="margin-top:5px;font-size:10.5px;color:#777">%s Click a class for its description and rule. %s</div>
     </div>',
    stop_ev, stop_ev, stop_ev, stop_ev, stop_ev, stop_ev, title,
    paste(rows, collapse = ""), coalesce(low_line, ""), note, KG_SEASON_NOTE))
}

# Share of non-alpine Norway, formatted for the overview (box and sidebar)
pct_norway <- function(p) case_when(p >= 10   ~ sprintf("%.0f%%", p),
                                    p >= 0.1  ~ sprintf("%.1f%%", p),
                                    p >= 0.01 ~ sprintf("%.2f%%", p),
                                    TRUE      ~ "<0.01%")

# Climate classes of non-alpine Norway per period (names as in KG_MAPS),
# largest first, in their normal colours (support 1: nothing tinted)
norway_classes <- list(db_query("SELECT * FROM kgNorway"),
                       db_query("SELECT * FROM kgNorwayFuture")) %>%
  lapply(function(d) {
    filter(d, proportion > 0) %>%
      inner_join(select(kg_legend, kgCode, kgLabel, kgValue, hex), by = "kgCode") %>%
      transmute(kgCode, kgLabel, kgValue, hex, shown = hex, support = 1L,
                pct = 100 * proportion) %>%
      arrange(desc(pct))
  }) %>%
  setNames(names(KG_MAPS))

# Clickable DOI for the data sources (NULL if the key is missing, older DBs)
doi_link <- function(doi, prefix = NULL) {
  if (is.na(doi)) return(NULL)
  url <- paste0("https://doi.org/", doi)
  tagList(prefix, tags$a(href = url, target = "_blank", url))
}

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

# Class value (kgValue) under a map point on the chosen period's map; NA
# outside the grid or without a class. Same Web Mercator grid as the shading.
kg_value_at <- function(lng, lat, period) {
  lng <- (lng + 180) %% 360 - 180                 # leaflet lets lng run past ±180
  if (lat < -60 || lat > 85) return(NA_integer_)
  r <- 6378137
  xy <- cbind(r * lng * pi / 180, r * log(tan(pi / 4 + lat * pi / 360)))
  cell <- cellFromXY(kg_grid, xy)
  if (is.na(cell)) return(NA_integer_)
  v <- as.integer(kg_idx[[period]][cell]) - 1L
  if (v < 1L) NA_integer_ else v
}

# Hover label on the map: the browser sends the pointer position
# (input$map_hover, throttled), the server answers with "Cfa = Humid
# subtropical climate" or "" (custom message kgHover). Replies to older
# positions are ignored. Hidden over the boxes and off the map.
HOVER_JS <- "
function(el, x) {
  var map = this, seq = 0, last = 0, timer = null, pos = null;
  var tip = document.createElement('div');
  tip.style.cssText = 'position:absolute;z-index:1000;pointer-events:none;display:none;' +
    'background:rgba(255,255,255,.95);padding:2px 7px;font-size:12px;border-radius:3px;' +
    'box-shadow:0 1px 3px rgba(0,0,0,.3);white-space:nowrap';
  el.appendChild(tip);
  function hide() { tip.style.display = 'none'; seq++; clearTimeout(timer); }
  function send() {
    last = Date.now();
    Shiny.setInputValue('map_hover', {lat: pos.lat, lng: pos.lng, id: ++seq});
  }
  map.on('mousemove', function(e) {
    if (e.originalEvent.target.closest('.leaflet-control')) { hide(); return; }
    tip.style.left = (e.containerPoint.x + 12) + 'px';
    tip.style.top  = (e.containerPoint.y - 30) + 'px';
    pos = e.latlng;
    clearTimeout(timer);
    if (Date.now() - last > 120) send(); else timer = setTimeout(send, 120);
  });
  map.on('mouseout', hide);
  Shiny.addCustomMessageHandler('kgHover', function(m) {
    if (m.id !== seq) return;
    tip.textContent = m.text;
    tip.style.display = m.text ? 'block' : 'none';
  });
}"

# hex: colour per value in occ_values
kg_image <- function(occ_values, hex, period) {
  rgba <- matrix(as.raw(0), 4, max(kg_legend$kgValue) + 1)   # transparent
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

ui <- page(
  title = APP_NAME,
  theme = hm_theme(),
  vkm_header(APP_NAME, "FinnPRIO: Köppen climate classification"),
  # Window height minus header and footer: the sidebar scrolls on its own and
  # the map fills the rest, as with page_sidebar() before.
  layout_sidebar(height = "calc(100vh - 125px)", fillable = TRUE,
  sidebar = sidebar(
    width = 460,
    # selectize filters the list as you type (scientific name or EPPO code).
    # Starts empty (Norway overview); the clear button (x) returns to it.
    selectizeInput("eppo", "Species (click and type to search name or EPPO code)",
                   choices = species_choices,
                   options = list(placeholder = "Type a name or EPPO code...",
                                  plugins = list("clear_button"),
                                  onInitialize = I("function() { this.setValue(''); }"))),
    # Way back to the overview, shown while a species is chosen
    conditionalPanel("input.eppo && input.eppo !== ''",
      tags$button(type = "button", class = "btn btn-outline-primary btn-sm mb-3",
                  onclick = "document.getElementById('eppo').selectize.clear();",
                  "← Back to world view (Norway's climate classes)")),
    radioButtons("period", "Climate map", choices = names(KG_MAPS)),
    if (has_hosts) uiOutput("host_choice"),
    uiOutput("summary"),
    plotOutput("legend", height = "auto"),
    tags$small(
      sprintf("Occupied class: %s, from present-day records. ", rule_text),
      "Climate: Beck et al. 2023, 1991-2020 and 2041-2070 (SSP5-8.5)",
      doi_link(build["kgDoi"], ", "), ". ",
      "Class rules: Beck et al. 2018",
      doi_link("10.1038/sdata.2018.214", ", "),
      "; class descriptions adapted from Wikipedia, ",
      tags$a(href = "https://en.wikipedia.org/wiki/K%C3%B6ppen_climate_classification",
             target = "_blank", em("Köppen climate classification")),
      " (CC BY-SA 4.0). ",
      "Occurrences: ",
      if (is.na(build["gbifCitation"])) "GBIF" else
        tagList(sub(" https://doi.org/.*", " ", build[["gbifCitation"]]),
                doi_link(build["gbifDownloadDoi"])),
      sprintf(", cleaned, one point per 1 km cell. Built %s.",
              substr(build[["buildDate"]], 1, 10)),
      if (has_hosts) tagList(
        " Host ranges (natural range, Europe): Caudullo et al. 2017",
        doi_link(host_meta["hostDoi"], ", "),
        sprintf("; hosts from the EPPO Global Database (%s), exact species matches only.",
                host_meta[["hostListDate"]]))
    )
  ),
  tags$style(".leaflet-container { background: #dce9f2; }"),   # ocean (map data colour)
  card(full_screen = TRUE, leafletOutput("map", height = "100%"))
  ),
  vkm_footer(APP_NAME)
)

server <- function(input, output, session) {

  # Chosen species; "" = none (Norway overview). The species queries below
  # then return no rows, so points, legend and class box stay empty.
  eppo     <- reactive(if (is.null(input$eppo)) "" else input$eppo)
  overview <- reactive(!nzchar(eppo()))
  norway   <- reactive(norway_classes[[input$period]])

  pts <- reactive({
    # one marker per cell (occurrences has one row per cell and basisOfRecord)
    db_query("SELECT o.cell, o.lon, o.lat, o.kgCode, l.kgLabel,
                     GROUP_CONCAT(o.basisOfRecord, ', ') AS basisOfRecord,
                     SUM(o.nRecords) AS nRecords,
                     MIN(o.firstYear) AS firstYear, MAX(o.lastYear) AS lastYear
              FROM occurrences o LEFT JOIN kgLegend l USING (kgCode)
              WHERE o.eppoCode = ?
              GROUP BY o.cell",
             params = list(eppo()))
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
             params = list(eppo())) %>%
      mutate(occupied = occupied == 1,
             support  = if_else(occupied, support_level(nCells), NA_integer_))
  })

  # Occupied classes with their shading colour (tint by data support)
  occupied <- reactive({
    filter(classes(), occupied) %>%
      left_join(select(kg_legend, kgCode, kgValue, hex), by = "kgCode") %>%
      mutate(shown = tint(hex, SUPPORT_TINT[support]))
  })

  # Host trees of the chosen pest with a mapped range (none: NULL)
  hosts <- reactive({
    if (!has_hosts) return(NULL)
    h <- filter(pest_hosts, eppoCode == eppo())
    if (nrow(h) == 0) NULL else h
  })

  output$summary <- renderUI({
    if (overview()) norway_summary() else tagList(climate_summary(), host_summary())
  })

  # Overview: what the map shows, then one row per class (swatch, code,
  # name, share of non-alpine Norway in the chosen period)
  norway_summary <- function() {
    nor <- norway()
    tagList(
      p(strong("No species chosen."),
        sprintf(" The map shows where the climate classes of non-alpine Norway (%s) occur worldwide. ",
                input$period),
        "Pick a species above to see its occurrences; the 'Back to world view' button returns here."),
      tags$table(style = "width:100%;font-size:.95em;margin-bottom:1rem",
        tags$tr(tags$th(colspan = 3, "Class"),
                tags$th(style = "text-align:right", "Norway")),
        lapply(seq_len(nrow(nor)), function(i) tags$tr(
          tags$td(style = "width:18px",
                  tags$span(style = sprintf(
                    "display:inline-block;width:12px;height:12px;background:%s;border:1px solid rgba(0,0,0,.3)",
                    nor$hex[i]))),
          tags$td(style = "width:40px", strong(nor$kgCode[i])),
          tags$td(nor$kgLabel[i]),
          tags$td(style = "text-align:right", pct_norway(nor$pct[i]))))))
  }

  host_summary <- function() {
    if (!has_hosts) return(NULL)
    h <- hosts()
    if (is.null(h)) {
      return(p(em("No host range mapped"),
               " (no EPPO host of this pest among the Caudullo et al. 2017 species)."))
    }
    p("Hosts with a mapped range: ",
      HTML(paste(sprintf("<i>%s</i>%s", h$hostName,
                         if_else(h$hostType == "Host", "", paste0(" (", tolower(h$hostType), ")"))),
                 collapse = ", ")),
      ". Dashed line on the map: natural range (present day) of the host ",
      "chosen above; other hosts of the pest are not mapped.")
  }

  # Host menu: major hosts first; "all combined" when there are several
  output$host_choice <- renderUI({
    h <- hosts()
    if (is.null(h)) return(NULL)
    h <- arrange(h, match(hostType, c("Major host", "Host", "Wild/Weed")), hostName)
    choices <- setNames(h$hostName, paste0(h$hostName,
                                           if_else(h$hostType == "Host", "",
                                                   paste0(" (", tolower(h$hostType), ")"))))
    if (nrow(h) > 1) {
      choices <- c(choices, setNames(paste0("all:", eppo()),
                                     sprintf("All %d listed hosts combined", nrow(h))))
    }
    selectInput("host", "Host tree", choices = choices)
  })

  # Chosen range; NULL until the menu matches the current pest
  range_id <- reactive({
    h <- hosts()
    id <- input$host
    valid <- !is.null(h) && !is.null(id) &&
      (id %in% h$hostName || id == paste0("all:", eppo()))
    if (valid) id else NULL
  })
  range_name <- reactive({
    id <- range_id()
    if (startsWith(id, "all:")) "all listed hosts" else sprintf("<i>%s</i>", id)
  })

  climate_summary <- function() {
    sp  <- filter(species, eppoCode == eppo())
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
  }

  # Legend grid: occupied classes x data support, each class outlined at its
  # own level; Norway share for the chosen period
  output$legend <- renderPlot({
    occ <- occupied()
    req(nrow(occ) > 0)
    future <- input$period != names(KG_MAPS)[1]
    occ <- mutate(occ,
                  row    = factor(kgCode, rev(kgCode)),
                  norway = if (future) pctNorwayFuture else pctNorway,
                  nLabel = if_else(norway > 0, sprintf("%.1f%%", norway), "–"),
                  nFace  = if_else(norway > 0, "bold", "plain"))
    grid <- cross_join(select(occ, row, hex, own = support), tibble(col = 1:3)) %>%
      mutate(fill = tint(hex, SUPPORT_TINT[col]))

    ggplot(grid, aes(col, row)) +
      geom_tile(aes(fill = fill), colour = "white", linewidth = 1) +
      geom_tile(data = filter(grid, col == own), fill = NA, colour = "black",
                linewidth = 0.7, width = 0.92, height = 0.88) +
      geom_text(data = occ, aes(x = 3.65, y = row, label = nLabel, fontface = nFace),
                hjust = 0, size = 4.4, inherit.aes = FALSE) +
      annotate("text", x = 3.65, y = nrow(occ) + 0.75, label = "Norway",
               hjust = 0, size = 4.4, fontface = "bold") +
      scale_fill_identity() +
      scale_x_continuous(breaks = 1:3, labels = SUPPORT_LABELS, position = "top",
                         expand = expansion(add = c(0.5, 1.25))) +
      scale_y_discrete(labels = setNames(sprintf("%s (n = %s)", occ$kgCode,
                                                 format(occ$nCells, big.mark = ",", trim = TRUE)), occ$row)) +
      coord_cartesian(clip = "off") +
      labs(x = "Data support (n = 1 km cells)", y = NULL) +
      theme_minimal(base_size = 15) +
      theme(panel.grid = element_blank(),
            text = element_text(colour = HM_COLOURS$text), axis.text = element_text(colour = HM_COLOURS$text),
            axis.title.x.top = element_text(size = 14, margin = margin(b = 4)),
            axis.text.x.top  = element_text(size = 11, lineheight = 0.9))
  }, height = function() 110 + 34 * max(1, nrow(occupied())), res = 96)

  output$map <- renderLeaflet({
    # Layer order: land, KG shading, borders, points
    # canvas instead of SVG: tens of thousands of points stay responsive
    # maxBounds (viscosity 1: hard edge) and minZoom keep it to one world,
    # the same extent as the climate map (Antarctica cropped)
    leaflet(options = leafletOptions(preferCanvas = TRUE, minZoom = 2,
                                     maxBounds = list(list(-60, -180), list(85, 180)),
                                     maxBoundsViscosity = 1)) %>%
      addMapPane("land", zIndex = 300) %>%
      addMapPane("kg", zIndex = 350) %>%
      addMapPane("host", zIndex = 400) %>%
      addMapPane("borders", zIndex = 420) %>%
      addMapPane("points", zIndex = 450) %>%
      addPolygons(data = world, fillColor = "#ffffff", fillOpacity = 1, stroke = FALSE,
                  options = pathOptions(pane = "land")) %>%
      addPolygons(data = world, fill = FALSE, color = HM_COLOURS$muted, weight = 0.5,
                  options = pathOptions(pane = "borders", interactive = FALSE)) %>%
      setView(20, 30, zoom = 2) %>%
      htmlwidgets::onRender(HOVER_JS)
  })

  # Classes shaded on the map: Norway's classes in the overview, else the
  # species' occupied classes (kgValue, shown colour)
  shaded <- reactive(if (overview()) norway() else occupied())

  # Hover label: name of the shaded class under the pointer (only classes
  # shaded now, on the chosen period's map)
  observeEvent(input$map_hover, {
    h   <- input$map_hover
    occ <- shaded()
    v   <- kg_value_at(h$lng, h$lat, input$period)
    txt <- ""
    if (!is.na(v) && v %in% occ$kgValue) {
      code <- kg_legend$kgCode[kg_legend$kgValue == v]
      txt  <- sprintf("%s = %s", code,
                      KG_CLASS_INFO$common[match(code, KG_CLASS_INFO$kgCode)])
    }
    session$sendCustomMessage("kgHover", list(id = h$id, text = txt))
  })

  # Shading: the occupied classes on the chosen period's map, tinted by data
  # support (legend in the sidebar); in the overview Norway's classes,
  # untinted. Separate from the points so switching period keeps the points
  # and the zoom. Independent of the host choice.
  observe({
    occ <- shaded()
    m   <- leafletProxy("map") %>% clearImages()

    if (nrow(occ) > 0) {
      opts <- gridOptions(pane = "kg")
      opts$opacity <- 0.85
      opts$noWrap  <- TRUE   # one copy of the shading, not repeated around the globe
      m %>%
        # same call addRasterImage() ends with, given our own PNG
        invokeMethod(NULL, "addRasterImage", kg_image(occ$kgValue, occ$shown, input$period),
                     kg_latlng, NULL, NULL, opts)
    }
  })

  # Class box (lower right): rebuilt per species; none without occupied
  # classes. Overview: Norway's classes with their share of Norway.
  observe({
    m <- leafletProxy("map") %>% removeControl("kgclasses")
    box <- if (overview()) {
      nor <- norway()
      kg_class_box(nor, share = pct_norway(nor$pct),
                   title = sprintf("Climate classes of non-alpine Norway, %s", input$period),
                   note  = "% = share of non-alpine Norway.")
    } else if (nrow(occupied()) > 0) kg_class_box(occupied())
    if (!is.null(box)) {
      addControl(m, box, position = "bottomright", layerId = "kgclasses", className = "")
    }
  })

  # Back to the overview (species box cleared): whole world again
  observeEvent(overview(), {
    if (overview()) leafletProxy("map") %>% setView(20, 30, zoom = 2)
  }, ignoreInit = TRUE)

  # Host geometry of the chosen species: range (hover label), edge (range
  # boundary on land)
  host_layers <- reactive({
    req(range_id())
    g <- db_query("SELECT layer, wkt FROM hostRange WHERE rangeId = ?",
                  params = list(range_id()))
    setNames(lapply(g$wkt, st_as_sfc, crs = 4326), g$layer)
  })

  # Host range edge: own group and pane, so redrawing the points keeps it
  observe({
    m <- leafletProxy("map") %>% clearGroup("host") %>% removeControl("hostlegend")
    req(range_id())
    g <- host_layers()
    label <- if (startsWith(range_id(), "all:")) {
      paste("Hosts:", paste(hosts()$hostName, collapse = ", "))
    } else range_id()
    # invisible fill: hover label anywhere inside the range
    m <- addPolygons(m, data = g$range, group = "host", stroke = FALSE,
                     fillOpacity = 0, label = label,
                     options = pathOptions(pane = "host"))
    if (!is.null(g$edge)) {
      m <- addPolylines(m, data = g$edge, group = "host",
                        color = HOST_EDGE$colour, weight = HOST_EDGE$weight,
                        opacity = HOST_EDGE$opacity, dashArray = HOST_EDGE$dash,
                        options = pathOptions(pane = "host", interactive = FALSE))
    }
    addControl(m, host_legend(range_name()), position = "bottomleft", layerId = "hostlegend",
               className = "")   # own box (leaflet's "info legend" box is fixed width)
  })

  observe({
    d   <- pts()
    occ <- filter(classes(), occupied)
    m   <- leafletProxy("map") %>% clearGroup("occ")

    if (nrow(d) > 0) {
      d <- mutate(d,
                  occupied = kgCode %in% occ$kgCode,
                  popup = sprintf("<b>%s</b> %s<br>%s, %.0f record(s)<br>%s-%s",
                                  kgCode, kgLabel, basisOfRecord, nRecords,
                                  coalesce(as.character(firstYear), "?"),
                                  coalesce(as.character(lastYear), "?")))
      m %>%
        # filled: point in an occupied class; hollow: class below the rule
        addCircleMarkers(data = d, lng = ~lon, lat = ~lat, popup = ~popup, group = "occ",
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
