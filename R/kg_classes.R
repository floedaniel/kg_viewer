# kg_classes.R: rule and short description per Köppen-Geiger class, for the
# class box in the lower right of the map (app.R, kg_class_box()).
# Rules: Beck et al. 2018 (Sci. Data 5:180214, Table 1), the scheme behind
# the Beck et al. 2023 maps. Descriptions: adapted from Wikipedia,
# "Köppen climate classification" (CC BY-SA 4.0); thresholds left to the rules.
# Class names come from kgLegend (Beck's legend.txt).

KG_CLASS_INFO <- local({
  ge <- "≥"; le <- "≤"; mn <- "−"; deg <- "°C"

  temp_letter <- c(
    a = sprintf("warmest month %s 22 %s", ge, deg),
    b = sprintf("warmest month < 22 %s; at least 4 months %s 10 %s", deg, ge, deg),
    c = sprintf("1%s3 months %s 10 %s", "–", ge, deg),
    d = sprintf("1%s3 months %s 10 %s; coldest month < %s38 %s", "–", ge, deg, mn, deg))
  precip_letter <- c(
    s = "dry summer: driest summer month < 40 mm and < 1/3 of the wettest winter month",
    w = "dry winter: driest winter month < 1/10 of the wettest summer month",
    f = "no dry season (neither the dry-summer nor the dry-winter test is met)")
  b_threshold <- sprintf(paste0(
    "Threshold (mm) = 2 %s mean annual temperature (%s), + 28 if over 70%% of ",
    "precipitation falls in summer, + 14 if neither half-year has over 70%%."), "×", deg)

  rule_of <- function(code) {
    g <- substr(code, 1, 1); l2 <- substr(code, 2, 2); l3 <- substr(code, 3, 3)
    not_b <- "not arid (B)"
    r <- switch(g,
      A = c(sprintf("Coldest month %s 18 %s", ge, deg),
            switch(l2,
              f = sprintf("driest month %s 60 mm", ge),
              m = sprintf("driest month < 60 mm but %s 100 %s annual precipitation/25", ge, mn),
              w = sprintf("driest month < 60 mm and < 100 %s annual precipitation/25", mn)),
            not_b),
      B = c(switch(l2,
              W = sprintf("Annual precipitation < 5 %s threshold", "×"),
              S = sprintf("Annual precipitation 5%s10 %s threshold", "–", "×")),
            sprintf("mean annual temperature %s 18 %s", if (l3 == "h") ge else "<", deg)),
      C = c(sprintf("Warmest month > 10 %s; coldest month between 0 and 18 %s", deg, deg),
            precip_letter[[l2]], temp_letter[[l3]], not_b),
      D = c(sprintf("Warmest month > 10 %s; coldest month %s 0 %s", deg, le, deg),
            precip_letter[[l2]], temp_letter[[l3]], not_b),
      E = if (l2 == "T") sprintf("Warmest month between 0 and 10 %s", deg)
          else sprintf("Warmest month %s 0 %s", le, deg))
    out <- paste0(paste(r, collapse = "; "), ".")
    if (g == "B") paste(out, b_threshold) else out
  }

  desc <- c(
    Af  = "Hot and wet all year, with no dry season. Usually within about 10° of the equator.",
    Am  = "Hot all year; monsoon winds that change direction with the seasons give a short dry season and a long, very wet rainy season.",
    Aw  = "Hot all year with a pronounced dry season. Mostly at the outer margins of the tropics.",
    BWh = "Hot desert: very dry, with hot summers and mild winters. Typically in the subtropics, where stable descending air keeps skies clear.",
    BWk = "Cold desert: very dry, with warm to hot summers and cold, dry winters. Typically at higher latitudes or elevations than hot deserts.",
    BSh = "Hot semi-arid (steppe): dry, though less so than desert, with hot summers and warm to cool winters.",
    BSk = "Cold semi-arid (steppe): dry, with warm to hot summers and cold winters. Typically in elevated parts of the temperate zone and in continental interiors.",
    Csa = "Hot-summer Mediterranean: hot, dry summers and mild, rainy winters. Usually on the western sides of continents between about 30° and 45° latitude.",
    Csb = "Warm-summer Mediterranean: warm, dry summers and mild, rainy winters. Where summers are cooler than in Csa, often near cold seas or at higher elevation.",
    Csc = "Cold-summer Mediterranean: short, cool, dry summers and wet winters. Rare; in high-elevation areas next to Csb climates.",
    Cwa = "Monsoon-influenced humid subtropical: hot, wet summers and dry winters. At similar latitudes to Cfa, where monsoons dominate.",
    Cwb = "Dry-winter subtropical highland: mild all year, with dry winters and rainy summers. Mainly in highlands of the tropics and subtropics.",
    Cwc = "Dry-winter cold subtropical highland: cool summers and dry winters. Rare; mostly at high elevations in the Andes.",
    Cfa = "Humid subtropical: hot, humid summers and mild to cool winters, with precipitation all year. Usually on the eastern sides of continents.",
    Cfb = "Oceanic: mild summers and cool winters, with precipitation all year. Usually in the higher mid-latitudes on the western sides of continents; also in some subtropical highlands.",
    Cfc = "Subpolar oceanic: short, cool summers and mild winters for the latitude, with precipitation all year. Poleward of, or higher than, oceanic climates.",
    Dsa = "Hot-summer continental with dry summers: hot, dry summers and cold winters. Rare; mostly at higher elevations near Mediterranean climates.",
    Dsb = "Warm-summer continental with dry summers: warm, dry summers and cold winters. Rare; mostly at higher elevations near Mediterranean climates.",
    Dsc = "Subarctic with dry summers: short, cool, dry summers and cold winters. Rare; mostly at high elevations.",
    Dsd = "Extremely cold subarctic with dry summers: severe winters. Very rare; eastern Siberia.",
    Dwa = "Monsoon-influenced hot-summer continental: hot, wet summers and cold, dry winters. Mostly in eastern Asia.",
    Dwb = "Monsoon-influenced warm-summer continental: warm, wet summers and cold, dry winters. Mostly in eastern Asia.",
    Dwc = "Monsoon-influenced subarctic: short, cool, wet summers and very cold, dry winters. Mostly in eastern Asia.",
    Dwd = "Monsoon-influenced extremely cold subarctic: severe, dry winters. Only in eastern Siberia.",
    Dfa = "Hot-summer humid continental: hot summers and cold winters, with precipitation all year. Generally in the high 30s and low 40s latitudes.",
    Dfb = "Warm-summer humid continental: warm summers and cold winters, with precipitation all year. Generally poleward of the hot-summer continental climates.",
    Dfc = "Subarctic: short, cool summers and long, very cold winters, with precipitation all year. Poleward of the other D climates or at higher elevation.",
    Dfd = "Extremely cold subarctic: short summers and severe winters, with precipitation all year. Only in eastern Siberia.",
    ET  = "Tundra: no real warm season. On the northern edges of North America and Eurasia, and at high elevations.",
    EF  = "Ice cap: monthly mean temperatures never rise above freezing. Antarctica, inner Greenland and high mountain summits.")

  # Common name (Wikipedia's), shown when hovering the map: "Cfa = Humid
  # subtropical climate"
  common <- c(
    Af = "Tropical rainforest", Am = "Tropical monsoon", Aw = "Tropical savanna",
    BWh = "Hot desert", BWk = "Cold desert",
    BSh = "Hot semi-arid", BSk = "Cold semi-arid",
    Csa = "Hot-summer Mediterranean", Csb = "Warm-summer Mediterranean",
    Csc = "Cold-summer Mediterranean",
    Cwa = "Monsoon-influenced humid subtropical",
    Cwb = "Dry-winter subtropical highland",
    Cwc = "Dry-winter cold subtropical highland",
    Cfa = "Humid subtropical", Cfb = "Temperate oceanic", Cfc = "Subpolar oceanic",
    Dsa = "Mediterranean-influenced hot-summer humid continental",
    Dsb = "Mediterranean-influenced warm-summer humid continental",
    Dsc = "Mediterranean-influenced subarctic",
    Dsd = "Mediterranean-influenced extremely cold subarctic",
    Dwa = "Monsoon-influenced hot-summer humid continental",
    Dwb = "Monsoon-influenced warm-summer humid continental",
    Dwc = "Monsoon-influenced subarctic",
    Dwd = "Monsoon-influenced extremely cold subarctic",
    Dfa = "Hot-summer humid continental", Dfb = "Warm-summer humid continental",
    Dfc = "Subarctic", Dfd = "Extremely cold subarctic",
    ET = "Tundra", EF = "Ice cap")
  stopifnot(setequal(names(common), names(desc)))

  data.frame(kgCode = names(desc), description = unname(desc),
             rule = vapply(names(desc), rule_of, ""),
             common = paste(common[names(desc)], "climate"), row.names = NULL)
})

KG_SEASON_NOTE <- paste("Summer/winter: the warmer/colder six months",
                        "(Apr–Sep / Oct–Mar in the northern hemisphere).")
