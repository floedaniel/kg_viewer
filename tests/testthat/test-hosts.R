# Run from the repo root: testthat::test_dir("tests/testthat")
source(file.path(testthat::test_path(), "..", "..", "R", "hosts.R"))

test_that("choose_cho_layers keeps clip over plg per part", {
  files <- paste0("Pinus_nigra_", c("dalmatica_pnt", "laricio_plg", "laricio_pnt",
                                    "nigra_plg", "nigra_plg_clip", "nigra_pnt",
                                    "pallasiana_plg", "pallasiana_plg_clip",
                                    "pallasiana_pnt", "salzmannii_plg",
                                    "salzmannii_pnt"), ".shp")
  expect_equal(choose_cho_layers(files),
               sort(paste0("Pinus_nigra_", c("dalmatica_pnt", "laricio_plg", "laricio_pnt",
                                             "nigra_plg_clip", "nigra_pnt",
                                             "pallasiana_plg_clip", "pallasiana_pnt",
                                             "salzmannii_plg", "salzmannii_pnt"), ".shp")))
})

test_that("choose_cho_layers drops planted/naturalised layers", {
  files <- paste0("Picea_abies_", c("plg", "plg_clip", "pnt", "syn_plg",
                                    "syn_plg_clip", "syn_pnt"), ".shp")
  expect_equal(choose_cho_layers(files),
               c("Picea_abies_plg_clip.shp", "Picea_abies_pnt.shp"))
})

test_that("choose_cho_layers handles points only", {
  expect_equal(choose_cho_layers("Phoenix_theophrasti_pnt.shp"),
               "Phoenix_theophrasti_pnt.shp")
})

test_that("host_key matches species exactly, not genus or subspecies", {
  expect_equal(host_key("Abies borisii-regis"), host_key("Abies borisiiregis"))
  expect_false(host_key("Pinus") == host_key("Pinus sylvestris"))
  expect_false(host_key("Populus nigra var. italica") == host_key("Populus nigra"))
})
