context("test-get_geographic_data")

library(data.table)
library(testthat)

geographic_data_de <- raster::getData("GADM", country = "DE", level = 2)

test_that("get_geographic_data", {
  Data <- data.table(latitude = 48, longitude = 11)

  Output <- data.table(Land = "Deutschland",
                       Bundesland = "Bayern",
                       Landkreis = "Landsberg am Lech",
                       Gemeinde = NA_character_,
                       Ort = NA_character_)

  # FIXME: Why does expect_equal complain about attributes?
  expect_equivalent(get_geographic_data(Data, .countries = list(geographic_data_de)),
                    Output)

  expect_equivalent(get_geographic_data(.latitude = Data$latitude,
                                        .longitude = Data$longitude,
                                        .countries = list(geographic_data_de)),
                    Output)
})


context("test-get_tk25_full")

test_that("get_tk25_full", {
  Data <- data.table(latitude = 47.5, longitude = 11)

  Output <- data.table(TK25 = "8432 Oberammergau",
                       TK25_Nummer = "8432",
                       TK25_Quadrant = "8432 Q3 Oberammergau",
                       TK25_Quadrant_Nummer = "3")

  expect_equal(get_tk25_full(Data),
               Output)

  expect_equal(get_tk25_full(.latitude = Data$latitude, .longitude = Data$longitude),
               Output)
})


context("test-get_naturraum")

test_that("get_naturraum", {
  Data <- data.table(latitude = 48, longitude = 12)

  Output <- data.table(Naturraum_Hauptname = "Voralpines Moor- und Hügelland",
                       Naturraum_Name = "Inn-Chiemsee-Hügelland",
                       Naturraum_Grossland = "Alpenvorland")

  expect_equal(get_naturraum(Data),
               Output)

  expect_equal(get_naturraum(.latitude = Data$latitude, .longitude = Data$longitude),
               Output)
})


context("test-get_geo_info")

test_that("get_geo_info", {
  Data <- data.table(latitude = 48.5, longitude = 12)

  Output <- data.table(latitude = 48.5,
                       longitude = 12,
                       Land = "Deutschland",
                       Bundesland = "Bayern",
                       Landkreis = "Landshut",
                       Gemeinde = NA_character_,
                       Ort = NA_character_,
                       TK25 = "7438 Landshut West",
                       TK25_Nummer = "7438",
                       TK25_Quadrant = "7438 Q3 Landshut West",
                       TK25_Quadrant_Nummer = "3",
                       Naturraum_Hauptname = "Unterbayerisches Hügelland und Isar-Inn-Schotterplatten",
                       Naturraum_Name = "Unteres Isartal",
                       Naturraum_Grossland = "Alpenvorland")

  expect_equivalent(get_geo_info(Data, .countries = list(geographic_data_de)),
                    Output)

  expect_equivalent(get_geo_info(.latitude = Data$latitude, .longitude = Data$longitude,
                                 .countries = list(geographic_data_de)),
                    Output)
})

# Clean up
file.remove("GADM_2.8_DEU_adm2.rds")
