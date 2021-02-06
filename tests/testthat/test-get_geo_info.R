test_that("get_geo_info", {

  output <-
    data.table(
      longitude = 12,latitude = 48.5,
      land = "Deutschland",
      bundesland = "Bayern",
      regierungsbezirk = "Niederbayern",
      landkreis = "Landshut",
      gemeinde = "Bruckberg",
      ort = "Bruckberg",
      naturraum_grosslandschaft = "Alpenvorland",
      naturraum_hauptname = "Unterbayerisches Hügelland und Isar-Inn-Schotterplatten",
      naturraum_name = "Unteres Isartal",
      tk25 = "7438 Landshut West",
      tk25_quadrant = "7438 30 Landshut West",
      tk25_viertelquadrant = "7438 33 Landshut West"
    )

  expect_equal(
    get_geo_info(
      .longitude = 12, .latitude = 48.5,
      .naturraum = naturraum_bayern,
      .boundaries = boundaries_bayern
    ),
    output
  )
})
