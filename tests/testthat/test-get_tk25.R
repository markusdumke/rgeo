test_that("coordinates_to_tk25", {
  expect_equal(
    coordinates_to_tk25(11, 48),
    data.table(
      tk25 = "7932 Utting am Ammersee",
      tk25_quadrant = "7932 30 Utting am Ammersee",
      tk25_viertelquadrant = "7932 33 Utting am Ammersee",
      tk25_64 = "7932 333 Utting am Ammersee")
  )
})

test_that("coordinates_to_tk25", {
  expect_equal(
    coordinates_to_tk25(c(11, 12), c(48, 49)),
    data.table(
      tk25 = c("7932 Utting am Ammersee", "6938 Regensburg"),
      tk25_quadrant = c("7932 30 Utting am Ammersee", "6938 30 Regensburg"),
      tk25_viertelquadrant = c("7932 33 Utting am Ammersee", "6938 33 Regensburg"),
      tk25_64 = c("7932 333 Utting am Ammersee", "6938 333 Regensburg"))
  )
})

test_that("tk25_to_coordinates", {
  expect_equal(
    tk25_to_coordinates("7932 33 Utting"),
    data.table(
      south = 47.99905,
      north = 48.09904,
      west = 10.998731,
      east = 11.16538,
      center.lat = 48.04904,
      center.lng = 11.08205
    ),
    tolerance = 0.00001
  )
})
