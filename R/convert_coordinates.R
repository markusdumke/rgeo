#' Convert coordinates from Gauss-Krueger to WGS84.
#'
#' @param .gk.rw A numeric vector of Gauss Krueger Rechtswert coordinates.
#' @param .gk.hw A numeric vector of Gauss Krueger Hochwert coordinates.
#'
#' @export
#' @import data.table sf magrittr
#'
#' @examples
#' convert_coordinates(.gk.rw = 3557875, .gk.hw = 5541342)
#' # lat <- 50.0056; lng <- 9.806289
convert_coordinates <- function(.gk.rw = NULL, .gk.hw = NULL) {

  checkmate::assert_numeric(.gk.rw)
  checkmate::assert_numeric(.gk.hw)

  data <- data.table(cbind("gk_rw" = .gk.rw, "gk_hw" = .gk.hw))

  # Defining Gauss Krüger
  data$id <- seq_len(NROW(data))
  data_1 <- data[gk_rw > 4000000]
  data_2 <- data[gk_rw <= 4000000]

  gk_wgs84_1 <- NULL
  gk_wgs84_2 <- NULL

  # Transform coordinates from Gauss-Krueger to WGS84
  if (NROW(data_1) > 0) {
    coords_1 <-
      st_as_sf(data_1, coords = c("gk_rw", "gk_hw"), crs = 31468) %>%
      st_transform(4326)

    gk_wgs84_1 <-
      cbind(st_set_geometry(coords_1, NULL), st_coordinates(coords_1)) %>%
      setnames(c("X", "Y"), c("longitude", "latitude")) %>%
      .[, id := data_1$id] %>%
      .[]
  }

  if (NROW(data_2) > 0) {
    coords_2 <-
      st_as_sf(data_2, coords = c("gk_rw", "gk_hw"), crs = 31467) %>%
      st_transform(4326)

    gk_wgs84_2 <-
      cbind(st_set_geometry(coords_2, NULL), st_coordinates(coords_2)) %>%
      setnames(c("X", "Y"), c("longitude", "latitude")) %>%
      .[, id := data_2$id] %>%
      .[]
  }

  rbind(as.data.table(gk_wgs84_1), as.data.table(gk_wgs84_2)) %>%
    .[order(id)] %>%
    .[, id := NULL] %>%
    .[]
}
