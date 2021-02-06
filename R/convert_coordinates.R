#' Convert coordinates from Gauss-Krueger to WGS84.
#'
#' @param .gk.rw A numeric vector of Gauss Krueger Rechtswert coordinates.
#' @param .gk.hw A numeric vector of Gauss Krueger Hochwert coordinates.
#'
#' @export
#' @import data.table rgdal sp magrittr
#'
#' @examples
#' convert_coordinates(.gk.rw = 3557875, .gk.hw = 5541342)
#' # lat <- 50.005600; lng <- 9.806272
convert_coordinates <- function(.gk.rw = NULL, .gk.hw = NULL) {

  checkmate::assert_numeric(.gk.rw)
  checkmate::assert_numeric(.gk.hw)

  Data <- data.table(cbind("gk.rw" = .gk.rw, "gk.hw" = .gk.hw))

  # Defining Gauss Krüger
  Data$id <- seq_len(NROW(Data))
  Data.1 <- Data[gk.rw > 4000000]
  Data.2 <- Data[gk.rw <= 4000000]

  GK.WGS84.1 <- NULL
  GK.WGS84.2 <- NULL

  if (NROW(Data.1) > 0) {
    coordinates(Data.1) <- c("gk.rw", "gk.hw")

    # Gauss-Krueger reference system
    proj4string(Data.1) <- CRS("+proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +ellps=bessel +datum=potsdam +units=m +no_defs")
    # Tranforming to WGS84 longlat
    GK.WGS84.1 <- spTransform(Data.1, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  }

  if (NROW(Data.2) > 0) {
    coordinates(Data.2) <- c("gk.rw", "gk.hw")
    proj4string(Data.2) <- CRS("+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +ellps=bessel +datum=potsdam +units=m +no_defs")
    GK.WGS84.2 <- spTransform(Data.2, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  }

  rbind(as.data.table(GK.WGS84.1), as.data.table(GK.WGS84.2)) %>%
    .[order(id)] %>%
    .[, id := NULL] %>%
    setnames(c("gk.rw", "gk.hw"), c("longitude", "latitude")) %>%
    .[]
}
