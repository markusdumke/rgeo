#' Convert coordinates from Gauss-Krueger to WGS84
#'
#' @export
#' @import data.table rgdal sp
#'
#' @examples
#' gk.rw <- 3557875
#' gk.hw <- 5541342
#' convert_coordinates(.gk.rw = gk.rw, .gk.hw = gk.hw)
#' # lat <- 50.005600; lng <- 9.806272
#'
#' Data <- data.table(gk.rw = c(3557875, 4454942), gk.hw = c(5541342, 5532305))
#' convert_coordinates(Data)
convert_coordinates <- function(.Data = NULL, .gk.rw = NULL, .gk.hw = NULL) {
  if (is.null(.Data)) {
    Data <- data.table(cbind("gk.rw" = .gk.rw, "gk.hw" = .gk.hw))
  }

  if (!is.null(.Data)) {
    Data <- .Data %>% copy
    stopifnot(all(c("gk.rw", "gk.hw") %in% names(.Data)))
  }

  # Defining Gauss Krüger
  Data$id <- seq_len(NROW(Data))
  Data.1 <- Data[gk.rw > 4000000]
  Data.2 <- Data[gk.rw <= 4000000]

  coordinates(Data.1) <- c("gk.rw", "gk.hw")
  coordinates(Data.2) <- c("gk.rw", "gk.hw")

  # Gauss-Krueger reference system
  proj4string(Data.1) <- CRS("+proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +ellps=bessel +datum=potsdam +units=m +no_defs")
  proj4string(Data.2) <- CRS("+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +ellps=bessel +datum=potsdam +units=m +no_defs")

  # Tranforming to WGS84 longlat
  GK.WGS84.1 <- spTransform(Data.1, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  GK.WGS84.2 <- spTransform(Data.2, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

  rbind(as.data.table(GK.WGS84.1), as.data.table(GK.WGS84.2)) %>%
    .[order(id)] %>%
    .[, id := NULL] %>%
    setnames(c("gk.rw", "gk.hw"), c("longitude", "latitude")) %>%
    .[]
}
