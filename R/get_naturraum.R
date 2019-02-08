#' Get naturraum (Bavaria)
#'
#' @inheritParams get_geo_info
#'
#' @return A data.table with three columns.
#' @export
#'
#' @examples
#' get_naturraum(data.table(latitude = 48, longitude = 11))
get_naturraum <- function(.Data = NULL, .latitude = NULL, .longitude = NULL) {

  # Input checks
  checkmate::assert_data_table(.Data, null.ok = TRUE)
  checkmate::assert_numeric(.longitude, null.ok = TRUE)
  checkmate::assert_numeric(.latitude, null.ok = TRUE)
  stopifnot(!is.null(.Data) || (!is.null(.latitude) && (!is.null(.longitude))))
  stopifnot(length(.longitude) == length(.latitude))

  if (is.null(.Data)) {
    .Data <- data.table(latitude = .latitude, longitude = .longitude)
  }

  data_geo <- .Data %>% copy
  sp::coordinates(data_geo) <- c("longitude", "latitude")
  sp::proj4string(data_geo) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  naturraum <- sp::spTransform(naturraum, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

  sp::over(data_geo, naturraum) %>%
    setDT %>%
    .[, .(HAUPT_NAME, NAME, GROSSLAND)]
}
