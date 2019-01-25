#' Get geographic information for coordinates
#'
#' `get_geo_info` returns geographical information like country, tk25
#' from coordinates.
#'
#' @param .Data A data.table.
#'   You can either specify an input data.table with columns named `longitude`
#'   and `latitude` or pass on two equal-length vectors to `.longitude` and `.latitude`.
#' @param .longitude (numeric vector) \cr Longitude vector.
#' @param .latitude (numeric vector) Latitude vector.
#' @param .sites (SpatialPolygons object) \cr
#'   Spatial object to test if points lie inside sites.
#' @inheritParams get_geographic_data
#'
#' @return A data.table with new columns for the geographic informations.
#'
#' @export
#' @import data.table
#' @importFrom magrittr "%>%"
#'
#' @examples
#' geographic_data_de <- raster::getData("GADM", country = "DE", level = 2)
#' get_geo_info(.longitude = 11, .latitude = 48, .countries = list(geographic_data_de))
#' file.remove("GADM_2.8_DEU_adm2.rds")
get_geo_info <- function(.Data = NULL,
                         .longitude = NULL,
                         .latitude = NULL,
                         .countries = list(),
                      #   .sites = NULL,
                         ...) {

  # Input checks
  checkmate::assert_data_table(.Data, null.ok = TRUE)
  checkmate::assert_numeric(.longitude, null.ok = TRUE)
  checkmate::assert_numeric(.latitude, null.ok = TRUE)
  checkmate::assert_list(.countries)
  stopifnot(!is.null(.Data) || (!is.null(.latitude) && (!is.null(.longitude))))
  stopifnot(length(.longitude) == length(.latitude))

  if (is.null(.Data)) {
    .Data <- data.table(latitude = .latitude, longitude = .longitude)
  }

  # Get geographic units
  geo_info <- get_geographic_data(.Data, .countries = .countries)

  Data <- .Data %>%
    .[, Land := geo_info$NAME_0] %>%
    .[, Bundesland := geo_info$NAME_1] %>%
    .[, Kreis := geo_info$NAME_2] %>%
    .[, Gemeinde := geo_info$NAME_3] %>%
    .[, Ort := geo_info$NAME_4]

  # Get TK25 info
  tk25.number <- get_tk25_number(Data$latitude, Data$longitude)
  tk25.info <- get_tk25_info(tk25.number, .quadrant = NULL)

  Data <- Data %>%
    .[, TK25 := stringr::str_c(tk25.info$number, " ", tk25.info$name)] %>%
    .[, TK25_number := tk25.info$number] %>%
    .[, TK25_Quadrant_number := get_tk25_quadrant_number(latitude,
                                                         longitude,
                                                         tk25.info$center.lat,
                                                         tk25.info$center.lng)] %>%
    .[, TK25_Quadrant := stringr::str_c(tk25.info$number, "_", TK25_Quadrant_number, " ", tk25.info$name)]

  # # Extract site info
  # if (is.null(.sites)) {
  #   Data <- Data %>%
  #     .[, Fundort := NA_character_]
  # } else {
  #   Data.Geo <- Data
  #   sp::coordinates(Data.Geo) <- c("longitude", "latitude")
  #   sp::proj4string(Data.Geo) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  #   Data <- Data %>%
  #     .[, Fundort := over(Data.Geo, sites)$site]
  # }

  Data[]
}
