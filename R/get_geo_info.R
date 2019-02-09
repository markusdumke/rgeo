#' Get geographic information for coordinates
#'
#' `get_geo_info` returns geographical information like country, tk25
#' from coordinates.
#'
#' @param .Data A data.table.
#'        You can either specify an input data.table with columns named `longitude`
#'        and `latitude` or pass on two equal-length vectors to `.longitude` and `.latitude`.
#' @param .longitude Numeric vector of longitudes.
#' @param .latitude Numeric vector of latitudes.
#' @param .sites SpatialPolygons object.
#'        Spatial object to test if points lie inside sites.
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
                         .latitude = NULL,
                         .longitude = NULL,
                         .countries = list()) {

  Data <-
    check_data(.Data, .latitude = .latitude, .longitude = .longitude)

  # Get geographic units
  geo.info <- get_geographic_data(.Data = .Data,
                                  .latitude = .latitude,
                                  .longitude = .longitude,
                                  .countries = .countries)

  # Get naturraum
  naturraum.info <- get_naturraum(.Data = .Data,
                                  .latitude = .latitude,
                                  .longitude = .longitude)

  # Get TK25 info
  tk25.info <- get_tk25_full(.Data = .Data,
                             .latitude = .latitude,
                             .longitude = .longitude)

  cbind(Data, geo.info, tk25.info, naturraum.info)
}
