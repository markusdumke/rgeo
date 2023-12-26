#' Get geographic information for coordinates.
#'
#' This functions intersects coordinates with a set of shapefiles.
#'
#' @param .longitude A numeric vector of longitudes.
#' @param .latitude A numeric vector of latitudes.
#' @param .naturraum Optional sf polygon of naturraum data.
#' @param .boundaries Optional sf polygon of administrative boundaries.
#' @param .elevation Optional terra::rast of elevation values.
#' @param .tk25 Logical value. Should tk25 info be included?
#'
#' @return A data.table with one row per coordinate pair.
#' @export
#'
#' @examples
#' get_geo_info(
#'   .longitude = 11, .latitude = 48,
#'   .naturraum = naturraum_bayern,
#'   .boundaries = boundaries_bayern
#' )
get_geo_info <- function(.longitude,
                         .latitude,
                         .naturraum = NULL,
                         .boundaries = NULL,
                         .elevation = NULL,
                         .tk25 = TRUE) {

  checkmate::assert_numeric(.longitude)
  checkmate::assert_numeric(.latitude)

  coords <-
    sf::st_sfc(
      lapply(
        seq_along(.longitude), function(i) {
          sf::st_point(c(.longitude[i], .latitude[i]))
        }),
      crs = "+proj=longlat +datum=WGS84 +no_defs")

  data <- data.table(longitude = .longitude,
                     latitude = .latitude)

  # Get administrative boundaries
  if (!is.null(.boundaries)) {
    boundaries.info <-
      .boundaries[as.integer(sf::st_intersects(coords, .boundaries)), ] %>%
      setDT() %>%
      .[, geometry := NULL]
    data <- cbind(data, boundaries.info)
  }

  # Get naturraum
  if (!is.null(.naturraum)) {
    naturraum.info <-
      .naturraum[as.integer(sf::st_intersects(coords, .naturraum)), ] %>%
      setDT() %>%
      .[, geometry := NULL]
    data <- cbind(data, naturraum.info)
  }

  # Get elevation
  if (!is.null(.elevation)) {
    # Extract elevation values with terra
    data <-
      data[, hoehe := terra::extract(.elevation, data[, .(longitude, latitude)])$elevation]
  }

  # Get TK25
  if (.tk25) {
    tk25.info <- coordinates_to_tk25(.latitude = .latitude,
                                     .longitude = .longitude)
    data <- cbind(data, tk25.info)
  }

  data %>%
    setnames(stringr::str_to_lower) %>%
    .[]
}
