#' Get geographic information for coordinates
#'
#' @param .Data A data.table containing latitude and longitude columns.
#' @param .countries A list with geographic data from [raster::getData()].
#'
#' @return a list containing named entries NAME_0, NAME_1, NAME_2, NAME_3, NAME_4. See [raster::getData()].
#'
#' @export
#' @import data.table
#' @importFrom magrittr "%>%"
#' @seealso [raster::getData()]
#'
#' @examples
#' library(data.table)
#' data <- data.table(latitude = 48, longitude = 11)
#' geographic_data_de <- raster::getData("GADM", country = "DE", level = 2)
#' get_geographic_data(data, .countries = list(geographic_data_de))
#' file.remove("GADM_2.8_DEU_adm2.rds")
get_geographic_data <- function(.Data, .countries = list()) {

  # make SpatialPointsDataFrame
  data_geo <- .Data %>% copy
  sp::coordinates(data_geo) <- c("longitude", "latitude")
  # use same lat/lon reference system
  sp::proj4string(data_geo) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  # which Land/Bundesland/Kreis/Gemeinde (if any) contains each sighting
  # store the the name in a list, initialize with NAs
  empty <- rep(NA, nrow(data))
  res <- list(NAME_0 = empty, NAME_1 = empty, NAME_2 = empty,
              NAME_3 = empty, NAME_4 = empty)

  for (i in .countries) {
    which_administrative <- sp::over(data_geo, i)
    where_not_na <- !is.na(which_administrative$NAME_0)
    if (!is.null(which_administrative$NAME_0[where_not_na])) {
      res$NAME_0[where_not_na] <- which_administrative$NAME_0[where_not_na]

      # lookup <- tibble::tribble(~"deutsch", ~"englisch",
      #                           "Deutschland", "Germany",
      #                           "Italien", "Italy",
      #                           "Schweiz", "Switzerland",
      #                           "Österreich", "Austria")
      # res$NAME_0 <- lookup$deutsch[match(unlist(res$NAME_0), lookup$englisch)]
      res$NAME_0 <- res$NAME_0
    }
    if (!is.null(which_administrative$NAME_1[where_not_na])) {
      res$NAME_1[where_not_na] <- which_administrative$NAME_1[where_not_na]
    }
    if (!is.null(which_administrative$NAME_2[where_not_na])) {
      res$NAME_2[where_not_na] <- which_administrative$NAME_2[where_not_na]
    }
    if (!is.null(which_administrative$NAME_3[where_not_na])) {
      res$NAME_3[where_not_na] <- which_administrative$NAME_3[where_not_na]
    }
    if (!is.null(which_administrative$NAME_4[where_not_na])) {
      res$NAME_4[where_not_na] <- which_administrative$NAME_4[where_not_na]
    }
  }
  res
}
