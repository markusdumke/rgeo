#' Get geographic information for coordinates
#'
#' @inheritParams get_geo_info
#' @param .countries A list with geographic data from [raster::getData()].
#'
#' @return A list containing named entries NAME_0, NAME_1, NAME_2, NAME_3, NAME_4.
#'         See [raster::getData()].
#'
#' @export
#' @import data.table
#' @importFrom magrittr "%>%"
#' @seealso [raster::getData()]
#'
#' @examples
#' library(data.table)
#' Data <- data.table(latitude = 48:50, longitude = 11:13)
#' geographic_data_de <- raster::getData("GADM", country = "DE", level = 2)
#' get_geographic_data(Data, .countries = list(geographic_data_de))
#' file.remove("GADM_2.8_DEU_adm2.rds")
get_geographic_data <- function(.Data = NULL, .latitude = NULL, .longitude = NULL, .countries = list()) {

  # Input checks
  Data.Geo <-
    check_data(.Data, .latitude = .latitude, .longitude = .longitude)

  for (i in seq_along(.countries)) {
    .countries[[i]] <- sp::spTransform(.countries[[i]],
                                       CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  }

  # Preallocate result
  res <- data.table(NAME_0 = rep(NA_character_, NROW(Data.Geo)),
                    NAME_1 = rep(NA_character_, NROW(Data.Geo)),
                    NAME_2 = rep(NA_character_, NROW(Data.Geo)),
                    NAME_3 = rep(NA_character_, NROW(Data.Geo)),
                    NAME_4 = rep(NA_character_, NROW(Data.Geo)))

  where.na <- rep(TRUE, NROW(Data.Geo))

  for (i in .countries) {

    geo <- sp::over(Data.Geo[where.na] %>% transform_data, i)
    vars <- intersect(names(res), names(geo))
    res[where.na, (vars) := geo[, vars]]

    where.na <- !is.na(res$NAME_0)
  }
  setnames(res, paste0("NAME_", 0:4), c("Land", "Bundesland", "Kreis", "Gemeinde", "Ort"))
  res[Land == "Germany", Land := "Deutschland"]
  res[]
}
