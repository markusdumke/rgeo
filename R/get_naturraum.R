#' Get naturraum (Bavaria)
#'
#' This function returns information on the natural region (Naturraum) in Bavaria
#' for given coordinates.
#'
#' @inheritParams get_geo_info
#'
#' @return A data.table with three columns.
#' @export
#'
#' @examples
#' get_naturraum(data.table(latitude = 48, longitude = 11))
get_naturraum <- function(.Data = NULL, .latitude = NULL, .longitude = NULL) {

  Data.Geo <-
    check_data(.Data, .latitude = .latitude, .longitude = .longitude) %>%
    transform_data

  naturraum <- sp::spTransform(rgeo::naturraum, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

  sp::over(Data.Geo, naturraum) %>%
    setDT %>%
    .[, .(HAUPT_NAME, NAME, GROSSLAND)] %>%
    setnames(c("HAUPT_NAME", "NAME", "GROSSLAND"),
             c("Naturraum_Hauptname", "Naturraum_Name", "Naturraum_Grosslandschaft")) %>%
    .[]
}
