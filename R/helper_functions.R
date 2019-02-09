check_data <- function(.Data, .latitude, .longitude) {
  # Input checks
  checkmate::assert_data_table(.Data, null.ok = TRUE)
  if (!is.null(.Data))
    checkmate::assert_names(names(.Data), must.include = c("latitude", "longitude"))
  checkmate::assert_numeric(.longitude, null.ok = TRUE)
  checkmate::assert_numeric(.latitude, null.ok = TRUE)
  stopifnot(!is.null(.Data) || (!is.null(.latitude) && (!is.null(.longitude))))
  stopifnot(length(.longitude) == length(.latitude))

  if (is.null(.Data)) {
    .Data <- data.table(latitude = .latitude, longitude = .longitude)
  }
  .Data[, .(latitude, longitude)]
}

transform_data <- function(.Data) {
  sp::coordinates(.Data) <- c("longitude", "latitude")
  sp::proj4string(.Data) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  .Data
}
