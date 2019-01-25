#' Get TK25 information.
#'
#' TK25 (Topographische Karte) is a german topographic named raster.
#' Get name, number or coordinates of corners and center points respectively.
#'
#' @param .Data A data.table with latitude and longitude columns.
#'
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#' get_tk25(data.table(latitude = 48, longitude = 11))
get_tk25 <- function(.Data) {
  checkmate::assert_data_frame(.Data)
  checkmate::assert_names(names(.Data), must.include = c("latitude", "longitude"))

  l <- data.table(number = character(NROW(.Data)),
                  name = character(NROW(.Data)))

  for (i in seq_len(nrow(.Data))) {
    tk <- .Data[i, latitude] < tk25[, "north"] &  .Data[i, longitude] < tk25[, "east"] &
      .Data[i, latitude] > tk25[, "south"] &  .Data[i, longitude] > tk25[, "west"]
    if (any(tk)) {
      l[i, ] <- tk25[tk, 1:2]
    } else {
      l[i, ] <- NA_character_
    }
  }
  l
}

#' @param .latitude Numeric vector of latitudes.
#' @param .longitude Numeric vectors of longitudes.
#'
#' @rdname get_tk25
#' @export
#'
#' @examples
#' get_tk25_number(48, 11)
get_tk25_number <- function(.latitude, .longitude) {
  dat <- data.table(
    lat.num = 16, lat = 54.24844, lat.dif = 0.09999027,
    lng.num = 48, lng = 13.74838, lng.dif = 0.1666454
  )
  # tk25 numbers consist of at least two numbers in each dimension
  lat.num <- sprintf("%02d", round(dat$lat.num - (.latitude - dat$lat) / dat$lat.dif) + 1)
  lng.num <- sprintf("%02d", round(dat$lng.num + (.longitude - dat$lng) / dat$lng.dif))
  paste0(lat.num, lng.num)
}

#' @param .tk25.center.latitude Numeric vector of latitudes. Center of TK25.
#' @param .tk25.center.longitude Numeric vector of longitudes. Center of TK25.
#'
#' @rdname get_tk25
#' @export
#'
#' @examples
#' get_tk25_quadrant_number(53.6, 10.3, 53.6485, 10.24883)
get_tk25_quadrant_number <- function(.latitude, .longitude,
                                     .tk25.center.latitude,
                                     .tk25.center.longitude) {



  lest::case_when(
    .latitude - .tk25.center.latitude >= 0 & .longitude >= .tk25.center.longitude ~ 2,
    .latitude - .tk25.center.latitude < 0 & .longitude >= .tk25.center.longitude ~ 4,
    .latitude - .tk25.center.latitude >= 0 & .longitude < .tk25.center.longitude ~ 1,
    .latitude - .tk25.center.latitude < 0 & .longitude < .tk25.center.longitude ~ 3
  )
}

#' @param .tk25.number Character or numeric vector. Number of TK25 square,
#'        e.g. "1648" or "1648_1" (allowed in get_tk25_info).
#'
#' @rdname get_tk25
#' @export
#'
#' @examples
#' get_tk25_coordinates("2327")
#' get_tk25_coordinates("2327", "1")
#' get_tk25_name("2327")
#' get_tk25_info("2327")
get_tk25_coordinates <- function(.tk25.number, .quadrant = NULL) {
  tk25.number <- as.character(.tk25.number)
  dat <- data.table(
    lat.num = 16, lat = 54.24844, lat.dif = 0.09999027,
    lng.num = 48, lng = 13.74838, lng.dif = 0.1666454
  )
  # how to split?
  # longitude: take last two entries as longitude number
  lng.num <- as.integer(stringr::str_sub(tk25.number, start = -2, end = -1))
  lat.num <- as.integer(stringr::str_sub(tk25.number, start = 1, end = -3))
  lng.center <- dat$lng + (lng.num - dat$lng.num) * dat$lng.dif
  lat.center <- dat$lat - (lat.num - dat$lat.num - 1) * dat$lat.dif
  x <- 2
  if (!is.null(.quadrant)) {
    quadrant <- as.numeric(.quadrant)
    x <- 4
    lng.center <- lest::case_when(
      quadrant %in% c(2, 4) ~ lng.center + dat$lng.dif / x,
      quadrant %in% c(1, 3) ~ lng.center - dat$lng.dif / x
    )
    lat.center <- lest::case_when(
      quadrant %in% c(1, 2) ~ lat.center + dat$lat.dif / x,
      quadrant %in% c(3, 4) ~ lat.center - dat$lat.dif / x
    )
  } else {
    quadrant <- NA
  }
  south <- lat.center - dat$lat.dif / x
  north <- lat.center + dat$lat.dif / x
  east <- lng.center + dat$lng.dif / x
  west <- lng.center - dat$lng.dif / x
  data.table(
    number = tk25.number,
    quadrant.number = quadrant,
    south = south,
    north = north,
    west = west,
    east = east,
    center.lat = lat.center,
    center.lng = lng.center
  )
}

#' @rdname get_tk25
#' @export
get_tk25_name <- function(.tk25.number) {
  # the problem is that non-matches return character(0) and will be dropped
  # solution: preallocate vector names and fill in matches and leave non-matches at NA
  names <- rep(NA_character_, length(.tk25.number))
  # FIXME: wrong oder here
  for (i in seq_along(.tk25.number)) {
    name <- tk25[as.character(.tk25.number[i]) == tk25$number, "name"]
    names[i] <- ifelse(length(name) > 0, name, NA_character_)
  }
  names
}


#' @param .quadrant Character, numeric or logical vector. Number of TK25 Quadrant, between 1 and 4.
#'
#' @rdname get_tk25
#' @export
get_tk25_info <- function(.tk25.number, .quadrant = NULL) {
  if (isTRUE(.quadrant)) {
    split.string <- stringr::str_split(.tk25.number, "_", simplify = TRUE)
    .tk25.number <- split.string[, 1]
    .quadrant <- split.string[, 2]
  }
  df <- get_tk25_coordinates(.tk25.number, .quadrant)
  df$name <- get_tk25_name(.tk25.number)

  if (!is.null(.quadrant)) {
    .quadrant <- stringr::str_c(" Q", df$quadrant.number)
  } else {
    .quadrant <- ""
  }

  df$full.name <- stringr::str_c( df$number, .quadrant, " ",  df$name)
  df
}

# # add this to tests:
# n <- sample(1:nrow(data), size = 100)
# for (i in n) {
#   print(paste0("True: ", data[i, "TK25"], " - ",
#   "Predicted: ", get_tk25_number(data[i, "latitude"], data[i, "longitude"])))
# }
#
# data$TK25_number <- stringr::str_extract(data$TK25, "\\d*")
# pred <- get_tk25_number(data[["latitude"]], data[["longitude"]])
#
# all(data$TK25_number == pred, na.rm = TRUE)
