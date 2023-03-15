
#' Coordinates to TK25
#'
#' @inheritParams get_geo_info
#'
#' @export
#'
#' @examples
#' coordinates_to_tk25(11, 48)
coordinates_to_tk25 <- function(.longitude = NULL, .latitude = NULL) {

  checkmate::assert_numeric(.longitude)
  checkmate::assert_numeric(.latitude)

  tk25.number <- get_tk25_number(.latitude = .latitude,
                                 .longitude = .longitude)
  tk25.name <- get_tk25_name(tk25.number)
  tk25.info <- get_tk25_coordinates(tk25.number, .quadrant = NULL)
  tk25.quadrant.number <- get_tk25_quadrant_number(.latitude = .latitude,
                                                   .longitude = .longitude,
                                                   .tk25.center.latitude = tk25.info$center.lat,
                                                   .tk25.center.longitude = tk25.info$center.lng)
  tk25.quadrant.info <- get_tk25_coordinates(tk25.number, .quadrant = tk25.quadrant.number)
  tk25.16 <- get_tk25_quadrant_number(.latitude = .latitude,
                                      .longitude = .longitude,
                                      .tk25.center.latitude = tk25.quadrant.info$center.lat,
                                      .tk25.center.longitude = tk25.quadrant.info$center.lng)
  tk25.16.info <- get_tk25_coordinates(tk25.number, .quadrant = tk25.quadrant.number, .q16 = tk25.16)
  tk25.64 <- get_tk25_quadrant_number(.latitude = .latitude,
                                      .longitude = .longitude,
                                      .tk25.center.latitude = tk25.16.info$center.lat,
                                      .tk25.center.longitude = tk25.16.info$center.lng)
  data.table(
    tk25 = stringr::str_c(tk25.number, " ", tk25.name),
    tk25_quadrant = stringr::str_c(tk25.number, " ", tk25.quadrant.number, "0 ", tk25.name),
    tk25_viertelquadrant = stringr::str_c(tk25.number, " ", tk25.quadrant.number, tk25.16, " ", tk25.name),
    tk25_64 = stringr::str_c(tk25.number, " ", tk25.quadrant.number, tk25.16, tk25.64, " ", tk25.name)
  )
}

#' TK25 to coordinates
#'
#' @param .tk25 TK25 Number. String in format "7933 33".
#' @param .quadrant Boolean value.
#' @param .viertelquadrant Boolean value.
#'
#' @export
#' @examples
#' tk25_to_coordinates("7932 33 Utting")
tk25_to_coordinates <- function(.tk25, .quadrant = FALSE, .viertelquadrant = FALSE) {

  split <- stringr::str_split(.tk25, stringr::fixed(" "))
  lapply(split,
         function(.x) {
           tk25.number <- .x[1]
           quadrant <- `if`(isTRUE(.quadrant), stringr::str_sub(.x[2], 1, 1), NULL)
           viertelquadrant <- `if`(isTRUE(.viertelquadrant), stringr::str_sub(.x[2], 2, 2), NULL)

           get_tk25_coordinates(tk25.number, quadrant, viertelquadrant)
         }) %>%
    rbindlist()
}


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


get_tk25_coordinates <- function(.tk25.number, .quadrant = NULL, .q16 = NULL) {
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
    if (!is.null(.q16)) {
      q16 <- as.numeric(.q16)
      x <- 8
      lng.center <- lest::case_when(
        q16 %in% c(2, 4) ~ lng.center + dat$lng.dif / x,
        q16 %in% c(1, 3) ~ lng.center - dat$lng.dif / x
      )
      lat.center <- lest::case_when(
        q16 %in% c(1, 2) ~ lat.center + dat$lat.dif / x,
        q16 %in% c(3, 4) ~ lat.center - dat$lat.dif / x
      )
    }
  }
  south <- lat.center - dat$lat.dif / x
  north <- lat.center + dat$lat.dif / x
  east <- lng.center + dat$lng.dif / x
  west <- lng.center - dat$lng.dif / x
  data.table(
    south = south,
    north = north,
    west = west,
    east = east,
    center.lat = lat.center,
    center.lng = lng.center
  )
}


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










################################# OLD ##########################################

# dat <- data.table(
#   lat.num = 17, lat = 54.24844, lat.dif = 0.09999027,
#   lng.num = 48, lng = 13.7481, lng.dif = 0.1666454
# )
# # tk25 numbers consist of at least two numbers in each dimension
# lat.diff <- dat$lat.num - (.latitude - dat$lat) / dat$lat.dif
# lng.diff <- dat$lng.num + (.longitude - dat$lng) / dat$lng.dif
# lat.num <- sprintf("%02d", round(lat.diff))
# lng.num <- sprintf("%02d", round(lng.diff))
# tk25.number <- paste0(lat.num, lng.num)
#
#
# tk25.quadrant <-
#   lest::case_when(
#     lat.diff - round(lat.diff) < 0 && lng.diff - round(lng.diff) < 0 ~ 1,
#     lat.diff - round(lat.diff) < 0 && lng.diff - round(lng.diff) > 0 ~ 2,
#     lat.diff - round(lat.diff) > 0 && lng.diff - round(lng.diff) < 0 ~ 3,
#     lat.diff - round(lat.diff) > 0 && lng.diff - round(lng.diff) > 0 ~ 4
#   )
#
#' #' Get TK25 1/16 number
#' #'
#' #' @inheritParams get_geo_info
#' #'
#' #' @rdname get_tk25
#' #' @export
#' #'
#' #' @examples
#' #' get_tk25_16_number(.latitude = 48, .longitude = 11)
#' get_tk25_16_number <- function(.Data = NULL, .latitude = NULL, .longitude = NULL) {
#'
#'   Data <- check_data(.Data, .latitude = .latitude, .longitude = .longitude)
#'
#'   tk25.number <- get_tk25_number(.latitude = Data$latitude, .longitude = Data$longitude)
#'   tk25.info <- get_tk25_info(tk25.number, .quadrant = NULL)
#'   tk25.quadrant.info <- get_tk25_quadrant_number(.latitude = Data$latitude,
#'                                                  .longitude = Data$longitude,
#'                                                  .tk25.center.latitude = tk25.info$center.lat,
#'                                                  .tk25.center.longitude = tk25.info$center.lng)
#'   if (tk25.quadrant.info %in% c(1, 2)) {
#'     center.lat <- tk25.info$center.lat + 0.09999027 / 4
#'   }
#'   if (tk25.quadrant.info %in% c(3, 4)) {
#'     center.lat <- tk25.info$center.lat - 0.09999027 / 4
#'   }
#'   if (tk25.quadrant.info %in% c(1, 3)) {
#'     center.lng <- tk25.info$center.lng - 0.1666454 / 4
#'   }
#'   if (tk25.quadrant.info %in% c(2, 4)) {
#'     center.lng <- tk25.info$center.lng + 0.1666454 / 4
#'   }
#'   tk25.16 <- get_tk25_quadrant_number(.latitude = Data$latitude,
#'                                       .longitude = Data$longitude,
#'                                       .tk25.center.latitude = center.lat,
#'                                       .tk25.center.longitude = center.lng)
#'   data.table(
#'     TK25 = tk25.info$full.name,
#'     TK25_Nummer = tk25.info$number,
#'     TK25_Quadrant = stringr::str_c(tk25.info$number, " ", tk25.quadrant.info, "0",
#'                                    ifelse(is.na(tk25.info$name), "", stringr::str_c(" ", tk25.info$name))),
#'     TK25_Quadrant_Nummer = as.character(tk25.quadrant.info),
#'     TK25_Q16 = stringr::str_c(tk25.number, " ", tk25.quadrant.info, tk25.16, " ", tk25.info$name),
#'     TK25_Q16_Nummer = stringr::str_c(tk25.number, " ", tk25.quadrant.info, tk25.16)
#'   )
#'
#' }
#'
#' #' Get TK25 information.
#' #'
#' #' TK25 (Topographische Karte) is a german topographic named raster.
#' #' Get name, number or coordinates of corners and center points respectively.
#' #'
#' #' @param .Data A data.table with latitude and longitude columns.
#' #'
#' #' @export
#' #' @import data.table
#' #'
#' #' @examples
#' #' library(data.table)
#' #' get_tk25(data.table(latitude = 48, longitude = 11))
#' get_tk25 <- function(.Data) {
#'   checkmate::assert_data_frame(.Data)
#'   checkmate::assert_names(names(.Data), must.include = c("latitude", "longitude"))
#'
#'   l <- data.table(number = character(NROW(.Data)),
#'                   name = character(NROW(.Data)))
#'
#'   for (i in seq_len(nrow(.Data))) {
#'     tk <- .Data[i, latitude] < tk25[, "north"] &  .Data[i, longitude] < tk25[, "east"] &
#'       .Data[i, latitude] > tk25[, "south"] &  .Data[i, longitude] > tk25[, "west"]
#'     if (any(tk)) {
#'       l[i, ] <- tk25[tk, 1:2]
#'     } else {
#'       l[i, ] <- NA_character_
#'     }
#'   }
#'   l
#' }
#'
#' #' @param .quadrant Character, numeric or logical vector. Number of TK25 Quadrant, between 1 and 4.
#' #'
#' #' @rdname get_tk25
#' #' @export
#' get_tk25_info <- function(.tk25.number, .quadrant = NULL) {
#'   if (isTRUE(.quadrant)) {
#'     split.string <- stringr::str_split(.tk25.number, "_", simplify = TRUE)
#'     .tk25.number <- split.string[, 1]
#'     .quadrant <- split.string[, 2]
#'   }
#'   df <- get_tk25_coordinates(.tk25.number, .quadrant)
#'   df$name <- get_tk25_name(.tk25.number)
#'
#'   if (!is.null(.quadrant)) {
#'     .quadrant <- stringr::str_c(" Q", df$quadrant.number)
#'   } else {
#'     .quadrant <- ""
#'   }
#'
#'   df$full.name <- stringr::str_c(df$number, .quadrant,
#'                                  ifelse(is.na(df$name), "", stringr::str_c(" ",  df$name)))
#'
#'   df
#' }
#'
#'
#' #' Get TK25 information
#' #' @inheritParams get_geo_info
#' #' @export
#' #' @examples
#' #' get_tk25_full(.latitude = 47.5, .longitude = 11)
#' get_tk25_full <- function(.Data = NULL, .latitude = NULL, .longitude = NULL) {
#'
#'   Data <- check_data(.Data, .latitude = .latitude, .longitude = .longitude)
#'
#'   tk25.number <- get_tk25_number(.latitude = Data$latitude, .longitude = Data$longitude)
#'   tk25.info <- get_tk25_info(tk25.number, .quadrant = NULL)
#'   tk25.quadrant.info <- get_tk25_quadrant_number(.latitude = Data$latitude,
#'                                                  .longitude = Data$longitude,
#'                                                  .tk25.center.latitude = tk25.info$center.lat,
#'                                                  .tk25.center.longitude = tk25.info$center.lng)
#'   data.table(
#'     TK25 = tk25.info$full.name,
#'     TK25_Nummer = tk25.info$number,
#'     TK25_Quadrant = stringr::str_c(tk25.info$number, " ", tk25.quadrant.info, "0",
#'                                    ifelse(is.na(tk25.info$name), "", stringr::str_c(" ", tk25.info$name))),
#'     TK25_Quadrant_Nummer = as.character(tk25.quadrant.info)
#'   )
#' }
#'
#'
#' # # add this to tests:
#' # n <- sample(1:nrow(data), size = 100)
#' # for (i in n) {
#' #   print(paste0("True: ", data[i, "TK25"], " - ",
#' #   "Predicted: ", get_tk25_number(data[i, "latitude"], data[i, "longitude"])))
#' # }
#' #
#' # data$TK25_number <- stringr::str_extract(data$TK25, "\\d*")
#' # pred <- get_tk25_number(data[["latitude"]], data[["longitude"]])
#' #
#' # all(data$TK25_number == pred, na.rm = TRUE)
