# Create data frame containing number, name and coordinates of each TK25 square in Germany

# Extract names and numbers of TK25 from kml file

# kml file containing all German TK25 squares can be downloaded here:
# http://www.orchids.de/haynold/tkq/KoordinatenErmittler.php?dl=1

# read kml file as text into R, set encoding to conserve special characters
kml.text <- readLines("inst/tkq_deutschland.kml", encoding = "UTF-8")

# get all entries of the character vector which contain a TK25 name
# by using a regular expression: match <name> followed by 4 numbers
re <- "<name>[0-9]{4}"
tk25 <- grep(re, kml.text, value = TRUE)

# get string between <name> and </name>
tk25 <- stringr::str_match(tk25, "<name>(.*?)</name>")[, 2]
# split string after 4 numbers and an empty space character to get the name of the TK25
tk25_names <- sapply(strsplit(tk25, "[0-9]{4} "), "[[", 2)
# get first 4 characters, the number of the TK25
tk25_numbers <- unname(sapply(tk25, substring, first = 1, last = 4))

# Get coordinates of border points of rectangle
tk25_coord <- create_raster(bounds = list(north = 55.0983, west = 6, east = 15.1647, south = 47.1992), diff_east_west = 0.1666454,
  diff_north_south = 0.09999027, east = 11.16538, west = 10.99873,
  south = 48.39901, north = 48.499)
# set TK25 numbers as rownames
lat <- formatC(seq(1, 57), flag = 0, width = 2)
lng <- formatC(seq(87, 9), flag = 0, width = 2)
numbers <- c()
for (i in seq_along(lng)) {
  numbers <- append(numbers, paste0(lng[i], lat))
}
rownames(tk25_coord) <- numbers

# now we have to merge tk25_coord and tk25_names using the numbers
tk25_names <- data.frame(name = tk25_names)
rownames(tk25_names) <- tk25_numbers
tk25 = merge(tk25_names, as.data.frame(tk25_coord), by = "row.names")
colnames(tk25)[1] <- "number"
tk25$name <- as.character(tk25$name)

# save tk25 data
usethis::use_data(tk25)
