library(sp)
library(rgdal)
library(purrr)

# 1. Biogeographische Regionen
# ng_geo.shp: alpin vs kontinental (2)

# 2. Großlandschaften
# ng_gross: Alpen, Mittelgebirge, ...

# 3. Naturraum-Haupteinheiten (Ssymank)
# ng_ssymank: Hauptnaturraeume (18)
# ng_ssymank/ng_haupt.shp

# 4. Naturraum-Einheiten (Meynen/Schmithüsen et. al.)
# ng_meynen_schmit: Unternaturraeume

# 5. Naturraum-Untereinheiten (ABSP)
# ng_unter_absp: detailliert
# ng_abspziele: detailliert
# ng_unter_absp/ng_unter.shp

naturraum <- readOGR(dsn = paste0("naturraeume_bayern/ng_unter_absp/ng_unter.shp"),
                     stringsAsFactors = FALSE,
                     encoding = "UTF-8")
sp::proj4string(naturraum) <- "+proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +ellps=bessel +datum=potsdam +units=m +no_defs"
naturraum <- spTransform(naturraum, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
naturraum@data <- modify(naturraum@data, enc2utf8)

usethis::use_data(naturraum, overwrite = TRUE)


# sp::spTransform(rgeo::naturraum,
#                 sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
# naturraum <- raster::aggregate(naturraum_by_einheiten, by = "NAME")
# saveRDS(naturraum, "data/geographic_data/naturraum_bayern.rds")
