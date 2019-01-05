rm(list = ls())

library(sp)
library(raster)
library(rgdal)
library(tidyverse)
library(rgeos)
library(mgcv)
library(stringi)
library(stringdist)

ascii_lower <- function (v) {
  stri_trans_general(v, "latin-ascii") %>% tolower()
}

## Creating nighttime light data
mun_shp_lcc <- shapefile("../raw_data/shapefiles_INEGI_SCINCE_2010/municipal.shp")
loc_urb_lcc <- shapefile("../raw_data/shapefiles_INEGI_SCINCE_2010/loc_urb.shp")
loc_rur_lcc <- shapefile("../raw_data/shapefiles_INEGI_SCINCE_2010/loc_rur.shp")

mun_shp_wgs <- spTransform(mun_shp_lcc, CRS("+init=epsg:4326"))
loc_rur_wgs <- spTransform(loc_rur_lcc, CRS("+init=epsg:4326"))
loc_urb_wgs <- spTransform(loc_urb_lcc, CRS("+init=epsg:4326"))

ntl_ras <- raster("../raw_data/nighttime_light_raster/SVDNB_npp_20150101-20151231_75N180W_vcm-orm-ntl_v10_c201701311200.avg_rade9.tif")
ntl_yuc <- crop(ntl_ras, mun_shp_wgs)

# plot(ntl_yuc > 10)
# plot(mun_shp_wgs, add=T)

ntl_yuc_wgs <- as.data.frame(ntl_yuc, xy = T)
head(ntl_yuc_wgs)
colnames(ntl_yuc_wgs)[3] <- "ntl"
head(ntl_yuc_wgs)

## Get rid of non-Yucatan points, use the state's shapefile
ntl_yuc_wgs_mat <- as.matrix(ntl_yuc_wgs[,c("x", "y")])
yuc <- shapefile("../raw_data/shapefiles_INEGI_SCINCE_2010/estatal.shp") %>%
  spTransform(CRS("+init=epsg:4326"))

# The shapefile is one 'polygons' object, which contains 54 'Polygon' objects
# Check if all points are within at least one of these 54 'Polygon'
l <- length(yuc@polygons[[1]]@Polygons)
coords <- yuc@polygons[[1]]@Polygons %>%
  map(~ .x@coords)
cond <- coords %>%
  map(~ in.out(.x, ntl_yuc_wgs_mat)) %>%
  unlist() %>%
  matrix(nrow = nrow(ntl_yuc_wgs_mat))
cond <- rowSums(cond) > 0
ntl_yuc_wgs <- ntl_yuc_wgs[cond,]

# plot(rasterFromXYZ(ntl_yuc_wgs))
# plot(mun_shp_wgs, add=T)

## Assign municipality to each point by checking if the point is
## within the polygon
ntl_yuc_wgs_mat <- as.matrix(ntl_yuc_wgs[,c("x", "y")])
ntl_yuc_wgs$MUNICIPIO <- NA

for (i in 1:nrow(mun_shp_lcc)) {
  coords <- mun_shp_wgs@polygons[[i]]@Polygons %>%
    map(~ .x@coords)
  cond <- coords %>%
    map(~ in.out(.x, ntl_yuc_wgs_mat)) %>%
    unlist() %>%
    matrix(nrow = nrow(ntl_yuc_wgs_mat))
  cond <- rowSums(cond) > 0
  ntl_yuc_wgs$MUNICIPIO[cond] <- mun_shp_lcc$NOMBRE[i]
}

sum(is.na(ntl_yuc_wgs$MUNICIPIO))

## Assign urban localities to each point
ntl_yuc_wgs$LOCALIDAD <- NA
ntl_yuc_wgs$CVEGEO <- NA
ntl_yuc_wgs$URB <- NA

for (i in 1:nrow(loc_urb_wgs)) {
  coords <- loc_urb_wgs@polygons[[i]]@Polygons[[1]]@coords
  cond <- in.out(coords, ntl_yuc_wgs_mat)
  ntl_yuc_wgs$LOCALIDAD[cond] <- loc_urb_wgs$NOMBRE[i]
  ntl_yuc_wgs$MUNICIPIO[cond] <- loc_urb_wgs$NOM_MUN[i]
  ntl_yuc_wgs$CVEGEO[cond] <- loc_urb_wgs$CVEGEO[i]
  ntl_yuc_wgs$URB[cond] <- "U"
}

ntl_yuc_wgs$URB[is.na(ntl_yuc_wgs$URB)] <- "R"

# Convert accented characters
ntl_yuc_wgs$LOCALIDAD <- ascii_lower(ntl_yuc_wgs$LOCALIDAD)
ntl_yuc_wgs$MUNICIPIO <- ascii_lower(ntl_yuc_wgs$MUNICIPIO)

# dir.create("../derived_data/")
write_csv(ntl_yuc_wgs, "../derived_data/yuc_ntl.csv")
