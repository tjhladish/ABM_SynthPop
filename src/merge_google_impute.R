rm(list = ls())

library(sp)
library(raster)
library(rgdal)
library(tidyverse)
library(rgeos)
library(mgcv)
library(stringi)
library(stringdist)

ntl <- read.csv("../derived_data/yuc_ntl.csv")
dat_google <- read_csv("../derived_data/sch_dat/dat_google_accepted.csv")
dat_impute <- read_csv("../derived_data/sch_dat/dat_imputed.csv")

dat <- read.csv("../raw_data/SEGEY_school_data_2017/catalogo_escuelas-uniq.csv")
dat$ID_unique <- 1:nrow(dat)

#### Merging into fullset
ntl_raster <- rasterFromXYZ(ntl[,c('x', 'y', 'ntl')], crs = CRS("+init=epsg:4326"))
coords <- SpatialPoints(dat_google[,c('long', 'lat')], 
                        proj4string = CRS("+init=epsg:4326"))
accepted_ntl <- raster::extract(x = ntl_raster, coords)
dat_google$ntl <- accepted_ntl

dat_complete <- bind_rows(dat_google, dat_impute) %>%
  arrange(ID_unique)
dat_complete <- dat %>%
  right_join(dat_complete)

write_csv(dat_complete, "../derived_data/sch_dat/final_school_coordinates.csv")