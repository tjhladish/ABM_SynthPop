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

#### Impute data
## Data
dat <- read_csv("../derived_data/sch_dat/dat_need_impute.csv")
ntl <- read.csv("../derived_data/yuc_ntl.csv")

mun_shp_lcc <- shapefile("../raw_data/shapefiles_INEGI_SCINCE_2010/municipal.shp")
loc_urb_lcc <- shapefile("../raw_data/shapefiles_INEGI_SCINCE_2010/loc_urb.shp")
loc_rur_lcc <- shapefile("../raw_data/shapefiles_INEGI_SCINCE_2010/loc_rur.shp")

mun_shp_wgs <- spTransform(mun_shp_lcc, CRS("+init=epsg:4326"))
loc_rur_wgs <- spTransform(loc_rur_lcc, CRS("+init=epsg:4326"))
loc_urb_wgs <- spTransform(loc_urb_lcc, CRS("+init=epsg:4326"))

## Create spatial points objects so CRS is captured
ntl_mat <- as.matrix(ntl[,c("x", "y")])
ntl_mat_wgs <- SpatialPoints(ntl_mat, CRS("+init=epsg:4326"))
ntl_mat_lcc <- spTransform(ntl_mat_wgs, crs(loc_rur_lcc))

## Sampling matched schools
# Sampling criteria - for urban, points must be in urban polygons
# For rural, (1) Within municipal, (2) Not in urban, (3) 5km from ref point
get_valid_point <- function (cvegeo, urb) {
  if(urb == "U") {
    cond <- ntl$CVEGEO == cvegeo
    cond <- ifelse(is.na(cond), F, cond)
  } else {
    mun <- loc_rur_wgs$NOM_MUN[loc_rur_wgs$CVEGEO == cvegeo] %>%
      ascii_lower()
    cond1 <- ntl$MUNICIPIO == mun & ntl$URB == "R"
    cond1 <- ifelse(is.na(cond1), F, cond1)
    ind1 <- which(cond1)
    
    cond <- rep(F, length(cond1))
    pol <- loc_rur_lcc[loc_rur_lcc$CVEGEO == cvegeo,]
    cond2 <- (gDistance(ntl_mat_lcc[ind1], pol, byid = T) %>% as.vector()) < 5000
    cond[ind1] <- cond2
  }
  
  return(cond)
}

# sample a point from all valid points
sample_pt <- function (pts_df) {
  if(nrow(pts_df) == 0) {
    return(data.frame(long=NA, lat=NA, ntl=NA))
  } else {
    samp <- sample(1:nrow(pts_df), size = 1, prob = pts_df$prob)
    return(pts_df[samp,1:3])
  }
}

set.seed(4326)
ids <- dat %>%
  dplyr::filter(!is.na(CVEGEO)) %>%
  .$ID_unique %>% unique()
n <- length(ids)
samppt_df <- data.frame(ID_unique = ids,
                        long = numeric(n),
                        lat = numeric(n),
                        ntl = numeric(n))
for (i in 1:n) {
  id <- ids[i]
  cvegeo <- dat$CVEGEO[dat$ID_unique == id]
  urb <- dat$URB[dat$ID_unique == id]
  
  cond <- map2(cvegeo, urb, ~ get_valid_point(.x, .y)) %>%
    unlist() %>%
    matrix(nrow = nrow(ntl)) %>%
    rowSums() > 0
  
  ntl_sub <- ntl[cond,] %>%
    mutate(ntl2 = ntl+0.0001) %>%
    mutate(prob = ntl2/sum(ntl2)) %>%
    as.data.frame()
  
  samppt_df[i, 2:4] <- sample_pt(ntl_sub)
  print(samppt_df[i,])
}

# 9 unmatched ones from this round of sampling
samppt_df_comp <- samppt_df[!is.na(samppt_df$long),]

# Municpality-based sampling
ids1 <- samppt_df[is.na(samppt_df$long),"ID_unique"]
ids2 <- dat %>%
  dplyr::filter(is.na(CVEGEO)) %>%
  .$ID_unique %>% unique()
ids <- c(ids1, ids2) %>% sort

dat_nomatch <- dat[dat$ID_unique %in% ids,] %>%
  arrange(ID_unique)

# Assume rural for all unmatched ones 
get_valid_pt_unmatched <- function (mun) {
  cond <- ntl$MUNICIPIO == mun & ntl$URB == "R"
  return(cond)
}

# n <- length(ids)
samppt_unmatch_df <- dat_nomatch$MUNICIPIO %>%
  map(~ get_valid_pt_unmatched(.x)) %>%
  map(~ ntl[.x,] %>%
        mutate(ntl2 = ntl+0.0001) %>%
        mutate(prob = ntl2/sum(ntl2)) %>%
        as.data.frame()) %>%
  map_df(~ sample_pt(.x))

samppt_unmatch_df$ID_unique <- ids
colnames(samppt_unmatch_df)[1:2] <- c("long", "lat")

samppt_full <- bind_rows(samppt_df_comp, samppt_unmatch_df)

## Jitter
resolution <- 15/3600
x <- runif(nrow(samppt_full), -resolution/2, resolution/2)
y <- runif(nrow(samppt_full), -resolution/2, resolution/2)
samppt_full$long <- samppt_full$long + x
samppt_full$lat <- samppt_full$lat + y

## Write to csv
write_csv(samppt_full, "../derived_data/sch_dat/dat_imputed.csv")
