rm(list = ls())

library(sp)
library(raster)
library(rgdal)
library(tidyverse)
library(rgeos)
library(mgcv)
library(stringi)
library(stringdist)

# setwd("src/")
parsed_dat <- read_tsv("../derived_data/sch_google_json_parsed.out", col_names = F)
str(parsed_dat)
parsed_dat <- parsed_dat %>%
  dplyr::select(ID_unique = X1,
                complete = X4,
                school = X6,
                long = X9,
                lat = X8,
                coord_type = X10,
                ID_original = X12)

dat <- read.csv("../raw_data/SEGEY_school_data_2017/catalogo_escuelas-uniq.csv")

dat_unique <- dat %>%
  dplyr::select(ID_original = ID, MUNICIPIO, LOCALIDAD)

dat_unique$ID_unique <- 1:nrow(dat_unique)
parsed_dat$ID_unique <- parsed_dat$ID_unique + 1

dat_joined <- parsed_dat %>%
  left_join(dat_unique, by = c("ID_unique" = "ID_unique",
                               "ID_original" = "ID_original"))

#### Build Municipality and Locality data
# Functions: Convert to unaccented and lowercase characters
ascii_lower <- function (v) {
  stri_trans_general(v, "latin-ascii") %>% tolower()
}

# Municipality
mun_shp <- shapefile("../raw_data/shapefiles_INEGI_SCINCE_2010/municipal.shp") %>%
  spTransform(CRS("+init=epsg:4326"))

# Locality data
loc_urb_lcc <- shapefile("../raw_data/shapefiles_INEGI_SCINCE_2010/loc_urb.shp")
loc_urb_wgs <- loc_urb_lcc %>%
  spTransform(CRS("+init=epsg:4326"))
loc_rur_lcc <- shapefile("../raw_data/shapefiles_INEGI_SCINCE_2010/loc_rur.shp")
loc_rur_wgs <- loc_rur_lcc %>%
  spTransform(CRS("+init=epsg:4326"))

# Build table of localities
tmp1 <- loc_urb_lcc@data %>% 
  dplyr::select(CVEGEO, MUNICIPIO = NOM_MUN, LOCALIDAD = NOMBRE) %>%
  mutate(URB = "U")

tmp2 <- loc_rur_lcc@data %>% 
  dplyr::select(CVEGEO, MUNICIPIO = NOM_MUN, LOCALIDAD = NOMBRE) %>%
  mutate(URB = "R")

dat_locnombre <- bind_rows(tmp1, tmp2)
dat_locnombre$MUNICIPIO <- stri_trans_general(dat_locnombre$MUNICIPIO, "latin-ascii") %>% tolower()
dat_locnombre$LOCALIDAD <- stri_trans_general(dat_locnombre$LOCALIDAD, "latin-ascii") %>% tolower()

# Fixing names for cases with obvious spelling/naming mismatch
dat_joined$MUNICIPIO <- tolower(dat_joined$MUNICIPIO)
dat_joined$LOCALIDAD <- tolower(dat_joined$LOCALIDAD)

cond <- (dat_joined$LOCALIDAD =="unidades de riego") & (dat_joined$MUNICIPIO == "ticul")
dat_joined$LOCALIDAD[cond] <- "ticul [unidad de riego]"
cond <- (dat_joined$LOCALIDAD =="el alamo") & (dat_joined$MUNICIPIO == "tizimin")
dat_joined$LOCALIDAD[cond] <- "alamo"
cond <- (dat_joined$LOCALIDAD =="x 'lapak") & (dat_joined$MUNICIPIO == "yaxcaba")
dat_joined$LOCALIDAD[cond] <- "xlapak"

# Check if there's a match of locality and municipality in dat_joined
dat_joined$hit_id <- 1:nrow(dat_joined)

dat_match <- dat_joined %>%
  left_join(dat_locnombre)
dat_unmatched <- dat_match[is.na(dat_match$CVEGEO),]

# 42 rows didn't match with locality and muncipality
# Will deal with them later...
# For each hit_id, check what's the minimum distance to those points
project_points <- function (long, lat) {
  xy <- data.frame(x = long, y = lat)
  coordinates(xy) <- ~ x + y
  proj4string(xy) <- CRS("+init=epsg:4326")
  xy <- spTransform(xy, CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  return(xy)
}

check_dist <- function (cvegeo, urb, long, lat) {
  long <- as.numeric(long)
  lat <- as.numeric(lat)
  if (is.na(cvegeo)) return(NA)
  if(urb == "U") {
    coords <- loc_urb_wgs[loc_urb_wgs$CVEGEO == cvegeo,]
    coords <-  coords@polygons[[1]]@Polygons[[1]]@coords
    cond <- in.out(coords, c(long, lat))
    if(cond) return(0)
    else {
      xy <- project_points(long, lat)
      coords <- project_points(coords[,1], coords[,2])
      return(gDistance(xy, coords))
    }
    return(ifelse(cond, 0, 99999))
  } else {
    coords <- loc_rur_lcc[loc_rur_lcc$CVEGEO == cvegeo,]
    xy <- data.frame(x = long, y = lat)
    coordinates(xy) <- ~x+y
    proj4string(xy) <- CRS("+init=epsg:4326")
    xy <- spTransform(xy, CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
    return(gDistance(xy, coords))
  }
}

min_dist_df <- data.frame(hit_id = 1:max(dat_match$hit_id),
                          min_dist = NA)
for (i in 1:max(dat_match$hit_id)) {
  dat_subset <- dat_match[dat_match$hit_id == i,] %>% as.data.frame()
  if (any(is.na(dat_subset$long))) {
    # print(i)
    next
  }
  
  min_dist <- apply(dat_subset, 1, 
                    function (x) check_dist(x['CVEGEO'], x['URB'], x['long'], x['lat'])) %>%
    min()
  
  min_dist_df$min_dist[i] <- min_dist
}

dat_joined <- dat_joined %>%
  left_join(min_dist_df, by = c("hit_id" = "hit_id"))

# Rule 1: Minimum distance to locality < 5000
dat_accept_r1 <- dat_joined %>%
  filter(min_dist < 5000)

# Rule 2: No duplicate
tmp <- dat_accept_r1 %>%
  group_by(long, lat) %>%
  tally() %>%
  filter(n == 1)
dat_accept_r2 <- tmp %>%
  left_join(dat_accept_r1) %>%
  arrange(ID_unique) %>%
  dplyr::select(-n)

# For each school, select one of the coordinates based on
# whether google says it's a school or not and coordinates type
uniq_id <- dat_accept_r2$ID_unique %>% unique
l <- length(uniq_id)
df1 <- data.frame(long = numeric(l), lat = numeric(l), ID_unique = numeric(l),
                 complete = character(l), school = character(l), 
                 coord_type = character(l), ID_original = numeric(l), 
                 MUNICIPIO = character(l), LOCALIDAD = character(l), 
                 REDUNDANCY = character(l), hit_id = numeric(l), 
                 min_dist = numeric(l), stringsAsFactors = F)
rank_school <- c("school", "unknown")
rank_type <- c("ROOFTOP", "RANGE_INTERPOLATED", "GEOMETRIC_CENTER", "APPROXIMATE")
for (i in 1:length(uniq_id)) {
  dat_subset <- dat_accept_r2[dat_accept_r2$ID_unique == uniq_id[i],]
  if (nrow(dat_subset) == 1) df1[i,] <- dat_subset
  else {
    score <- sapply(dat_subset$school, function (x) which(x == rank_school)) * 4
    score <- score + sapply(dat_subset$coord_type, function (x) which(x == rank_type))
    ind <- which.min(score)
    df1[i,] <- dat_subset[ind,]
  }
  
}

# Check if those rejected from first round due to rule 2 can make it
tmp <- dat_accept_r1 %>%
  group_by(long, lat) %>%
  tally() %>%
  filter(n > 1)

dat_reject_rnd1 <- tmp %>%
  left_join(dat_accept_r1) %>%
  arrange(ID_unique) %>%
  dplyr::select(-n)
sum(dat_reject_rnd1$ID_unique %in% uniq_id)

# 76 of those rejected already have their coordinates picked
# This means some of those rejected in round 1 due to rule 2 (duplication)
# might be freed up.
dat_rnd2 <- dat_reject_rnd1[!dat_reject_rnd1$ID_unique %in% uniq_id,]
tmp <- dat_rnd2 %>%
  group_by(long, lat) %>%
  tally() %>%
  filter(n == 1)
dat_accept_rnd2 <- tmp %>%
  left_join(dat_rnd2) %>%
  arrange(ID_unique) %>%
  dplyr::select(-n)

# 53 of them accepted into 2nd round because they are no longer duplicate
# (Because their counterpart got removed from the list)
uniq_id <- dat_accept_rnd2$ID_unique %>% unique
l <- length(uniq_id)
df2 <- data.frame(long = numeric(l), lat = numeric(l), ID_unique = numeric(l),
                  complete = character(l), school = character(l), 
                  coord_type = character(l), ID_original = numeric(l), 
                  MUNICIPIO = character(l), LOCALIDAD = character(l), 
                  REDUNDANCY = character(l), hit_id = numeric(l), 
                  min_dist = numeric(l), stringsAsFactors = F)
for (i in 1:length(uniq_id)) {
  dat_subset <- dat_accept_rnd2[dat_accept_rnd2$ID_unique == uniq_id[i],]
  if (nrow(dat_subset) == 1) df2[i,] <- dat_subset
  else {
    score <- sapply(dat_subset$school, function (x) which(x == rank_school)) * 4
    score <- score + sapply(dat_subset$coord_type, function (x) which(x == rank_type))
    ind <- which.min(score)
    df2[i,] <- dat_subset[ind,]
  }
  
}

dat_accept_overall <- bind_rows(df1, df2)

# Now check if the set that has no matching locality is worth keeping
# By checking if the coordinates is inside the municipal and near
# any of the known localities
dat_nomatch <- dat_joined %>%
  filter(is.na(min_dist) & !is.na(long))
dat_nomatch2 <- dat_nomatch %>% 
  left_join(dat_locnombre, by = c("MUNICIPIO" = "MUNICIPIO"))

uniq_hit_id <- unique(dat_nomatch2$hit_id)
min_dist_nomatch <- data.frame(hit_id = uniq_hit_id,
                               min_dist = NA)

for (i in 1:length(uniq_hit_id)) {
  dat_subset <- dat_nomatch2[dat_nomatch2$hit_id == uniq_hit_id[i],] %>% 
    as.data.frame()
  
  min_dist <- apply(dat_subset, 1, 
                    function (x) check_dist(x['CVEGEO'], x['URB'], x['long'], x['lat'])) %>%
    min()
  
  # Also check if inside municipal
  ind <- (mun_shp$NOMBRE %>% ascii_lower()) %in% dat_subset$MUNICIPIO %>%
    which()
  mun_coords <- mun_shp[ind,]@polygons[[1]]@Polygons[[1]]@coords
  dat_coords <- dat_subset[,c('long', 'lat')] %>%
    unique %>% as.matrix()
  in_mun <- in.out(mun_coords, dat_coords)
  
  min_dist_nomatch$min_dist[i] <- ifelse(in_mun, min_dist, NA)
}

ind <- which(!is.na(min_dist_nomatch$min_dist))
dat_accept_nomatch <- dat_nomatch[ind,] # Only one is worth it!
dat_accept_nomatch$min_dist <- min_dist_nomatch$min_dist[ind]
dat_accept_overall <- bind_rows(dat_accept_overall, dat_accept_nomatch)

reject_ID <- which(!dat_unique$ID_unique %in% dat_accept_overall$ID_unique)
dat_need_impute <- dat_unique[reject_ID,]
dat_need_impute$MUNICIPIO <- tolower(as.character(dat_need_impute$MUNICIPIO))
dat_need_impute$LOCALIDAD <- tolower(as.character(dat_need_impute$LOCALIDAD))
dat_need_impute <- dat_need_impute %>%
  left_join(dat_locnombre)

dat_accept_overall2 <- dat_accept_overall %>%
  select(ID_unique, long, lat)
dir.create("../derived_data/sch_dat/")
write.csv(dat_accept_overall2, "../derived_data/sch_dat/dat_google_accepted.csv", quote = F, row.names = F)
write_csv(dat_need_impute, "../derived_data/sch_dat/dat_need_impute.csv")