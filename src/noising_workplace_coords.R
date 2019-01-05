## Adding noise to duplicated workplace coordinates
library(tidyverse)
library(raster)

dat <- read.csv("../raw_data/denue_workplace/denue_inegi_31_.csv",
                encoding = "UTF-8", stringsAsFactors = F)

dat1 <- dat %>%
  dplyr::select(CVE_MUN = cve_mun, CVE_LOC = cve_loc, CVE_AGEB = ageb, lat = latitud, long = longitud) %>%
  mutate(CVE_MUN = str_pad(CVE_MUN, 3, side="left", "0"),
         CVE_LOC = str_pad(CVE_LOC, 4, side="left", "0"),
         ID = 1:nrow(dat))

#### Find shortest distance between bloc of people based on Merida
dd <- dat1 %>%
  filter(CVE_MUN == "102") %>%
  dplyr::select(long) %>%
  dist()
maxd_long <- quantile(dd, 0.05)
dd <- dat1 %>%
  filter(CVE_MUN == "102") %>%
  dplyr::select(lat) %>%
  dist()
maxd_lat <- quantile(dd, 0.05)

#### Get duplicated entries
dupe_ll <- dat1 %>%
  group_by(lat, long) %>%
  tally() %>%
  filter(n > 1)
dat_dupe <- dat1 %>%
  inner_join(dupe_ll)
dat_nodupe <- dat1 %>%
  filter(!ID %in% dat_dupe$ID)

dat_dupe$lat <- dat_dupe$lat + runif(nrow(dat_dupe), -maxd_lat, maxd_lat)
dat_dupe$long <- dat_dupe$long + runif(nrow(dat_dupe), -maxd_long, maxd_long)

dat_noised <- dat_dupe %>%
  dplyr::select(-n) %>%
  bind_rows(dat_nodupe)

## Confirm that there's no more duplicates
dat_noised %>%
  group_by(long, lat) %>%
  tally() %>%
  filter(n > 1)

write_csv(dat_noised, "../derived_data/workplace_coords_no_duplicate.csv")
