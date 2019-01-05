rm(list = ls())

library(tidyverse)

dat <- read.csv("../raw_data/SEGEY_school_data_2017/catalogo_escuelas.csv",
                col.names = c("ID", "CCT", "NOMBRE", "TURNO", "NIVEL", "MATRICULA",
                              "MUNICIPIO", "LOCALIDAD", "CALLE_Y_NUMERO", "COLONIA", "REGION"))

dat_uniq <- dat %>%
  group_by(NOMBRE, MUNICIPIO, LOCALIDAD, CALLE_Y_NUMERO, COLONIA, REGION) %>%
  summarise(MATRICULA = sum(MATRICULA),
            ID = min(ID)) %>%
  arrange(ID)

write_csv(dat_uniq, "../raw_data/SEGEY_school_data_2017/catalogo_escuelas-uniq.csv")