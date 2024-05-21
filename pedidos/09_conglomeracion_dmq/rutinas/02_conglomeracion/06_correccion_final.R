rm(list = ls())

library(tidyverse)
library(sf)
library(openxlsx)

li <- 60

manzanas_sectores_upm <- readRDS("pedidos/09_conglomeracion_dmq/productos/02_conglomeracion/manzanas_sectores_upm.rds")

load("pedidos/09_conglomeracion_dmq/intermedios/02_conglomeracion/revisar.RData")

index <- substr(revisar$id_upm, 1, 6)

#index <- unique(substr(revisar_01$id_upm, 1, 6))

#####

i = i+1

# sectores <- read_sf(paste0("intermedios/01_preparacion_validacion/",
#                            substr(index[i], 1, 2), "/", index[i],
#                            "/sector.gpkg")) %>% 
#   filter(substr(sec, 7, 9) == "999")

manzanas <- read_sf(paste0("pedidos/09_conglomeracion_dmq/intermedios/01_preparacion_validacion/",
                           substr(revisar$id_upm[i], 1, 6), "_1701", revisar$cod_adz[i],
                           "/manzanas_extendidas.gpkg"))

sector_az <- read.xlsx("pedidos/09_conglomeracion_dmq/insumos/Correspondencia_Adm_Zonales.xlsx", sheet = 1) %>% 
  mutate(cod_adz = case_when(ad_zonal == "CALDERON" ~ "1701z1",
                             ad_zonal == "CHOCO ANDINO" ~ "1701z2",
                             ad_zonal == "ELOY ALFARO" ~ "1701z3",
                             ad_zonal == "EUGENIO ESPEJO" ~ "1701z4",
                             ad_zonal == "LA DELICIA" ~ "1701z5",
                             ad_zonal == "LOS CHILLOS" ~ "1701z6",
                             ad_zonal == "MANUELA SAENZ" ~ "1701z7",
                             ad_zonal == "QUITUMBE" ~ "1701z8",
                             ad_zonal == "TUMBACO" ~ "1701z9",
                             T ~ "noesquito")) %>% 
  select(sec, cod_adz)

manzanas_se <- read_sf(paste0("intermedios/01_preparacion_validacion/",
                           substr(index[i], 1, 2), "/", index[i],
                           "/manzana.gpkg")) %>%
  mutate(sec = substr(man, 1, 12)) %>% 
  left_join(sector_az, by = "sec") %>% 
  filter(cod_adz == paste0("1701", revisar$cod_adz[i]))
  


lala <- manzanas %>%
  left_join(manzanas_sectores_upm %>%
            select(man = man_sec, id_upm = paste0("id_upm_", li), viv), by = "man") %>% 
  group_by(id_upm) %>%
  summarise(viv = sum(viv))



# lala <- sectores %>% 
#   left_join(manzanas_sectores_upm %>%
#               select(sec = man_sec, id_upm = paste0("id_upm_", li)), by = "sec") %>%
#   filter(id_upm %in% revisar$id_upm) %>% 
#   group_by(id_upm) %>%
#   summarise()

dim(lala)[1]

write_sf(lala, "upm.gpkg")
write_sf(manzanas, "manzanas_ext.gpkg")
#write_sf(sectores, "manzanas.gpkg")

revisar$id_upm[i]



#####

revisar_01 <- revisar %>% 
  mutate(obs = case_when(id_upm == "1701500258" ~ "ahí queda",
                         id_upm == "1701500501" ~ "se cambia de manzana",
                         T ~ "no se revisó"))

manzanas_sectores_upm_01 <- manzanas_sectores_upm %>% 
  mutate(id_upm_60 = case_when(man_sec == "170150050004003" ~ "1701500514",
                               T ~ id_upm_60))

apoyo <- manzanas_sectores_upm_01 %>% 
  group_by(id_upm_60, cod_adz) %>% 
  summarise() %>% 
  mutate(parroquia = substr(id_upm_60, 1, 6)) %>% 
  arrange(parroquia, cod_adz, id_upm_60) %>% 
  group_by(parroquia) %>% 
  mutate(cong = row_number()) %>% 
  ungroup() %>% 
  mutate(id_upm = case_when(substr(id_upm_60, 7, 7) == "9" ~  paste0(substr(id_upm_60, 1, 6), "9", str_pad(cong, 3, "left", "0")),
                            T ~ paste0(substr(id_upm_60, 1, 6), str_pad(cong, 4, "left", "0")))) %>% 
  select(id_upm_60, cod_adz, id_upm)


# Renumeración final de manera general 
manzanas_sectores_upm_02 <- manzanas_sectores_upm_01 %>% 
  select(man_sec, id_upm_60, viv, cod_adz) %>% 
  left_join(apoyo, by = c("id_upm_60", "cod_adz")) %>% 
  select(man_sec, id_upm, viv)
  
saveRDS(manzanas_sectores_upm_02,
        "pedidos/09_conglomeracion_dmq/productos/02_conglomeracion/manzanas_sectores_upm_final.rds")


resumen_upm <- manzanas_sectores_upm_01 %>% 
  mutate(id_zon = case_when(substr(man_sec, 7, 7) == "9" ~ substr(man_sec, 1, 9),
                            T ~ paste0(substr(man_sec, 1, 6), "000"))) %>% 
  group_by(id_zon) %>% 
  mutate(n_man_sec_zon = n()) %>% 
  ungroup() %>% 
  group_by(id_upm = id_upm_60, id_zon, n_man_sec_zon, cod_adz) %>% 
  summarise(viv = sum(viv),
            n_man_upm = n()) %>% 
  ungroup() %>% 
  group_by(id_upm, cod_adz) %>% 
  mutate(n_partes_upm = n()) 

lol <- manzanas_sectores_upm_01 %>% 
  group_by(id_upm = id_upm_60, cod_adz) %>% 
  summarise(viv = sum(viv))
