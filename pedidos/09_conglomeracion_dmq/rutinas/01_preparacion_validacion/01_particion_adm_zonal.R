rm(list = ls())

library(tidyverse)
library(openxlsx)
library(sf)

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
# 
# manzanas_az <- read.xlsx("pedidos/09_conglomeracion_dmq/insumos/Correspondencia_Adm_Zonales.xlsx", sheet = 2)
# 
# control <- manzanas_az %>% 
#   group_by(sec, ad_zonal_man = ad_zonal) %>% 
#   summarise() %>% 
#   full_join(sector_az, by = "sec") %>% 
#   filter(substr(sec, 7, 9) != "999")
# 
# sum(control$ad_zonal==control$ad_zonal)

# Abrimos todos los archivos de manzanas extendidas del canton 1701

load("intermedios/lista_parroquias.RData")

index = index[substr(index, 1, 4) == "1701"]
manzanas <- vector("list", 0)
sectores <- vector("list", 0)
for (i in 1: length(index)){
  manzanas[[i]]  <- read_sf(paste0("productos/01_preparacion_validacion/", 
                                   substr(index[i], 1, 2), "/", index[i],
                                   "/manzanas_extendidas.gpkg"))
  
  sectores[[i]] <- read_sf(paste0("intermedios/01_preparacion_validacion/", 
                             substr(index[i], 1, 2), "/" , index[i],
                             "/sector.gpkg")) %>% 
    filter(substr(sec, 7, 9) == "999")
  
  print(i)
  
}

manzanas_dmq <- do.call("rbind", manzanas) %>% 
  mutate(sec = substr(man, 1, 12)) %>% 
  left_join(sector_az, by = "sec")

sectores_dmq <- do.call("rbind", sectores)  %>% 
  left_join(sector_az, by = "sec")


control <- manzanas_dmq %>% 
  as.data.frame() %>% 
  select(mansec = man, cod_adz) %>% 
  rbind(sectores_dmq %>% 
          as.data.frame() %>% 
          select(mansec = sec, cod_adz)) %>% 
  mutate(parroq = substr(mansec, 1, 6)) %>% 
  group_by(parroq, cod_adz) %>% 
  summarise() %>% 
  group_by(parroq) %>% 
  mutate(n = n()) %>% 
  filter(n > 1)

for(i in 1:(dim(control)[1])){
  
  apoyo_man <- manzanas_dmq %>% 
    filter(substr(man, 1, 6) == control$parroq[i] & cod_adz == control$cod_adz[i])
  
  apoyo_sec <- sectores_dmq %>% 
    filter(substr(sec, 1, 6) == control$parroq[i] & cod_adz == control$cod_adz[i])
  
  dir.create(paste0("pedidos/09_conglomeracion_dmq/intermedios/01_preparacion_validacion/", 
                    control$parroq[i], "_", control$cod_adz[i]), showWarnings = F)
  if(dim(apoyo_man)[1] > 0){
    write_sf(apoyo_man, paste0("pedidos/09_conglomeracion_dmq/intermedios/01_preparacion_validacion/", 
                               control$parroq[i], "_", control$cod_adz[i], "/manzanas_extendidas.gpkg"))
  }
  if(dim(apoyo_sec)[1] > 0){
    write_sf(apoyo_sec, paste0("pedidos/09_conglomeracion_dmq/intermedios/01_preparacion_validacion/", 
                               control$parroq[i], "_", control$cod_adz[i], "/sectores.gpkg"))
  }
}




