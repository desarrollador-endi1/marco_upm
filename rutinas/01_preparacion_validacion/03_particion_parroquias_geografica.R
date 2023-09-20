#
rm(list = ls())
library(tidyverse)
library(sf)
library(foreach)
library(doParallel)

# numCores <- detectCores()
# numCores
registerDoParallel(2)

zona <- read_sf("insumos/01_preparacion_validacion/nogit/BNCPV22.gpkg", layer = "zon_a")

sector <- read_sf("insumos/01_preparacion_validacion/nogit/BNCPV22.gpkg", layer = "sec_a")

manzana <- read_sf("insumos/01_preparacion_validacion/nogit/BNCPV22.gpkg", layer = "man_a")
#Sectores cuya variable parroquia no coincide con el id_sector

contro_sector_parroquia <- sector %>% 
  filter(parroquia != substr(sec, 1, 6))

#Perfil de parroquias

parroquia <- sector %>%
  mutate(id_parroquia = substr(sec, 1, 6)) %>% 
  group_by(id_parroquia) %>% 
  summarise()

#Perfil de parroquias por amanzando disperso (ad)
perfil <- sector %>%
  mutate(id_parroquia = substr(sec, 1, 6),
         ad = ifelse(substr(sec, 7, 9) == "999", "dis", "aman")) %>% 
  group_by(id_parroquia, ad) %>% 
  summarise()

#Lectura de rios
rios <- read_sf("insumos/01_preparacion_validacion/nogit/rios.gpkg", layer = "rios")

#Creamos un vector con los identificador de parroquia

index <- unique(parroquia$id_parroquia)

foreach(i=1:length(index),
        .packages = c("tidyverse", "sf")) %dopar% {
          
  dir.create(paste0("intermedios/01_preparacion_validacion/",substr(index[i], 1, 2)),
             showWarnings = F)
  dir.create(paste0("intermedios/01_preparacion_validacion/",substr(index[i], 1, 2), "/", index[i]),
             showWarnings = F)
  
  manzana_01 <- manzana %>%
    mutate(id_parroquia = substr(man, 1, 6)) %>% 
    filter(id_parroquia == index[i]) %>% 
    select(man, id_parroquia)
  
  sector_01 <- sector %>%
    mutate(id_parroquia = substr(sec, 1, 6)) %>%
    filter(id_parroquia == index[i]) %>%
    select(sec, id_parroquia)
  
  zona_01 <- zona %>% 
    filter(substr(zon, 1, 6) == index[i]) %>% 
    select(zon)
  
  perfil_01 <- perfil %>%
    filter(id_parroquia == index[i])
  
  rios_01 <- st_intersection(rios, perfil_01) 
  
  if(dim(manzana_01)[1]>0){
    write_sf(manzana_01, paste0("intermedios/01_preparacion_validacion/", 
                                substr(index[i], 1, 2), "/", index[i] ,"/manzana.gpkg"))
  }
  
  write_sf(zona_01, paste0("intermedios/01_preparacion_validacion/",
                             substr(index[i], 1, 2), "/", index[i] ,"/zona.gpkg"))
  
  write_sf(sector_01, paste0("intermedios/01_preparacion_validacion/",
                            substr(index[i], 1, 2), "/", index[i] ,"/sector.gpkg"))
  
  if(dim(rios_01)[1]>0){
    rios_02 <- rios_01 %>% 
      summarise()
    
    write_sf(rios_02, paste0("intermedios/01_preparacion_validacion/",
                             substr(index[i], 1, 2), "/", index[i] ,"/rios.gpkg"))
  }
  
  write_sf(perfil_01, paste0("intermedios/01_preparacion_validacion/",
                             substr(index[i], 1, 2), "/", index[i] ,"/perfil.gpkg"))
}


