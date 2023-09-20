#
rm(list = ls())
#
library(tidyverse)
library(sf)

source("rutinas/funciones/extpol/extpol_2.R") #Extiente polígonos disjuntos

index_pro <- c("17", "09")
index_par <- c("170150", "090150")

# primer for de provincia
for(par in index_par){
  
  manzana <- read_sf(paste0("intermedios/01_preparacion_validacion/", substr(par, 1, 2), "/", par ,"/manzana.gpkg")) %>% 
    select(man, id_parroquia)
  
  zona <- read_sf(paste0("intermedios/01_preparacion_validacion/", substr(par, 1, 2), "/", par ,"/zona.gpkg")) %>% 
    filter(substr(zon, 7, 9) != "999")
  
  index_zon <- unique(zona$zon)
  
  man_ext <- vector("list", 0)
  
  i = 1
  
  for(z in index_zon){
    perfil <- zona %>% 
      filter(zon == z)
    
    polext <- extpol(manzana %>% filter(substr(man, 1, 9) == z) , 
                     perfil, id = "man", buf = 2, densidad = 0.5) 
    
    man_ext[[z]] <- polext[[2]]
    
    print(paste0(z, " zona número: ", i))
    
    i = i+1
  }
  
  save(man_ext, 
       file = paste0("intermedios/01_preparacion_validacion/", substr(par, 1, 2), "/", par ,"/manzanas_extendidas.RData"))
  
  man_ext <- do.call(rbind, man_ext)
  
  dir.create(paste0("productos/01_preparacion_validacion/", substr(par, 1, 2), "/", par))
  
  write_sf(man_ext, paste0("productos/01_preparacion_validacion/", substr(par, 1, 2), "/", par ,"/manzanas_extendidas.gpkg"))
  
  rm(manzana, perfil, polext, puntos_donas, man_ext)

}
  
