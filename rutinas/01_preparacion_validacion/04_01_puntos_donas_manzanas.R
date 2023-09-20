#
rm(list = ls())
#
library(tidyverse)
library(sf)

source("rutinas/funciones/extpol/extpol_2.R") #Extiente polígonos disjuntos

index_pro <- list.dirs("intermedios/01_preparacion_validacion/", full.names = F, recursive = F)
index_pro <- index_pro[index_pro %in% c("17", "09")]


# primer for de provincia
for(pro in index_pro){
  
  # for de parroquias
  
  index_par <- sort(list.dirs(paste0("intermedios/01_preparacion_validacion/", pro), full.names = F, recursive = F), decreasing = T)
  
  for(par in index_par){
    
    print(paste0(pro, " - ", par))
    
    manzana <- read_sf(paste0("intermedios/01_preparacion_validacion/", pro, "/", par ,"/manzana.gpkg")) %>% 
      select(man, id_parroquia)
    
    perfil <- read_sf(paste0("intermedios/01_preparacion_validacion/", pro, "/", par ,"/perfil.gpkg")) %>% 
      filter(ad == "aman")
    
    #lindero <- read_sf("insumos/rios.gpkg")
    
    # Extendemos el shape de polìgonos
    polext <- extpol(manzana , perfil, id = "man", buf = 2, densidad = 0.5) 
    
    puntos_donas <- polext[[1]]
    
    man_ext <- polext[[2]]
    
    dir.create(paste0("productos/01_preparacion_validacion/", pro))
    
    dir.create(paste0("productos/01_preparacion_validacion/", pro, "/", par))
    
    write_sf(puntos_donas, paste0("intermedios/01_preparacion_validacion/", pro, "/", par ,"/puntos_donas.gpkg"))
    
    write_sf(man_ext, paste0("productos/01_preparacion_validacion/", pro, "/", par ,"/manzanas_extendidas.gpkg"))
    
    rm(manzana, perfil, polext, puntos_donas, man_ext)
  }

}

