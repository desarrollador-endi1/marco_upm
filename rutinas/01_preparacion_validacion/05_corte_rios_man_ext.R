#
rm(list = ls())
library(tidyverse)
library(sf)
library(foreach)
library(doParallel)

# numCores <- detectCores()
# numCores
registerDoParallel(2)

rios <- read_sf("insumos/01_preparacion_validacion/rios_a.gpkg") %>% 
  st_buffer(0) %>% 
  st_make_valid()

provincia <- list.dirs("productos/01_preparacion_validacion", 
                       full.names = F, 
                       recursive = F)

for(i in 1:length(provincia)){
  
  parroquia <- list.dirs(paste0("productos/01_preparacion_validacion/", 
                                provincia[i]), 
                         full.names = F,
                         recursive = F)
  
  foreach(j=1:length(parroquia),
          .packages = c("tidyverse", "sf")) %dopar% {
    if(file.exists(paste0("productos/01_preparacion_validacion/", provincia[i], "/", 
                          parroquia[j], "/manzanas_extendidas.gpkg"))){
      manzana_ext <- read_sf(paste0("productos/01_preparacion_validacion/", provincia[i], "/", 
                                    parroquia[j], "/manzanas_extendidas.gpkg"))
      
      manzana_ori <- read_sf(paste0("intermedios/01_preparacion_validacion/", provincia[i], "/", 
                                    parroquia[j] ,"/manzana.gpkg")) %>% 
        select(man, id_parroquia)
      
      perfil <- read_sf(paste0("intermedios/01_preparacion_validacion/", provincia[i], "/", 
                               parroquia[j] ,"/perfil.gpkg")) %>% 
        filter(ad == "aman") %>% 
        st_buffer(0) %>% 
        st_make_valid()
      
      rios_01 <- st_intersection(rios, perfil) 
      
      if(dim(rios_01)[1]>0){
        rios_02 <- rios_01 %>% 
          summarise()
        
        manzana <- manzana_ext %>% 
          st_difference(rios_02) %>% 
          st_buffer(0) %>% 
          #Validación de geometría
          st_make_valid() %>% 
          #Trasnformación a multipoligono
          st_cast("MULTIPOLYGON") %>% 
          #Transformación a poligono
          st_cast("POLYGON") %>% 
          #Emparejamiento geográfico para identicar pedazos de manzanas a utilizar
          st_join(manzana_ori) %>% 
          filter(man.x == man.y) %>% 
          #Disolver a nivel de manzana
          group_by(man = man.x) %>% 
          summarise() %>% 
          st_make_valid() %>% 
          st_cast("MULTIPOLYGON")
      }else{
        manzana <- manzana_ext
      }
      
      
      write_sf(manzana, paste0("productos/01_preparacion_validacion/", provincia[i], "/", 
                               parroquia[j], "/manzanas_extendidas_linderos.gpkg"))
      #print(parroquia[j])
      
    }
    
  }
  
  
  
}

# for (i in 1:24){
#   parroquia <- list.dirs(paste0("productos/01_preparacion_validacion/", 
#                                 provincia[i]), 
#                          full.names = F,
#                          recursive = F)
#   for(j in 1:length(parroquia)){
#   if(file.exists(paste0("productos/01_preparacion_validacion/", provincia[i], "/", 
#                         parroquia[j], "/manzanas_extendidas_linderos.gpkg"))){
#     unlink(paste0("productos/01_preparacion_validacion/", provincia[i], "/", 
#                   parroquia[j], "/manzanas_extendidas_linderos.gpkg"))}
#   }
# }
