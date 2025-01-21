#
rm(list = ls())
# Cargamos las librerías a usar
library(tidyverse)
library(sf)

# Definimos el límite inferior del tamaño de los conglomerados
li = 60

# Se abre la división de manzanas en función del li
particion <- readRDS(paste0("intermedios/02_conglomeracion/particion_manzanas_li_", li, ".rds"))
manzanas_excluir <- unique(particion$mansec)

load("intermedios/lista_parroquias.RData")

index1 <- index

am <- vector("list", 0)
di <- vector("list", 0)

for(i in 1:length(index)){
  # manzanas extendidas
  if(file.exists(paste0("productos/01_preparacion_validacion/", 
                        substr(index[i], 1, 2), "/", index[i],
                        "/manzanas_extendidas.gpkg"))){
    am[[i]] <- read_sf(paste0("productos/01_preparacion_validacion/",
                             substr(index[i], 1, 2), "/", index[i],
                             "/manzanas_extendidas.gpkg"))
  }
  # sectores dispersos
  if(file.exists(paste0("intermedios/01_preparacion_validacion/", 
                        substr(index[i], 1, 2), "/", index[i],
                        "/sector.gpkg"))){
    di[[i]] <- read_sf(paste0("intermedios/01_preparacion_validacion/",
                              substr(index[i], 1, 2), "/", index[i],
                              "/sector.gpkg"))
  }
  print(i)
}

am <- do.call("rbind", am)
di <- do.call("rbind", di)

am_01 <- am |> 
  st_collection_extract("POLYGON")
  

write_sf(am_01, "intermedios/05_limites_upm/manzanas_consolidado.gpkg")
write_sf(di, "intermedios/05_limites_upm/sectores_consolidado.gpkg")
