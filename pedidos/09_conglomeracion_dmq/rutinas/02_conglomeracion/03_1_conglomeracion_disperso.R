rm(list = ls())
# Cargamos las librerías a usar
library(tidyverse)
library(sf)
# Cargamos las funciones a utilizar
source("rutinas/funciones/matinc.R")
source("rutinas/funciones/conglomera2.R")
# Definimos el límite inferior del tamaño de los conglomerados
li = 60
# Cargamos la base con el número de viviendas por edificio
peso_edif <- readRDS("intermedios/01_preparacion_validacion/precenso_edificios.rds")
# Calculamos el número de viviendas por man_sec
pesos <- peso_edif %>% 
  group_by(man_sec = ifelse(substr(id_edif, 7, 9) == "999", 
                            substr(id_edif, 1, 12),
                            substr(id_edif, 1, 15))) %>% 
  summarise(viv = sum(viv_ocu)) %>% 
  ungroup()

# Definimos el largo en metros mínimo para contar con incidencia entre manzanas
largo_sec <- 10

index <- list.dirs("pedidos/09_conglomeracion_dmq/intermedios/01_preparacion_validacion/", 
                   full.names = F, recursive = F)

for(i in 1 : length(index)){
  if(file.exists(paste0("pedidos/09_conglomeracion_dmq/intermedios/01_preparacion_validacion/",
                        index[i], "/sectores.gpkg"))){
    sectores <- read_sf(paste0("pedidos/09_conglomeracion_dmq/intermedios/01_preparacion_validacion/",
                               index[i], "/sectores.gpkg"))  %>% 
      filter(substr(sec, 7, 9) == "999")
    if(dim(sectores)[1] > 0){
      # Calculamos la matriz de incidencia de las manzanas de la parroquia
      matsec <- matinc(sectores, largo_sec, "sec")
      # Generamos la matriz de pesos de las manzanas 
      pessec <- cbind("id_sec" = row.names(matsec)) %>% 
        as.data.frame() %>% 
        left_join(pesos, by = c("id_sec" = "man_sec")) %>% 
        mutate(viv = ifelse(is.na(viv), 0, viv))
      # Aplicamos el algoritmo de conglomeración
      h <- conglomera2(matsec, peso = pessec, sl = li, id = "id_sec") %>% 
        mutate(congf = str_pad(congf, 6, "left", "0")) 
      # Generamos el shape de conglomerados
      apoyo <- sectores %>% 
        left_join(h, by = c("sec" = "id_sec")) %>% 
        mutate(parroquia = substr(sec, 1, 6)) %>% 
        group_by(parroquia, congf) %>% 
        summarise(viv = sum(viv))
      
      write_sf(apoyo, paste0("pedidos/09_conglomeracion_dmq/productos/02_conglomeracion/",
                             index[i], "/conglomerados_disperso_", li, ".gpkg"))
      
      saveRDS(h, paste0("pedidos/09_conglomeracion_dmq/productos/02_conglomeracion/",
                        index[i], "/man_sec_conglomerados_disperso_", li, ".rds"))
    }
    
  }
}
