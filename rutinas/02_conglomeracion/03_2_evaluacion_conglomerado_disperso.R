rm(list = ls())
# Cargamos las librerías a usar
library(tidyverse)
library(sf)
library(foreach)
library(doParallel)
# Cargamos las funciones a utilizar
source("rutinas/funciones/matinc.R")
source("rutinas/funciones/conglomerar.R")
source("Rutinas/Funciones/serpenteante.r")

# Definimos el límite inferior del tamaño de los conglomerados
li = 60
# Cargamos la base con el número de viviendas por edificio
peso_edif <- readRDS("intermedios/01_preparacion_validacion/precenso_edificios.rds")
# Calculamos el número de viviendas por man_sec
pesos <- peso_edif %>% 
  group_by(mansec) %>% 
  summarise(viv = sum(viv_ocu)) %>% 
  ungroup()

load("intermedios/lista_parroquias.RData")

index1 <- index

h <- vector("list", 0)
zona_01 <- vector("list", 0)

for(i in 1:length(index)){
  
  sc <- read_sf(paste0("intermedios/01_preparacion_validacion/", 
                       substr(index[i], 1, 2), "/", index[i],
                       "/sector.gpkg")) %>% 
    filter(substr(sec, 7, 9) == "999") %>% 
    rename(id_sec = sec)
  
  if(dim(sc)[1] != 0){
    h[[i]] <- readRDS(paste0("productos/02_conglomeracion/",
                             substr(index[i], 1, 2), "/", index[i],
                             "/man_sec_conglomerados_disperso_", li, ".rds"))
    
    # numeración de sectores dispersos
    intermedio <- sc %>% 
      left_join(h[[i]], by = "id_sec") %>% 
      group_by(id_parroquia, congf) %>% 
      summarise(viv = sum(viv)) %>% 
      ungroup()
    
    zona_01[[i]] <- serpenteante(intermedio, idp = "congf") %>% 
      mutate(orden = str_pad(orden,4,"left","0"),
             id_con = paste0(id_parroquia, orden)) %>%
      select(congf, id_con, viv) 
  }
}
H <- do.call("rbind", h) %>% 
  mutate(id_con_old = paste0(substr(id_sec, 1, 6), congf))
Z <- do.call("rbind", zona_01) %>% 
  mutate(id_con_old = paste0(substr(id_con, 1, 6), congf)) %>% 
  select(-viv)
# Calculamos el número de manzanas perdidas en la conglomeración
sum(!pesos$mansec[substr(pesos$mansec, 7, 9) == "999"] %in% H$id_sec)

sum(substr(H$congf, 1, 1) == "9")

H1 <- H %>% 
  mutate(id_con = paste0(substr(id_sec, 1, 6), congf)) %>% 
  group_by(id_con) %>% 
  summarise(n_sec = n(),
            viv = sum(viv)) %>% 
  filter(viv < 20)

cd <- H %>% 
  full_join(Z, by = "id_con_old") %>%
  mutate(id_con9 = paste0(substr(id_con, 1, 6), "9", substr(id_con, 8, 10))) %>% 
  select(id_sec, id_con = id_con9, viv)

#n_distinct(cd$id_con9)

saveRDS(cd, "intermedios/02_conglomeracion/sectores_conglomerados_60.rds")

write_sf(Z, "conglomerados_dispersos.gpkg")
