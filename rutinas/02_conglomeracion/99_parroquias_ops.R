#
rm(list=ls())
#
library(openxlsx)
library(sf)
library(tidyverse)
#
# Cargamos las funciones a utilizar
source("rutinas/funciones/matinc.R")
source("rutinas/funciones/conglomerar.R")
#
ops <- read.xlsx("D:/AG/ENDI/provisional_pedidos/s082/muestra_ops_v2.2.xlsx",
                 sheet = "Correspondencia UPM man_sec") 

index <- unique(substr(ops$id_upm, 1, 6))
index <- index[order(index)]
index <- index[!index %in% c("170150", "090150")]

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
largo_man <- 10

#inicializamos la lista
r <- vector("list", 0)

# Iniciamos el laso for que recorre las parroquias
for (j in 1 : length(index)){
  # Cargamos las manzanas de cada parroquia
  manzanas <- read_sf(paste0("productos/01_preparacion_validacion/", 
                             substr(index[j], 1, 2), "/" , index[j],
                             "/manzanas_extendidas.gpkg"))
  
  # Calculamos la matriz de incidencia de las manzanas de la parroquia
  matman <- matinc(manzanas, largo_man, "man")
  
  # Generamos la matriz de pesos de las manzanas 
  pesman <- cbind("id_man" = row.names(matman)) %>% 
    as.data.frame() %>% 
    left_join(pesos, by = c("id_man" = "man_sec")) %>% 
    mutate(viv = ifelse(is.na(viv), 0, viv))
  
  # Aplicamos el algoritmo de conglomeración
  h <- conglomerar(matman, peso = pesman, sl = li, id = "id_man") %>% 
    mutate(congf = str_pad(congf, 6, "left", "0"))
  
  # Generamos el shape de conglomerados
  apoyo <- manzanas %>% 
    left_join(h, by = c("man" = "id_man")) %>% 
    mutate(parroquia = substr(man, 1, 6))
  # group_by(parroquia, congf) %>% 
  # summarise(viv = sum(viv))
  
  r[[j]] <- apoyo
  
  cat("Se realizó la conglomeración de la parroquia", index[j])
}

r1 <- do.call("rbind", r)

for (j in 1 : length(index)){
  # Cargamos las manzanas de cada parroquia
  manzanas <- read_sf(paste0("intermedios/01_preparacion_validacion/", 
                             substr(index[j], 1, 2), "/" , index[j],
                             "/sector.gpkg")) %>% 
    rename(man = sec)
  
  # Calculamos la matriz de incidencia de las manzanas de la parroquia
  matman <- matinc(manzanas, largo_man, "man")
  
  # Generamos la matriz de pesos de las manzanas 
  pesman <- cbind("id_man" = row.names(matman)) %>% 
    as.data.frame() %>% 
    left_join(pesos, by = c("id_man" = "man_sec")) %>% 
    mutate(viv = ifelse(is.na(viv), 0, viv))
  
  # Aplicamos el algoritmo de conglomeración
  h <- conglomerar(matman, peso = pesman, sl = li, id = "id_man") %>% 
    mutate(congf = str_pad(congf, 6, "left", "0"))
  
  # Generamos el shape de conglomerados
  apoyo <- manzanas %>% 
    left_join(h, by = c("man" = "id_man")) %>% 
    mutate(parroquia = substr(man, 1, 6))
  # group_by(parroquia, congf) %>% 
  # summarise(viv = sum(viv))
  
  r[[j]] <- apoyo
  
  cat("Se realizó la conglomeración de la parroquia", index[j])
}

r2 <- do.call("rbind", r)

save(r1, r1, file = "listas.RData")

