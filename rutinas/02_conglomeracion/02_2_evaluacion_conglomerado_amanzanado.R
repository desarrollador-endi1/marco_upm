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
li = 80
# Cargamos la base con el número de viviendas por edificio
peso_edif <- readRDS("intermedios/01_preparacion_validacion/precenso_edificios.rds")
# Calculamos el número de viviendas por man_sec
pesos <- peso_edif %>% 
  group_by(mansec) %>% 
  summarise(viv = sum(viv_ocu)) %>% 
  ungroup()
# Se abre la división de manzanas en función del li
particion <- readRDS(paste0("intermedios/02_conglomeracion/particion_manzanas_li_", li, ".rds"))
manzanas_excluir <- unique(particion$mansec)

load("intermedios/lista_parroquias.RData")

index1 <- index

h <- vector("list", 0)

for(i in 1:length(index)){
  if(file.exists(paste0("productos/01_preparacion_validacion/", 
                        substr(index[i], 1, 2), "/", index[i],
                        "/manzanas_extendidas.gpkg"))){
    h[[i]] <- readRDS(paste0("productos/02_conglomeracion/",
                             substr(index[i], 1, 2), "/", index[i],
                             "/man_sec_conglomerados_", li, ".rds"))
  }
}
H <- do.call("rbind", h)
# Calculamos el número de manzanas perdidas en la conglomeración
sum(!pesos$mansec[substr(pesos$mansec, 7, 9) != "999"] %in% H$mansec) - n_distinct(particion$mansec)
# Control manzanas partidas manzanas conglomeradas
sum(H$mansec %in% unique(particion$mansec))

# creamos el identificador de conglomerado (idcon) 6 digitos de parroquia, 9 digitos de "congolomerado"
# para manzanas partidas y perdidas 9 digitos de identificador de manzana y conglomerado "000" y 
# 6 digitos de congf, 1 de particion, conglomeracion y perdidas (c conglomeracion, l perdidas, p partidas)

auxiliar <- H %>% 
  mutate(idcon = paste0(substr(mansec, 1, 6), "000", congf, "c")) %>% 
  select(mansec, idcon, viv) %>% 
  rbind(pesos %>% 
          filter(mansec %in% manzanas_excluir) %>% 
          mutate(idcon = paste0(mansec, "p")) %>% 
          select(mansec, idcon, viv),
        pesos %>% 
          filter(substr(mansec, 7, 9) != "999") %>% 
          filter(!(mansec %in% c(H$mansec, manzanas_excluir))) %>% 
          mutate(idcon = paste0(mansec, "l")) %>% 
          select(mansec, idcon, viv)) %>% 
  mutate(zona = ifelse(as.numeric(substr(mansec, 7, 9)) > 900, substr(mansec, 1, 9), 
                       paste0(substr(mansec, 1, 6), "ama"))) %>% 
  group_by(zona) %>% 
  mutate(nmanzon = n()) %>% 
  ungroup()

dim(auxiliar)[1] == (length(pesos$mansec[substr(pesos$mansec, 7, 9) != "999"]))

control <- auxiliar %>% 
  group_by(idcon) %>% 
  summarise(viv = sum(viv),
            nman = n(),
            nmanzon = min(nmanzon)) %>% 
  ungroup() %>% 
  mutate(nhnqh = ifelse(nman == nmanzon, 1, 0))

manzanas_juntar <- pesos %>% 
  filter(mansec %in% auxiliar$mansec[auxiliar$idcon %in% control$idcon[control$viv < li & control$nhnqh == 0]])

index <- unique(substr(manzanas_juntar$mansec, 1, 6))

k <- vector("list", 0)

for(i in 102:length(index)){
  manzanas <- read_sf(paste0("productos/01_preparacion_validacion/", 
                             substr(index[i], 1, 2), "/", index[i],
                             "/manzanas_extendidas.gpkg")) %>% 
    left_join(auxiliar %>% select(man = mansec, idcon, viv), by = "man") %>% 
    mutate(idcon = ifelse(man %in% manzanas_juntar$mansec, man, idcon))
  
  intermedio <- manzanas %>% 
    group_by(idcon) %>% 
    summarise(viv = sum(viv))
  
  matcon <- matinc(intermedio, tol = 1, id = "idcon")
  
  pescon <- cbind("idcon" = row.names(matcon)) %>% 
    as.data.frame() %>% 
    left_join(intermedio %>% 
                as.data.frame() %>% 
                select(-geom), by = "idcon") %>% 
    mutate(viv = ifelse(is.na(viv), 0, viv))
  # Aplicamos el algoritmo de conglomeración
  k1 <- conglomerar(matcon, peso = pescon, sl = li, id = "idcon") %>% 
    mutate(congf = str_pad(congf, 6, "left", "0"))
  
  apoyo <- manzanas %>% 
    as.data.frame() %>% 
    select(-geom) %>% 
    full_join(k1 %>% select(-viv), by = "idcon") %>% 
    mutate(congf = ifelse(is.na(congf), substr(idcon, 10, 15), congf ))
  
  conglomerado <- manzanas %>% 
    select(man, viv) %>% 
    left_join(apoyo %>%  select(man, congf), by = "man") %>% 
    group_by(congf, id_par = substr(man, 1, 6)) %>% 
    summarise(viv = sum(viv))
  
  zona_01 <- serpenteante(conglomerado, idp = "congf")%>% 
    mutate(orden = str_pad(orden,4,"left","0"),
           id_con = paste0(id_par, orden)) %>%
    select(congf, id_con, viv) 
  
  k[[i]] <- apoyo %>%
    select(man, viv, congf) %>% 
    left_join(zona_01 %>% 
                as.data.frame() %>% 
                select(-geom, -viv),
              by = "congf") %>% 
    select(man, id_con, viv)
  
  print(i)
}

K <- do.call("rbind", k)

index2 <- index1[!index1 %in% index]

t <- vector("list", 0)

for(i in 1:length(index2)){
  if(file.exists(paste0("productos/01_preparacion_validacion/", 
                                    substr(index2[i], 1, 2), "/", index2[i],
                                    "/manzanas_extendidas.gpkg"))){
    manzanas <- read_sf(paste0("productos/01_preparacion_validacion/", 
                               substr(index2[i], 1, 2), "/", index2[i],
                               "/manzanas_extendidas.gpkg")) %>% 
      left_join(auxiliar %>% select(man = mansec, idcon, viv), by = "man")
    
    conglomerado <- manzanas %>% 
      group_by(idcon, id_par = substr(man, 1, 6)) %>% 
      summarise(viv = sum(viv))
    
    zona_01 <- serpenteante(conglomerado, idp = "idcon")%>% 
      mutate(orden = str_pad(orden,4,"left","0"),
             id_con = paste0(id_par, orden)) %>%
      select(idcon, id_con, viv) 
    
    t[[i]] <- manzanas %>%
      as.data.frame() %>% 
      select(-geom) %>% 
      select(man, viv, idcon) %>% 
      left_join(zona_01 %>% 
                  as.data.frame() %>% 
                  select(-geom, -viv),
                by = "idcon") %>% 
      select(man, id_con, viv)
  }
}

Teta <- do.call("rbind", t)

final <- rbind(K, Teta)

saveRDS(final, paste0("productos/02_conglomeracion/conglomerados_", li, ".rds"))

auxiliar$mansec[!auxiliar$mansec %in% final$man]

final <- readRDS(paste0("productos/02_conglomeracion/conglomerados_", li, ".rds"))

lol <- final %>% 
  mutate(id_zon = substr(man, 1, 9)) %>% 
  group_by(id_zon) %>% 
  mutate(nmanzon = n()) %>% 
  ungroup() %>% 
  group_by(id_con) %>% 
  summarise(viv = sum(viv),
            nman = n(),
            nmanzon = min(nmanzon)) %>% 
  ungroup() %>% 
  mutate(control = ifelse(nman == nmanzon, 1, 0)) %>% 
  filter(viv < li) %>% 
  filter(control == 1)
