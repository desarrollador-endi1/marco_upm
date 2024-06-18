rm(list = ls())
# Cargamos las librerías a usar
library(tidyverse)
library(sf)
library(foreach)
library(doParallel)
# Cargamos las funciones a utilizar
source("rutinas/funciones/matinc.R")
source("rutinas/funciones/conglomera2.R")
source("Rutinas/Funciones/serpenteante.r")

# Definimos el límite inferior del tamaño de los conglomerados
li = 60
# Cargamos la base con el número de viviendas por edificio
peso_edif <- readRDS("intermedios/01_preparacion_validacion/precenso_edificios.rds")
# Calculamos el número de viviendas por man_sec
pesos <- peso_edif %>% 
  group_by(mansec) %>% 
  summarise(viv = sum(viv_ocu)) %>% 
  ungroup() %>% 
  filter(substr(mansec, 1, 6) %in% c("170150", "170180"))
# Se abre la división de manzanas en función del li
particion <- readRDS(paste0("intermedios/02_conglomeracion/particion_manzanas_li_", li, ".rds"))
manzanas_excluir <- unique(particion$mansec)

index <- list.dirs("pedidos/09_conglomeracion_dmq/intermedios/01_preparacion_validacion/", 
                   full.names = F, recursive = F)

h <- vector("list", 0)

for(i in 1:length(index)){
  if(file.exists(paste0("pedidos/09_conglomeracion_dmq/intermedios/01_preparacion_validacion/",
                        index[i], "/manzanas_extendidas.gpkg"))){
    h[[i]] <- readRDS(paste0("pedidos/09_conglomeracion_dmq/productos/02_conglomeracion/",
                              index[i], "/man_sec_conglomerados_", li, ".rds")) %>% 
      mutate(cod_adz = substr(index[i], 12, 13))
  }
}
H <- do.call("rbind", h)
# Calculamos el número de manzanas perdidas en la conglomeración
sum(!pesos$mansec[substr(pesos$mansec, 7, 9) != "999"] %in% H$mansec) - 
  n_distinct(particion$mansec[substr(particion$mansec, 1, 6) %in% c("170150", "170180")])
# Control manzanas partidas manzanas conglomeradas
sum(H$mansec %in% unique(particion$mansec))

# creamos el identificador de conglomerado (idcon) 6 digitos de parroquia, 9 digitos de "congolomerado"
# para manzanas partidas y perdidas 9 digitos de identificador de manzana y conglomerado "000" y 
# 6 digitos de congf, 1 de particion, conglomeracion y perdidas (c conglomeracion, l perdidas, p partidas)

auxiliar <- H %>% 
  mutate(idcon = paste0(substr(mansec, 1, 6), cod_adz, "000", congf, "c")) %>% 
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
  filter(mansec %in% auxiliar$mansec[auxiliar$idcon %in% control$idcon[control$viv < li & control$nhnqh == 0]]) %>% 
  rbind(pesos %>% 
          filter(mansec %in% H$mansec[substr(H$mansec, 7, 7) != "9" & substr(H$congf, 1, 1) == "9"])
  ) %>% 
  rbind(pesos %>% 
          filter(mansec %in% H$mansec[substr(H$mansec, 7, 7) == "9" & substr(H$congf, 1, 1) == "9"])
  )

index <- list.dirs("pedidos/09_conglomeracion_dmq/intermedios/01_preparacion_validacion/", 
                   full.names = F, recursive = F)

k <- vector("list", 0)

for(i in 1:length(index)){
  if(file.exists(paste0("pedidos/09_conglomeracion_dmq/intermedios/01_preparacion_validacion/",
                        index[i], "/manzanas_extendidas.gpkg"))){
  manzanas <- read_sf(paste0("pedidos/09_conglomeracion_dmq/intermedios/01_preparacion_validacion/",
                             index[i], "/manzanas_extendidas.gpkg")) %>% 
    left_join(auxiliar %>% select(man = mansec, idcon, viv), by = "man") %>% 
    mutate(idcon = ifelse(man %in% manzanas_juntar$mansec, man, idcon))
  
  if(sum(manzanas_juntar$mansec %in% manzanas$man) > 0){
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
    k1 <- conglomera2(matcon, peso = pescon, sl = li, idp = "idcon") %>% 
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
      select(man, id_con, viv) %>% 
      mutate(cod_adz = substr(index[i], 12,13))
    
    
  }
  }
  print(index[i])
}

K <- do.call("rbind", k)

index <- list.dirs("pedidos/09_conglomeracion_dmq/intermedios/01_preparacion_validacion/", 
                   full.names = F, recursive = F)

t <- vector("list", 0)

for(i in 1:length(index)){
  if(file.exists(paste0("pedidos/09_conglomeracion_dmq/intermedios/01_preparacion_validacion/",
                        index[i], "/manzanas_extendidas.gpkg"))){
    manzanas <- read_sf(paste0("pedidos/09_conglomeracion_dmq/intermedios/01_preparacion_validacion/",
                        index[i], "/manzanas_extendidas.gpkg")) %>% 
      left_join(auxiliar %>% select(man = mansec, idcon, viv), by = "man")
    
    if(sum(manzanas_juntar$mansec %in% manzanas$man) == 0){
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
        select(man, id_con, viv) %>% 
        mutate(cod_adz = substr(index[i], 12,13))
    }
    print(index[i])
    
  }
}

Teta <- do.call("rbind", t)

final <- rbind(K, Teta)

saveRDS(final, paste0("pedidos/09_conglomeracion_dmq/intermedios/02_conglomeracion/conglomerados_preliminares_", li, ".rds"))

auxiliar$mansec[!auxiliar$mansec %in% final$man]

