rm(list = ls())

library(tidyverse)
library(sf)

li <- 80

man_con <- readRDS(paste0("intermedios/02_conglomeracion/conglomerados_preliminares_", li, ".rds"))

peso_edif <- readRDS("intermedios/01_preparacion_validacion/precenso_edificios.rds")
# Calculamos el número de viviendas por man_sec
pesos <- peso_edif %>% 
  group_by(mansec) %>% 
  summarise(viv = sum(viv_ocu)) %>% 
  ungroup()

con_ais <- man_con %>% 
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

man_ais <- man_con %>% 
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
  filter(control == 0)

aux <- vector("list", 0)

index <- unique(substr(man_ais$id_con, 1, 6))

for (i in 1:length(index)){
  manzanas <- read_sf(paste0("productos/01_preparacion_validacion/", 
                             substr(index[i], 1, 2), "/", index[i],
                             "/manzanas_extendidas.gpkg")) %>% 
    left_join(man_con %>% select(man, id_con, viv), by = "man")
  
  auxiliar <- manzanas %>% 
    group_by(id_con) %>% 
    summarise(viv = sum(viv))
  
  aux_con <- auxiliar %>% 
    filter(viv >= li) %>% 
    rename(id_con_new = id_con,
           viv_new = viv) 
  
  aux_ais <- auxiliar %>% 
    filter(id_con %in% man_ais$id_con)
  
  
  
  lol <- st_join(aux_ais, aux_con, join = st_nearest_feature) %>% 
    as.data.frame() %>% 
    select(-geom) 
  distancia <- vector("numeric", dim(lol)[1])
  for (j in 1 : (dim(lol)[1])){
    distancia[j] <- as.numeric(st_distance(aux_ais %>% filter(id_con == lol$id_con[j]), aux_con %>% filter(id_con_new %in% lol$id_con_new[j])))
  }
  
  aux[[i]] <- lol %>% 
    cbind(distancia)

  print(index[i])
}

aux1 <- do.call("rbind", aux)

con_01 <- man_con %>% 
  left_join(aux1 %>% 
              select(id_con, id_con_new),
            by = "id_con") %>% 
  mutate(id_con_end = ifelse(is.na(id_con_new), id_con, id_con_new),
         id_par = substr(id_con_end, 1, 6)) %>% 
  arrange(id_con_end) %>% 
  group_by(id_par, id_con_end) %>% 
  summarise() %>% 
  ungroup() %>% 
  group_by(id_par) %>% 
  mutate(con = row_number()) %>% 
  ungroup()

man_con_01 <- man_con %>%
  left_join(aux1 %>% 
              select(id_con, id_con_new),
            by = "id_con") %>% 
  mutate(id_con_end = ifelse(is.na(id_con_new), id_con, id_con_new)) %>% 
  left_join(con_01 %>% 
              select(id_con_end, con),
            by = "id_con_end") %>% 
  mutate(id_conglomerado = paste0(substr(man, 1, 6), str_pad(con, 4, "left", "0"))) %>% 
  select(man, id_conglomerado, viv)

saveRDS(man_con_01, paste0("productos/02_conglomeracion/manzanas_conglomerados_", li, ".rds"))




