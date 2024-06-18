#
rm(list = ls())
# Cargamos las librerías a usar
library(tidyverse)
library(sf)
library(foreach)
library(doParallel)

# Definimos el límite inferior del tamaño de los conglomerados
li = 60

manzanas_sectores_upm <- readRDS("pedidos/09_conglomeracion_dmq/productos/02_conglomeracion/manzanas_sectores_upm.rds")

index <- list.dirs("pedidos/09_conglomeracion_dmq/intermedios/01_preparacion_validacion/", 
                   full.names = F, recursive = F)

upm <- vector("list", 0)
mm <- vector("list", 0)
bdd_dis <- vector("list", 0)

for(i in 1:length(index)){
  if(file.exists(paste0("pedidos/09_conglomeracion_dmq/intermedios/01_preparacion_validacion/", 
                        index[i], "/manzanas_extendidas.gpkg"))){
    # Cargamos las manzanas de cada parroquia
    manzanas <- read_sf(paste0("pedidos/09_conglomeracion_dmq/intermedios/01_preparacion_validacion/", 
                               index[i], "/manzanas_extendidas.gpkg"))

    upm[[i]] <- manzanas %>%
      left_join(manzanas_sectores_upm %>%
                  select(man = man_sec, id_upm = paste0("id_upm_", li)), by = "man") %>%
      group_by(id_upm) %>%
      summarise() %>%
      st_cast("MULTIPOLYGON") %>%
      st_cast("POLYGON") %>% 
      mutate(area = as.numeric(st_area(.))) %>% 
      filter(area > 10) %>% 
      group_by(id_upm) %>% 
      mutate(n = n()) %>% 
      ungroup() %>% 
      filter(n > 1)
    
    index1 <- unique(upm[[i]]$id_upm)
    
    if(length(index1) > 0){
      dis <- rep(0, length(index1))
      
      for (j in 1 : length(index1)){
        a <- st_distance(upm[[i]] %>% filter(id_upm == index1[j]))
        diag(a) <- 10000
        a <- as.numeric(apply(a, 1, min))
        dis[j] <- max(a)
      }
      bdd_dis[[i]] <- data.frame(id_upm = index1, distancia = dis, cod_adz = substr(index[i], 12,13))
    }
    
    #   mutate(area = as.numeric(st_area(.))) %>% 
    #   filter(area > 10) %>% 
    #   as.data.frame() %>%
    #   group_by(id_upm) %>%
    #   summarise(n = n()) %>%
    #   filter(n > 1) %>%
    #   ungroup()
    # 
    # mm[[i]] <- manzanas %>% 
    #   st_cast("MULTIPOLYGON") %>%
    #   st_cast("POLYGON") %>% 
    #   mutate(area = as.numeric(st_area(.))) %>% 
    #   filter(area > 10) %>% 
    #   as.data.frame() %>% 
    #   group_by(man) %>% 
    #   summarise(n = n()) %>% 
    #   filter(n > 1) %>% 
    #   ungroup()
  }
  print(index[i])
}

distancia <- do.call("rbind", bdd_dis)
#####
# upm_dis <- do.call("rbind", upm)
# 
# i = c(1:1042)[index == "220452"]
# 
# upm_problema <- "2204520002"
# 
# manzanas <- read_sf(paste0("productos/01_preparacion_validacion/", 
#                            substr(index[i], 1, 2), "/", index[i],
#                            "/manzanas_extendidas.gpkg"))
# 
# lala <- manzanas %>%
#   left_join(manzanas_sectores_upm %>%
#               select(man = man_sec, id_upm = paste0("id_upm_", li)), by = "man") %>% 
#   filter(id_upm == upm_problema) %>%
#   group_by(id_upm) %>%
#   summarise() %>%
#   st_cast("MULTIPOLYGON") %>%
#   st_cast("POLYGON") 
# 
# write.table(manzanas_sectores_upm[manzanas_sectores_upm$id_upm_60 == upm_problema,1], "clipboard", row.names = F)
# 
# loli <- manzanas %>%
#   left_join(manzanas_sectores_upm %>%
#               select(man = man_sec, id_upm = paste0("id_upm_", li)), by = "man") %>%
#   group_by(id_upm) %>%
#   summarise()
# 
# write_sf(loli, "lala.gpkg")
# 
# 
# lol <- manzanas_sectores_upm %>% 
#   group_by(id_upm_60) %>% 
#   summarise(viv_tot = sum(viv_tot_pre, na.rm = T),
#             viv_ocu = sum(viv_ocu_pre, na.rm = T)) %>% 
#   mutate(aman_dis = ifelse(substr(id_upm_60, 7, 7) == "9", "disp", "aman")) %>% 
#   group_by(pro = substr(id_upm_60, 1, 2), aman_dis) %>% 
#   summarise(min_viv_tot	= min(viv_tot, na.rm = T),
#             media_viv_tot = round(mean(viv_tot, na.rm = T), 1),
#             max_viv_tot	= max(viv_tot, na.rm = T),
#             min_viv_ocu	= min(viv_ocu, na.rm = T),
#             media_viv_ocu	= round(mean(viv_ocu, na.rm = T), 1),
#             max_viv_ocu	= max(viv_ocu, na.rm = T))

#####
resumen_upm <- manzanas_sectores_upm %>% 
  mutate(id_zon = case_when(substr(man_sec, 7, 7) == "9" ~ substr(man_sec, 1, 9),
                            T ~ paste0(substr(man_sec, 1, 6), "000"))) %>% 
  group_by(id_zon) %>% 
  mutate(n_man_sec_zon = n()) %>% 
  ungroup() %>% 
  group_by(id_upm = id_upm_60, id_zon, n_man_sec_zon, cod_adz) %>% 
  summarise(viv = sum(viv),
            n_man_upm = n()) %>% 
  ungroup() %>% 
  group_by(id_upm) %>% 
  mutate(n_partes_upm = n()) %>% 
  ungroup() %>% 
  left_join(distancia, by = c("id_upm", "cod_adz")) %>% 
  ungroup() %>% 
  mutate(distancia = ifelse(is.na(distancia), 0, distancia),
         tipo = ifelse(substr(id_upm, 7, 7) == "9", "disperso", "amanzanado"),
         control = case_when(distancia > 20 ~ "revisar",
                             n_partes_upm > 1 ~ "upm zonas diferentes",
                             viv >= 60 ~ "ok",
                             viv < 60 & n_man_sec_zon == n_man_upm ~ "ya no hay más",
                             T ~ "revisar"))


apoyo <- resumen_upm %>% 
  filter(control %in% c("ya no hay más", "revisar") & tipo == "amanzanado")

index_ynhm <- apoyo$id_upm
index_cod_adz <- apoyo$cod_adz

lol <- vector("list", 0)

for(i in 1:length(index_ynhm)){
  manzanas <- read_sf(paste0("pedidos/09_conglomeracion_dmq/intermedios/01_preparacion_validacion/", 
                             substr(index_ynhm[i], 1, 6), "_1701", index_cod_adz[i],
                             "/manzanas_extendidas.gpkg")) %>% 
    rename(man_sec = man)
  
  upm <- manzanas %>% 
    left_join(manzanas_sectores_upm %>% 
                select(man_sec, id_upm = id_upm_60), by = "man_sec") %>% 
    group_by(id_upm) %>% 
    summarise()
  
  if(dim(upm)[1] > 1){
    lol[[i]] <- st_join(upm %>% filter(id_upm == index_ynhm[i]), 
                        upm %>% filter(id_upm != index_ynhm[i]), 
                        join = st_nearest_feature) %>% 
      mutate(dis = as.numeric(st_distance(upm[upm$id_upm == .$id_upm.x,], 
                                          upm[upm$id_upm == .$id_upm.y,]))) %>% 
      as.data.frame() %>% 
      select(-geom) %>% 
      mutate(cod_adz = substr(index[i], 12,13))
  }
  print(i)
}

lol1 <- do.call("rbind", lol)

resumen_upm_01 <- resumen_upm %>% 
  left_join(lol1 %>% select(id_upm = id_upm.x, dis), by = "id_upm") %>% 
  mutate(dis = ifelse(is.na(dis), 0, dis)) %>% 
  select(id_upm, viv, dis_conexidad = distancia, tipo, control, dis_upm_cercana = dis, cod_adz)

revisar <- resumen_upm_01 %>% 
  filter(control == "revisar" | (dis_upm_cercana < 20 & dis_upm_cercana > 0))

save(resumen_upm_01, revisar, file = "pedidos/09_conglomeracion_dmq/intermedios/02_conglomeracion/revisar.RData")
