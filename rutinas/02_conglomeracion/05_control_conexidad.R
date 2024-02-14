rm(list = ls())
# Cargamos las librerías a usar
library(tidyverse)
library(sf)
library(foreach)
library(doParallel)

# Definimos el límite inferior del tamaño de los conglomerados
li = 60

manzanas_sectores_upm <- readRDS("productos/02_conglomeracion/manzanas_sectores_upm.rds")

load("intermedios/lista_parroquias.RData")

upm <- vector("list", 0)
mm <- vector("list", 0)
bdd_dis <- vector("list", 0)

for(i in 1:length(index)){
  if(file.exists(paste0("productos/01_preparacion_validacion/", 
                        substr(index[i], 1, 2), "/", index[i],
                        "/manzanas_extendidas.gpkg"))){
    # Cargamos las manzanas de cada parroquia
    manzanas <- read_sf(paste0("productos/01_preparacion_validacion/", 
                               substr(index[i], 1, 2), "/", index[i],
                               "/manzanas_extendidas.gpkg"))

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
      bdd_dis[[i]] <- data.frame(id_upm = index1, distancia = dis)
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

upm_dis <- do.call("rbind", upm)

apoyo <- manzanas_sectores_upm %>% 
  left_join(rename(upm_dis, id_upm_60 = id_upm, n_upm = n), by ="id_upm_60") %>% 
  left_join(rename(mm_dis, man_sec = man, n_man = n), by ="man_sec") %>% 
  filter(id_upm_60 %in% upm_dis$id_upm) %>% 
  group_by(id_upm_60) %>% 
  summarise(n_upm = mean(n_upm),
            n_man = sum(n_man, na.rm = T)) %>% 
  mutate(control = case_when(n_man == 0 ~ "problema geografico",
                             n_upm == n_man ~ "no problema",
                             T ~ "no se"))


i = c(1:1042)[index == "010162"]

lala <- manzanas %>%
  left_join(manzanas_sectores_upm %>%
              select(man = man_sec, id_upm = paste0("id_upm_", li)), by = "man") %>% 
  filter(id_upm == "0101620016") %>%
  group_by(id_upm) %>%
  summarise() %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON") 

write.table(manzanas_sectores_upm[manzanas_sectores_upm$id_upm_60 == "0101620016",1], "clipboard", row.names = F)

write_sf(manzanas, "lala.gpkg")


lol <- manzanas_sectores_upm %>% 
  group_by(id_upm_60) %>% 
  summarise(viv_tot = sum(viv_tot_pre, na.rm = T),
            viv_ocu = sum(viv_ocu_pre, na.rm = T)) %>% 
  mutate(aman_dis = ifelse(substr(id_upm_60, 7, 7) == "9", "disp", "aman")) %>% 
  group_by(pro = substr(id_upm_60, 1, 2), aman_dis) %>% 
  summarise(min_viv_tot	= min(viv_tot, na.rm = T),
            media_viv_tot = round(mean(viv_tot, na.rm = T), 1),
            max_viv_tot	= max(viv_tot, na.rm = T),
            min_viv_ocu	= min(viv_ocu, na.rm = T),
            media_viv_ocu	= round(mean(viv_ocu, na.rm = T), 1),
            max_viv_ocu	= max(viv_ocu, na.rm = T))
