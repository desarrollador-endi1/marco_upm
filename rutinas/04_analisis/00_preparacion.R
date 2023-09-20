#
rm(list = ls())
#
library(tidyverse)
library(openxlsx)
#

# juntar toda la conglomeración

provincia <- list.dirs("productos/01_preparacion_validacion", 
                       full.names = F, 
                       recursive = F)

ca100 <- vector("list", 0)
ca80 <- vector("list", 0)
ca60 <- vector("list", 0)
cd <- vector("list", 0)

for(i in 1:length(provincia)){
  parroquia <- list.dirs(paste0("productos/01_preparacion_validacion/", 
                                provincia[i]), 
                         full.names = F, 
                         recursive = F)
  
  
  for(j in 1:length(parroquia)){
    ##########################################################################################
    # 01. Realizar la zonificación y numeración
    ##########################################################################################
    
    # Abrimos el shape de conglomerados
    
    if(file.exists(paste0("productos/02_conglomeracion/", provincia[i], "/",
                          parroquia[j], "/man_sec_conglomerados_", "100", ".rds"))){
      ca100[[parroquia[j]]] <- readRDS(paste0("productos/02_conglomeracion/", provincia[i], "/",
                                              parroquia[j], "/man_sec_conglomerados_", "100", ".rds"))
      
    }
    
    if(file.exists(paste0("productos/02_conglomeracion/", provincia[i], "/",
                          parroquia[j], "/man_sec_conglomerados_", "80", ".rds"))){
      ca80[[parroquia[j]]] <- readRDS(paste0("productos/02_conglomeracion/", provincia[i], "/",
                                             parroquia[j], "/man_sec_conglomerados_", "80", ".rds"))
    }
    
    if(file.exists(paste0("productos/02_conglomeracion/", provincia[i], "/",
                          parroquia[j], "/man_sec_conglomerados_", "60", ".rds"))){
      ca60[[parroquia[j]]] <- readRDS(paste0("productos/02_conglomeracion/", provincia[i], "/",
                                             parroquia[j], "/man_sec_conglomerados_", "60", ".rds"))
    }
    
    if(file.exists(paste0("productos/02_conglomeracion/", provincia[i], "/",
                          parroquia[j], "/man_sec_conglomerados_disperso_60.rds"))){
      cd[[parroquia[j]]] <- readRDS(paste0("productos/02_conglomeracion/", provincia[i], "/",
                                           parroquia[j], "/man_sec_conglomerados_disperso_60.rds"))
    }
    cat(paste0(provincia[i], " - ", parroquia[j], "\n"))
  }
}

# lectura de la base de edificios censales
edif <- readRDS("intermedios/01_preparacion_validacion/precenso_edificios.rds")

edif1 <- edif %>% 
  group_by(mansec) %>% 
  summarise(viv_tot = sum(viv_tot),
            viv_par = sum(viv_par),
            viv_ocu = sum(viv_ocu)) %>% 
  ungroup()

ca100 <- do.call(rbind, ca100) %>% 
  left_join(edif1, by = c("id_man" = "mansec")) %>% 
  mutate(id_upm = paste0(substr(id_man, 1, 6), congf)) %>% 
  filter(substr(congf, 1, 1) != "9") %>% # se eliminan las upm que resultaron aisladas por el num de viviendas
  group_by(id_upm) %>% 
  summarise(npol = n(),
            viv_tot = sum(viv_tot),
            viv_par = sum(viv_par),
            viv_ocu = sum(viv_ocu)) %>% 
  ungroup()

ca80 <- do.call(rbind, ca80) %>% 
  left_join(edif1, by = c("id_man" = "mansec")) %>% 
  mutate(id_upm = paste0(substr(id_man, 1, 6), congf)) %>% 
  group_by(id_upm) %>% 
  summarise(npol = n(),
            viv_tot = sum(viv_tot),
            viv_par = sum(viv_par),
            viv_ocu = sum(viv_ocu)) %>% 
  ungroup()

ca60 <- do.call(rbind, ca60) %>% 
  left_join(edif1, by = c("id_man" = "mansec")) %>% 
  mutate(id_upm = paste0(substr(id_man, 1, 6), congf)) %>% 
  group_by(id_upm) %>% 
  summarise(npol = n(),
            viv_tot = sum(viv_tot),
            viv_par = sum(viv_par),
            viv_ocu = sum(viv_ocu)) %>% 
  ungroup()

cd <- do.call(rbind, cd) %>% 
  left_join(edif1, by = c("id_sec" = "mansec")) %>% 
  mutate(id_upm = paste0(substr(id_sec, 1, 6), "9", substr(congf, 2, 6))) %>% 
  group_by(id_upm) %>% 
  summarise(npol = n(),
            viv_tot = sum(viv_tot),
            viv_par = sum(viv_par),
            viv_ocu = sum(viv_ocu)) %>% 
  ungroup()

n_distinct(substr(edif1$mansec, 1, 6))
n_distinct(substr(edif1$mansec[substr(edif1$mansec, 7, 9)!="999"], 1, 6))
n_distinct(substr(edif1$mansec[substr(edif1$mansec, 7, 9)=="999"], 1, 6))
n_distinct(substr(ca60$id_upm, 1, 6))
n_distinct(substr(ca80$id_upm, 1, 6))
n_distinct(substr(ca100$id_upm, 1, 6))
n_distinct(substr(cd$id_upm, 1, 6))

#
# Escenarios
#
esc100 <- ca100 %>% 
  rbind(cd) %>% 
  mutate(Provincia = substr(id_upm, 1, 2),
         Aman_dis = ifelse(substr(id_upm, 7, 7) == "9", "Disperso", "Amanzanado")) %>% 
  group_by(Provincia, Aman_dis) %>% 
  summarise(n_upm = n_distinct(id_upm),
            n_pol_prom = round(mean(npol, na.rm = T), 1),
            min_viv_tot = min(viv_tot, na.rm = T),
            media_viv_tot = round(mean(viv_tot, na.rm = T), 1),
            max_viv_tot = max(viv_tot, na.rm = T),
            min_viv_ocu = min(viv_ocu, na.rm = T),
            media_viv_ocu = round(mean(viv_ocu, na.rm = T), 1),
            max_viv_ocu = max(viv_ocu, na.rm = T)) %>% 
  ungroup()

esc80 <- ca80 %>% 
  rbind(cd) %>% 
  mutate(Provincia = substr(id_upm, 1, 2),
         Aman_dis = ifelse(substr(id_upm, 7, 7) == "9", "Disperso", "Amanzanado")) %>% 
  group_by(Provincia, Aman_dis) %>% 
  summarise(n_upm = n_distinct(id_upm),
            n_pol_prom = round(mean(npol, na.rm = T), 1),
            min_viv_tot = min(viv_tot, na.rm = T),
            media_viv_tot = round(mean(viv_tot, na.rm = T), 1),
            max_viv_tot = max(viv_tot, na.rm = T),
            min_viv_ocu = min(viv_ocu, na.rm = T),
            media_viv_ocu = round(mean(viv_ocu, na.rm = T), 1),
            max_viv_ocu = max(viv_ocu, na.rm = T)) %>% 
  ungroup()

esc60 <- ca60 %>% 
  rbind(cd) %>% 
  mutate(Provincia = substr(id_upm, 1, 2),
         Aman_dis = ifelse(substr(id_upm, 7, 7) == "9", "Disperso", "Amanzanado")) %>% 
  group_by(Provincia, Aman_dis) %>% 
  summarise(n_upm = n_distinct(id_upm),
            n_pol_prom = round(mean(npol, na.rm = T), 1),
            min_viv_tot = min(viv_tot, na.rm = T),
            media_viv_tot = round(mean(viv_tot, na.rm = T), 1),
            max_viv_tot = max(viv_tot, na.rm = T),
            min_viv_ocu = min(viv_ocu, na.rm = T),
            media_viv_ocu = round(mean(viv_ocu, na.rm = T), 1),
            max_viv_ocu = max(viv_ocu, na.rm = T)) %>% 
  ungroup()

resumen <- edif1 %>% 
  mutate(identif = substr(mansec, 1, 12)) %>% 
  group_by(identif) %>% 
  summarise(npol = n_distinct(mansec),
            viv_tot = sum(viv_tot),
            viv_ocu = sum(viv_ocu)) %>% 
  mutate(Provincia = substr(identif, 1, 2),
         Aman_dis = ifelse(substr(identif, 7, 7) == "9", "Disperso", "Amanzanado")) %>% 
  group_by(Provincia, Aman_dis) %>% 
  summarise(n_sec = n_distinct(identif),
            n_pol_prom = round(mean(npol, na.rm = T), 1),
            min_viv_tot = min(viv_tot, na.rm = T),
            media_viv_tot = round(mean(viv_tot, na.rm = T), 1),
            max_viv_tot = max(viv_tot, na.rm = T),
            min_viv_ocu = min(viv_ocu, na.rm = T),
            media_viv_ocu = round(mean(viv_ocu, na.rm = T), 1),
            max_viv_ocu = max(viv_ocu, na.rm = T)) %>% 
  ungroup()

hist100 <- ca100 %>% 
  rbind(cd) %>% 
  mutate(Aman_dis = ifelse(substr(id_upm, 7, 7) == "9", "Disperso", "Amanzanado")) %>% 
  ggplot(aes(viv_tot, col = Aman_dis)) +
  geom_histogram(position = "nudge", bins = 30, fill = "grey") +
  facet_wrap(~Aman_dis)

hist100

# se le agrego 20 viviendas totales a la provincia de guayas por la falta de informaión de guayaquil
esc100_01 <- esc100 %>% 
  mutate(media_viv_tot = ifelse(Provincia == "09" & Aman_dis == "Amanzanado", 
                                media_viv_tot + 20, media_viv_tot)) %>% 
  select(-n_pol_prom, -n_upm)

write.xlsx(esc100_01, "productos/04_analisis/prom_viv_tot_provin_esc_100.xlsx")


