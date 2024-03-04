rm(list = ls())

library(tidyverse)
library(sf)

# Cargamos la base con el número de viviendas por edificio
precenso <- read_delim("D:/AG/Cartografía/cartografiaecuador/insumos/baseprecensal/BASE PRECENSAL/Base Precensal160323.txt_20230316_090503_0.csv",
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

particion_manzanas <- readRDS("intermedios/02_conglomeracion/particion_manzanas_li_60.rds")

manzanas_sectores_upm <- readRDS("productos/02_conglomeracion/manzanas_sectores_upm_final.rds") %>% 
  rename(mansec = man_sec)







resumen_upm <- manzanas_sectores_upm %>% 
  mutate(id_zon = case_when(substr(mansec, 7, 7) == "9" ~ substr(mansec, 1, 9),
                            T ~ paste0(substr(mansec, 1, 6), "000"))) %>% 
  group_by(id_zon) %>% 
  mutate(n_man_sec_zon = n()) %>% 
  ungroup() %>% 
  group_by(id_upm , id_zon, n_man_sec_zon) %>% 
  summarise(viv = sum(viv_ocu),
            n_man_upm = n()) %>% 
  ungroup() %>% 
  group_by(id_upm) %>% 
  mutate(n_partes_upm = n()) 

upm_manzanas_partidas <- manzanas_sectores_upm %>% 
  mutate(man_partida = ifelse(mansec %in% particion_manzanas$mansec, 1, 0)) %>% 
  group_by(id_upm) %>% 
  mutate(upm_partida = max(man_partida)) %>% 
  ungroup() %>% 
  filter(upm_partida == 1)  


lol <- particion_manzanas %>% 
  group_by(mansec) %>% 
  summarise(grupo = max(grupo),
            viv_man = sum(viv)) %>% 
  left_join(manzanas_sectores_upm %>% 
              select(mansec, id_upm), 
            by = "mansec") %>% 
  left_join(resumen_upm %>% 
              select(id_upm, n_man_upm, viv_upm = viv), 
            by = "id_upm")

lamil <- particion_manzanas %>% 
  group_by(mansec, grupo) %>% 
  summarise(viv_man = sum(viv))
