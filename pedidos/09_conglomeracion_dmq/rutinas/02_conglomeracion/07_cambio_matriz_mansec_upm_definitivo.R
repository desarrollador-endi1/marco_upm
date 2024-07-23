rm(list = ls())

library(tidyverse)
library(sf)

man_sec_upm <- readRDS("productos/02_conglomeracion/manzanas_sectores_upm_final.rds") 

man_sec_upm_dmq <- readRDS("pedidos/09_conglomeracion_dmq/productos/02_conglomeracion/manzanas_sectores_upm_final.rds")

n_mansec_170150_170180 <- n_distinct(man_sec_upm$man_sec[substr(man_sec_upm$man_sec, 1, 6) %in% c("170150", "170180")])

man_sec_upm_01 <- man_sec_upm %>%
  rename(id_upm_old = id_upm) %>% 
  left_join(man_sec_upm_dmq %>% 
              select(man_sec, id_upm_new = id_upm),
            by = "man_sec") %>% 
  mutate(id_upm = ifelse(is.na(id_upm_new), id_upm_old, id_upm_new))

manzanas_170150 <- read_sf(paste0("productos/01_preparacion_validacion/17/170150/manzanas_extendidas.gpkg"))

upm_old_170150 <- manzanas_170150 %>% 
  rename(man_sec = man) %>% 
  left_join(man_sec_upm, by = "man_sec") %>% 
  group_by(id_upm) %>% 
  summarise()

write_sf(upm_old_170150, "pedidos/09_conglomeracion_dmq/analisis/upm_old_170150.gpkg")

upm_new_170150 <- manzanas_170150 %>% 
  rename(man_sec = man) %>% 
  left_join(man_sec_upm_dmq, by = "man_sec") %>% 
  group_by(id_upm) %>% 
  summarise()

write_sf(upm_new_170150, "pedidos/09_conglomeracion_dmq/analisis/upm_new_170150.gpkg")

dif <- st_intersection(upm_old_170150, upm_new_170150) 

lol <- dif %>%
  mutate(area = as.numeric(st_area(.))) %>% 
  filter(area >0) %>% 
  as.data.frame() %>% 
  select(-geom) %>% 
  group_by(id_upm) %>% 
  mutate(nold = n()) %>% 
  ungroup() %>%
  group_by(id_upm.1) %>% 
  mutate(nnew = n()) %>% saveRDS(lol, "D:/MAG/marco_administracion/insumos/01_general/viv_tot_ocu_man_sec.rds")
saveRDS(lol, "D:/MAG/marco_administracion/insumos/01_general/viv_tot_ocu_man_sec.rds")

  ungroup() %>%
  filter(nold >1) %>% 
  filter(nnew >1)

saveRDS(man_sec_upm_01, "pedidos/09_conglomeracion_dmq/man_sec_upm_final_dmq.rds")
