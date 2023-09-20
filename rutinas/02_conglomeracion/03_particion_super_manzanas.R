rm(list = ls())
# Cargamos las librerías a usar
library(tidyverse)
library(sf)

li = 80

peso_edif <- readRDS("intermedios/01_preparacion_validacion/precenso_edificios.rds")

pesos <- peso_edif %>% 
  group_by(man_sec = ifelse(substr(id_edif, 7, 9) == "999", 
                            substr(id_edif, 1, 12),
                            substr(id_edif, 1, 15))) %>% 
  summarise(viv = sum(viv_ocu)) %>% 
  ungroup()

particion <- peso_edif %>% 
  #filter(substr(id_edif, 1, 15) == "170155038009001") %>% 
  filter(substr(id_edif, 1, 15) %in% unique(pesos$man_sec[pesos$viv >=240])) %>% 
  arrange(id_edif) %>% 
  group_by(id_man = substr(id_edif, 1, 15)) %>% 
  mutate(freqa = cumsum(viv_ocu),
         ng = floor((sum(viv_ocu))/(1.5 * li)),
         tam = 1 +ceiling(sum(viv_ocu)/ng),
         grupo = 1 + floor(freqa/tam)) %>% 
  ungroup()

control <- particion %>% 
  group_by(id_man, grupo) %>% 
  summarise(viv_ocu = sum(viv_ocu))

particion_01 <- particion %>% 
  mutate(grupo = ifelse(id_man == "170150176011001" & grupo == 3, 1, grupo))

control_01 <- particion_01 %>% 
  group_by(id_man, grupo) %>% 
  summarise(viv_ocu = sum(viv_ocu))

# ca04 <- read_sf("insumos/01_preparacion_validacion/nogit/GEODATABASE_NACIONAL_2021.gpkg", 
#                 layer = "ca04_a")
# 
# ca_04_sup_man <- ca04 %>% 
#   filter(man %in% unique(control_01$id_man))
# 
# write_sf(ca_04_sup_man, "intermedios/02_conglomeracion/ca_04_sup_man.gpkg")


ca_04_sup_man <- read_sf("intermedios/02_conglomeracion/ca_04_sup_man.gpkg")

ca_04_cong <- ca_04_sup_man %>% 
  ungroup() %>% 
  left_join(particion_01 %>% 
              select(edif_censo = id_edif, grupo) %>% 
              ungroup(),
            by = "edif_censo")

write_sf(ca_04_cong, "prueba.gpkg")
