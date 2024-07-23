#
rm(list = ls())
library(tidyverse)
library(sf)

upm_ciu <- readRDS("pedidos/10_taller_socializacion_mmm/informacion/marco_upm.rds")

will_area <- upm_ciu %>%
  mutate(parroquia = substr(id_upm, 1, 6),
         ama_dis = ifelse(substr(id_upm, 7, 7) == "9", "dis", "ama")) %>% 
  group_by(domest, parroquia, ama_dis, area) %>% 
  summarise() %>% 
  ungroup()

zona <- read_sf("D:/AG/Cartografía/cartografiaecuador/insumos/baseprecensal/BNCPV22.gpkg", 
                layer = "zon_a")

shp_parroquia <- zona %>% 
  mutate(parroquia = substr(zon, 1, 6),
         ama_dis = ifelse(substr(zon, 7, 9) == "999", "dis", "ama")) %>% 
  group_by(parroquia, ama_dis) %>% 
  summarise() %>% 
  left_join(will_area, by = c("parroquia", "ama_dis"))

pichincha <- shp_parroquia %>% 
  filter(substr(parroquia, 1, 2) == "17") %>% 
  group_by(domest) %>% 
  summarise()

guayas <- shp_parroquia %>% 
  filter(substr(parroquia, 1, 2) == "09") %>%
  group_by(domest) %>% 
  summarise()

orellana <- shp_parroquia %>% 
  filter(substr(parroquia, 1, 2) == "22") %>% 
  group_by(domest) %>% 
  summarise()

galapagos <- shp_parroquia %>% 
  filter(substr(parroquia, 1, 2) == "20") %>% 
  group_by(domest) %>% 
  summarise()

par_pichincha_ama_dis <- shp_parroquia %>% 
  filter(substr(parroquia, 1, 2) == "17")

par_guayas_ama_dis <- shp_parroquia %>% 
  filter(substr(parroquia, 1, 2) == "09")

par_orellana_ama_dis <- shp_parroquia %>% 
  filter(substr(parroquia, 1, 2) == "22")

par_galapagos_ama_dis <- shp_parroquia %>% 
  filter(substr(parroquia, 1, 2) == "20")

write_sf(pichincha, "pedidos/10_taller_socializacion_mmm/informacion/17.gpkg")
write_sf(par_pichincha_ama_dis, "pedidos/10_taller_socializacion_mmm/informacion/17_par_ama_dis.gpkg")

write_sf(guayas, "pedidos/10_taller_socializacion_mmm/informacion/09.gpkg")
write_sf(par_guayas_ama_dis, "pedidos/10_taller_socializacion_mmm/informacion/09_par_ama_dis.gpkg")

write_sf(orellana, "pedidos/10_taller_socializacion_mmm/informacion/22.gpkg")
write_sf(par_orellana_ama_dis, "pedidos/10_taller_socializacion_mmm/informacion/22_par_ama_dis.gpkg")

write_sf(galapagos, "pedidos/10_taller_socializacion_mmm/informacion/20.gpkg")
write_sf(par_galapagos_ama_dis, "pedidos/10_taller_socializacion_mmm/informacion/20_par_ama_dis.gpkg")



















# per_2022 <- read_delim("insumos/censo/pob_2022.csv", 
#                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
# 
# will_area <- upm_ciu %>%
#   mutate(parroquia = substr(id_upm, 1, 6)) %>% 
#   group_by(parroquia, area) %>% 
#   summarise() %>% 
#   mutate(id = paste0(parroquia, area)) %>% 
#   arrange(id)
# 
# lol <- per_2022 %>% 
#   mutate(parroquia = paste0(I01, I02, I03)) %>% 
#   group_by(parroquia) %>% 
#   summarise(pob_ama = sum(I04 != "999"),
#             pob_dis = sum(I04 == "999")) %>% 
#   mutate(area1 = ifelse(pob_ama < 2000, "2", "1"),
#          area2 = ifelse(pob_dis > 0, "2", "")) %>% 
#   select(parroquia, area1, area2) %>% 
#   pivot_longer(cols = - parroquia, names_to = "var", values_to = "area") %>% 
#   filter(area != "") %>% 
#   group_by(parroquia, area) %>% 
#   summarise() %>% 
#   mutate(id = paste0(parroquia, area)) %>% 
#   arrange(id)
# 
# identical(will_area$id, lol$id)
