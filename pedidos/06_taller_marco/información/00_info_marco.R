#
rm(list = ls())
library(tidyverse)
library(sf)

upm_ciu <- readRDS("pedidos/04_pareto/upm_ciu.rds")

will_area <- upm_ciu %>%
  mutate(parroquia = substr(id_upm, 1, 6),
         ama_dis = ifelse(substr(id_upm, 7, 7) == "9", "dis", "ama")) %>% 
  group_by(parroquia, ama_dis, area) %>% 
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
  mutate(dominio = case_when(parroquia == "170150" & area == "1" ~ "251",
                             T ~ paste0(substr(parroquia, 1, 2), area))) %>% 
  group_by(dominio) %>% 
  summarise()

par_pichincha_ama_dis <- shp_parroquia %>% 
  filter(substr(parroquia, 1, 2) == "17")

write_sf(pichincha, "pedidos/06_taller_marco/información/17.gpkg")
write_sf(par_pichincha, "pedidos/06_taller_marco/información/17_par_ama_dis.gpkg")



















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
