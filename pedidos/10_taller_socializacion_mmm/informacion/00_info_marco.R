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


zona <- read_sf("D:/Disco_F/!Shapes/2022/BNCPV22.gpkg", 
                layer = "zon_a")

# zona <- read_sf("D:/AG/Cartografía/cartografiaecuador/insumos/baseprecensal/BNCPV22.gpkg", 
#                 layer = "zon_a")

shp_parroquia <- zona %>% 
  mutate(parroquia = substr(zon, 1, 6),
         ama_dis = ifelse(substr(zon, 7, 9) == "999", "dis", "ama")) %>% 
  group_by(parroquia, ama_dis) %>% 
  summarise() %>% 
  left_join(will_area, by = c("parroquia", "ama_dis"))

# shape provincial
load("D:/Disco_F/ENDI2/insumos/03_muestra_usm/dpa_2022.RData")
rm(parroquia, canton)

provincia <- provincia %>% 
  mutate(nprovin = tolower(gsub(" ", "_", nprovin)))


for(i in 1:length(provincia$nprovin)){
  
  ob_prov <- shp_parroquia %>% 
    filter(substr(parroquia, 1, 2) == provincia$provin[i]) %>% 
    group_by(domest) %>% 
    summarise()
  
  ob_par <- shp_parroquia %>% 
    filter(substr(parroquia, 1, 2) == provincia$provin[i])
  
  
  write_sf(obj = ob_prov,
           dsn = "pedidos/10_taller_socializacion_mmm/informacion/provincias.gpkg",
           layer = provincia$nprovin[i])
  write_sf(obj = ob_par,
           dsn = "pedidos/10_taller_socializacion_mmm/informacion/parroquias.gpkg",
           layer = paste0("aman_dis_", provincia$nprovin[i]))
  
  print(i)
}

# para ppt marco, cantones guayaquil y riobamba

shp_canton <- zona %>% 
  mutate(canton = substr(zon, 1, 4),
         parroquia = substr(zon, 1, 6),
         ama_dis = ifelse(substr(zon, 7, 9) == "999", "dis", "ama")) %>% 
  filter(canton %in% c("0901", "0601", "0701")) %>% 
  group_by(canton, parroquia, ama_dis) %>% 
  summarise() %>% 
  left_join(will_area, by = c("parroquia", "ama_dis")) %>% 
  mutate(area_dpa = ifelse(substr(parroquia, 5, 6) == "50" & ama_dis == "ama", 1, 2)) %>% 
  ungroup()

write_sf(obj = shp_canton,
         dsn = "pedidos/10_taller_socializacion_mmm/administracion de muestras/cantones_0901_0701_0601.gpkg",
         layer = "cantones_0901_0701_0601")













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
