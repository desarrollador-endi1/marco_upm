#
rm(list = ls())
#
library(sf)
library(tidyverse)
#
selecciones <- readRDS("pedidos/10_taller_socializacion_mmm/administracion de muestras/marco_upm_06.rds")

b1 <- selecciones %>% 
  mutate(grupo = substr(id_upm, 11, 12),
         id_upm = substr(id_upm, 1, 10)) %>% 
  select(domgeo, estrato, id_upm, ends_with("_sel")) %>% 
  filter(enighur_sel + endi3_sel + enciet_202410_sel + enciet_202411_sel 
         + enciet_202412_sel != 0) %>% 
  pivot_longer(cols = c("enighur_sel", "endi3_sel", "enciet_202410_sel", 
                        "enciet_202411_sel", "enciet_202412_sel"),
               names_to = "encuesta",
               values_to = "sel") %>% 
  filter(sel == 1) %>% 
  mutate(encuesta = gsub("_sel", "", encuesta)) %>% 
  select(-sel)

marco <- selecciones  %>% 
  mutate(id_upm = substr(id_upm, 1, 10)) %>% 
  group_by(domgeo, estrato, id_upm) %>% 
  summarise() %>% 
  ungroup()
  
upm <- read_sf("productos/05_limites_upm/upm.gpkg")

upm_sel <- upm %>% 
  right_join(b1, by ="id_upm") %>% 
  select(id_upm, domgeo, estrato, viv, encuesta, geom)

upm_marco <- upm %>% 
  right_join(marco, by ="id_upm") %>% 
  mutate(area_sd = ifelse(substr(estrato, 1, 2) < 25,
                          paste0("p", substr(estrato, 3, 4)),
                          paste0("c", substr(estrato, 3, 4)))) %>% 
  select(id_upm, domgeo, estrato, area_sd, viv, geom)


write_sf(upm_sel, "pedidos/10_taller_socializacion_mmm/administracion de muestras/upm_sel.gpkg")
write_sf(upm_marco, "pedidos/10_taller_socializacion_mmm/administracion de muestras/upm_marco.gpkg")
