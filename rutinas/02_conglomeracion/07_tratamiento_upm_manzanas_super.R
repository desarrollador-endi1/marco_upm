#rm(list = ls())

options(scipen = 999)

library(tidyverse)
library(sf)
#library(lwgeom)
source("rutinas/funciones/matinc.R")
# abrir shape de ingresos y ca04

ingresos <- read_sf("D:/AG/Cartografía/cartografiaecuador/insumos/baseprecensal/BNCPV22_ingresos.gpkg")
#ejes <- read_sf("D:/AG/Cartografía/cartografiaecuador/insumos/baseprecensal/BNCPV22.gpkg", layer = "ejes_l")
ca04 <- read_sf("D:/AG/Cartografía/cartografiaecuador/insumos/baseprecensal/BNCPV22.gpkg", layer = "ca04_a")

# Cargamos la base con el número de viviendas por edificio
# precenso <- read_delim("D:/AG/Cartografía/cartografiaecuador/insumos/baseprecensal/BASE PRECENSAL/Base Precensal160323.txt_20230316_090503_0.csv",
#                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
# 
# edificio_conjunto <- precenso %>% 
#   mutate(n_edif = str_pad(n_edif, 3, "left", "0"), 
#          edif_cod = ifelse(zon != "999", 
#                            paste0(pro, can, par, zon, sec, man, n_edif),
#                            paste0(pro, can, par, zon, sec, n_loc, n_edif))) %>% 
#   filter(cod_dire == "Condominio o Conjunto") %>% 
#   group_by(edif_cod, cod_dire) %>% 
#   summarise()
# 
# saveRDS(edificio_conjunto, "intermedios/02_conglomeracion/edificio_conjunto.rds")
edificio_conjunto <- readRDS("intermedios/02_conglomeracion/edificio_conjunto.rds")

# Archivo de super manzanas
particion_manzanas <- readRDS("intermedios/02_conglomeracion/particion_manzanas_li_60.rds")

# Resultado de la conglomeración
manzanas_sectores_upm <- readRDS("productos/02_conglomeracion/manzanas_sectores_upm_final.rds") %>% 
  rename(mansec = man_sec)

# Ejemplo
index <- c("170150254001002")

i = 1

# perfil <- read_sf(paste0("intermedios/01_preparacion_validacion/", substr(index[i], 1, 2),
#                          "/", substr(index[i], 1, 6), "/zona.gpkg")) %>% 
#   filter(zon == substr(index[i], 1, 9))

# Se carga el shape de manzanas extendidas de la parroquia a la que pertenece la super manzana

man_ext <- read_sf(paste0("productos/01_preparacion_validacion/", substr(index[i], 1, 2),
                          "/", substr(index[i], 1, 6), "/manzanas_extendidas.gpkg"))

# Se carga el shape de manzanas sin extender
manzanas <- read_sf(paste0("intermedios/01_preparacion_validacion/", substr(index[i], 1, 2),
                          "/", substr(index[i], 1, 6), "/manzana.gpkg"))

# Se identifica la súper manzana
man_partida <- man_ext %>% 
  filter(man == index[i])

# Se genera la envolvente convexa de la súper manzan
man_convex <- st_convex_hull(man_partida)

# Se determinan las manzanas cotenidas en la envolvente convexa
auxiliar_contenido <- man_convex %>% 
  st_join(manzanas, join = st_contains) %>% 
  as.data.frame() %>% 
  select(man = man.y) 

man_contenida <- man_ext %>% filter(man %in% auxiliar_contenido$man, man != index[i])

# Se determinan manzanas conexas del perfil
auxiliar_perfil <-  man_ext %>% 
  filter(man != index[i]) %>% 
  mutate(perimetro = as.numeric(sf::st_perimeter(.))) %>% 
  st_intersection(man_ext %>% 
                    filter(man == index[i])) %>% 
  st_collection_extract("LINESTRING") %>% 
  mutate(largo = as.numeric(st_length(.)),
         prop = largo/perimetro) %>% 
  as.data.frame() %>% 
  select(-geom) %>% 
  filter(prop > 0.5)

man_perfil <- man_ext %>% 
  filter(man %in% auxiliar_perfil$man, !man %in% man_contenida$man)


# Se determina el conjunto de todas las manzanas a partir
conglomerado <- rbind(man_contenida, man_perfil, man_partida)

# Generamos el árbol de calles
arbol <- conglomerado %>% 
  summarise() %>% 
  st_difference(manzanas %>% 
                  filter(man %in% conglomerado$man) %>% 
                  summarise()) 

# Se genera la raíz del arbol
raiz <- conglomerado %>% 
  summarise() %>% 
  st_cast("LINESTRING") %>% 
  ungroup() %>% 
  mutate(largo = st_length(.)) %>% 
  filter(largo == max(largo)) %>% 
  st_buffer(20)

# Se identifican las ramas del árbol
ramas <- st_difference(arbol, raiz) %>% 
  st_cast("POLYGON") %>% 
  mutate(grupo = row_number(.))

# se crea el ca04 de los edificios en conjunto
ca04_conjunto <- ca04 %>% 
  filter(man %in% conglomerado$man) %>% 
  mutate(edif_cod = substr(viv_censo, 1, 18)) %>% 
  filter(edif_cod %in% edificio_conjunto$edif_cod) %>% 
  select(edif_cod)

# se asocia los ingresos a los edificios en conjunto
ingresos_edificio_conjunto <- ingresos %>% 
  filter(man %in% conglomerado$man) %>% 
  filter(tipo == "ENTRADA") %>% 
  st_centroid() %>% 
  st_join(ca04_conjunto, join = st_within) %>% 
  filter(!is.na(edif_cod)) %>% 
  filter(!duplicated(edif_cod))

# se asocia cada ingreso (de edificios en conjunto) a una rama
ingreso_conjunto_rama <- ingresos_edificio_conjunto %>% 
  st_join(ramas, join = st_nearest_feature) %>% 
  as.data.frame() %>% 
  select(edif_cod, grupo)

# se crea el ca04 de los edificios que no están en conjunto
ca04_no_conjunto <- ca04 %>% 
  filter(man %in% conglomerado$man) %>% 
  mutate(edif_cod = substr(viv_censo, 1, 18)) %>% 
  filter(!edif_cod %in% edificio_conjunto$edif_cod) %>% 
  select(edif_cod)

# se asocia los ingresos a los edificios que no están en conjunto
ingresos_edificio_no_conjunto <- ingresos %>% 
  filter(man %in% conglomerado$man) %>% 
  filter(tipo == "ENTRADA") %>% 
  st_centroid() %>% 
  st_join(ca04_no_conjunto, join = st_within) %>% 
  filter(!is.na(edif_cod)) %>% 
  filter(!duplicated(edif_cod))

# se asocia cada ingreso (de edificios no en conjunto) a una rama
ingreso_no_conjunto_rama <- ingresos_edificio_no_conjunto %>% 
  st_join(ramas %>% 
            filter(!grupo %in% ingreso_conjunto_rama$grupo), 
          join = st_nearest_feature) %>% 
  as.data.frame() %>% 
  select(edif_cod, grupo)

distancia <- as.numeric(st_distance(ingresos_edificio_no_conjunto, 
                         ramas[ingreso_no_conjunto_rama$grupo,], by_element = T))

ingreso_no_conjunto_rama_01 <- ingreso_no_conjunto_rama %>% 
  cbind(distancia) %>% 
  filter(distancia < 10)



ca04_conglomerado <- ca04 %>% 
  filter(man %in% conglomerado$man) %>% 
  mutate(edif_cod = substr(viv_censo, 1, 18)) %>% 
  left_join(ingreso_conjunto_rama %>% 
              rbind(ingreso_no_conjunto_rama_01 %>% select(edif_cod, grupo)), 
            by = "edif_cod") %>% 
  mutate(grupo = ifelse(is.na(grupo), 0, grupo))

write_sf(ca04_conglomerado, "ejemplo.gpkg")



















plot(man_contenida)
ingresos_man <- ingresos %>%
  filter(man == index[i])

donas <- st_difference(perfil, man_par) %>% 
  mutate(area = as.numeric(st_area(.)))


ejes_man <- st_intersects(ejes, st_buffer(man_ext, 10))


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
