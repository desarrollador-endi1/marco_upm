rm(list = ls())

library(tidyverse)
library(sf)

shape_upm <- read_sf("productos/05_limites_upm/upm.gpkg")

man_sec_upm <- readRDS("D:/MAG/marco_administracion/insumos/01_general/man_sec_upm.rds") |> 
  filter(man_sec %in% c("180165003003003", "180165003003005", "180165003003004",
                        "180165003001008", "180165003002008", "180165003002007",
                        "180165003001007", "180165003002009", "180165003005005",
                        "180165003005001", "180165003005002", "180165003005003",
                        "180165003003008", "180165003005004", "180165003003006",
                        "180165003003007", "180165999027", "180165999028",
                        "180165999024", "180165999025", "180165999026",
                        "180165999029"))

upm_piloto <- shape_upm |> 
  filter(id_upm %in% substr(man_sec_upm$id_upm, 1, 10))

manzanas_piloto <- read_sf("intermedios/01_preparacion_validacion/18/180165/manzana.gpkg") |> 
  filter(man %in% c("180165003003003", "180165003003005", "180165003003004",
                    "180165003001008", "180165003002008", "180165003002007",
                    "180165003001007", "180165003002009", "180165003005005",
                    "180165003005001", "180165003005002", "180165003005003",
                    "180165003003008", "180165003005004", "180165003003006",
                    "180165003003007", "180165999027"))
sectores_piloto <- read_sf("intermedios/01_preparacion_validacion/18/180165/sector.gpkg") |> 
  filter(sec %in% c("180165999027", "180165999028", "180165999029",
                    "180165999024", "180165999025", "180165999026"))

write_sf(upm_piloto, "pedidos/13_piloto_enciet/upm.gpkg")
write_sf(manzanas_piloto, "pedidos/13_piloto_enciet/manzanas_piloto.gpkg")
write_sf(sectores_piloto, "pedidos/13_piloto_enciet/sectores_pilot.gpkg")
