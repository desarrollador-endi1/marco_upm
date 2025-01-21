rm(list = ls())

library(tidyverse)
library(sf)

am <- read_sf("intermedios/05_limites_upm/manzanas_consolidado.gpkg")
di <- read_sf("intermedios/05_limites_upm/sectores_consolidado.gpkg")

amdi <- di |> 
  filter(substr(sec, 7, 9) == "999") |> 
  select(man_sec = sec) |> 
  rbind(am |> 
          select(man_sec = man))

write_sf(amdi, "intermedios/05_limites_upm/man_sec_consolidado.gpkg")

man_sec_upm <- readRDS("productos/02_conglomeracion/man_sec_upm.rds")

manzanas_faltantes <- man_sec_upm$man_sec[!man_sec_upm$man_sec %in% amdi$man_sec]

manzanas <- read_sf("insumos/nogit/BNCPV22.gpkg", layer = "man_a")

man_sec_final <- manzanas |> 
  filter(man %in% manzanas_faltantes) |> 
  select(man_sec = man) |> 
  rbind(amdi)

upm <- man_sec_final |> 
  left_join(man_sec_upm, by = "man_sec") |> 
  group_by(id_upm) |> 
  summarise(viv = sum(viv = viv_ocu))

write_sf(upm, "productos/05_limites_upm/upm.gpkg")
