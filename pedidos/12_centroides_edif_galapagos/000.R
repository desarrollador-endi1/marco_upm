#
rm(list = ls())
library(tidyverse)
library(sf)
library(foreach)
library(doParallel)

edif <- read_sf("D:/MAG/cartografia/insumos/precenso_2022/BNCPV22.gpkg", layer = "edif_p")

edif_gal <- edif |> 
  mutate(id_edif = ifelse(is.na(man), paste0(loc, str_pad(n_edif, 3, "left", "0")),
                          paste0(man, str_pad(n_edif, 3, "left", "0")))) |> 
  filter(substr(id_edif, 1, 2) == "20")

coordenadas <- edif_gal|> 
  st_coordinates() |> 
  cbind(id_edif = edif_gal$id_edif) |>
  data.frame() |> 
  group_by(id_edif) |> 
  summarise(x = first(X),
            y = first(Y))

saveRDS(coordenadas, "pedidos/12_centroides_edif_galapagos/centroides_edificios_galapagos.rds")
