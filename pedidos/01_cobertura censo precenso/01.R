#
rm(list = ls())
# Cargamos las librerías a usar
library(readr)
library(openxlsx)
library(tidyverse)
#
# Cargamos la base con el número de viviendas por edificio
precenso <- read_delim("D:/AG/Cartografía/cartografiaecuador/insumos/baseprecensal/BASE PRECENSAL/Base Precensal160323.txt_20230316_090503_0.csv",
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

precenso <- precenso %>% 
  filter(!is.na(c_ocup)) %>% 
  group_by(prov = pro, canton = can) %>% 
  summarise(viv_tot_pre = sum(c_ocup != "Colectiva"),
            viv_ocu_pre = sum(c_ocup == "Particular - Ocupada"),
            per_pre = sum(n_hbt, na.rm = T))


censo <- vivienda2022_dinem_3011 %>% 
  select(prov = I01, canton = I02, con_ocu_viv_par = V0201, con_ocu_viv_col = V0202, totper_censo = TOTPER) %>% 
  filter(!is.na(con_ocu_viv_par)) %>% 
  group_by(prov, canton) %>% 
  summarise(viv_tot_censo = n(),
            viv_ocu_censo = sum(con_ocu_viv_par %in% c(1, 2)),
            per_censo = sum(totper_censo, na.rm = T))

r1 <- precenso %>% 
  full_join(censo, by = c("prov", "canton"))

colSums(r1[,3:8])

##### Guardado #####

wb <- createWorkbook("Cobertura precenso censo")

addWorksheet(wb, "Cobertura")
writeData(wb, sheet = "Cobertura", r1)

saveWorkbook(wb, 
             "pedidos/01_cobertura censo precenso/resultado.xlsx", 
             overwrite = T)


