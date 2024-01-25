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

viv_2022 <- read_delim("insumos/censo/viv_2022.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

pre <- precenso %>% 
  filter(!is.na(c_ocup)) %>% 
  group_by(prov = pro, canton = can) %>% 
  summarise(viv_tot_pre = sum(c_ocup != "Colectiva"),
            viv_ocu_pre = sum(c_ocup == "Particular - Ocupada"),
            per_pre = sum(n_hbt, na.rm = T))


censo <- viv_2022 %>% 
  select(prov = I01, canton = I02, con_ocu_viv_par = V0201, con_ocu_viv_col = V0202, totper_censo = TOTPER) %>% 
  filter(!is.na(con_ocu_viv_par)) %>% 
  group_by(prov, canton) %>% 
  summarise(viv_tot_censo = n(),
            viv_ocu_censo = sum(con_ocu_viv_par %in% c(1, 2)),
            per_censo = sum(totper_censo, na.rm = T))

r1 <- pre %>% 
  full_join(censo, by = c("prov", "canton"))

colSums(r1[,3:8])
colSums(b1[,2:7], na.rm = T)

##### Guardado #####

wb <- createWorkbook("Cobertura precenso censo")

addWorksheet(wb, "Cobertura")
writeData(wb, sheet = "Cobertura", r1)

saveWorkbook(wb, 
             "pedidos/01_cobertura censo precenso/resultado_24_01_15.xlsx", 
             overwrite = T)

##### Comparación a nivel de manzana #####

pre_man <- precenso %>% 
  # filter(!is.na(c_ocup)) %>% 
  mutate(man_sec = ifelse(zon == "999", paste0(pro, can, par, zon, sec), paste0(pro, can, par, zon, sec, man))) %>% 
  group_by(man_sec) %>% 
  summarise(viv_tot_pre = sum(c_ocup != "Colectiva" & !is.na(c_ocup), na.rm = T),
            viv_ocu_pre = sum(c_ocup == "Particular - Ocupada", na.rm = T),
            per_pre = sum(n_hbt, na.rm = T))

censo_man <- viv_2022 %>% 
  select(pro = I01, can = I02, par = I03, zon = I04, sec = I05, man = I06,
         con_ocu_viv_par = V0201, con_ocu_viv_col = V0202, totper_censo = TOTPER) %>% 
  # filter(!is.na(con_ocu_viv_par)) %>% 
  mutate(man_sec = ifelse(zon == "999", paste0(pro, can, par, zon, sec), paste0(pro, can, par, zon, sec, man))) %>% 
  group_by(man_sec) %>% 
  summarise(viv_tot_censo = sum(!is.na(con_ocu_viv_par), na.rm = T),
            viv_ocu_censo = sum(con_ocu_viv_par %in% c(1, 2), na.rm = T),
            per_censo = sum(totper_censo, na.rm = T))

b1 <- pre_man %>% 
  full_join(censo_man, by = "man_sec")

b2 <- pre_man %>% 
  left_join(censo_man, by = "man_sec")

colSums(r1[,3:8])
colSums(b1[,2:7], na.rm = T)




