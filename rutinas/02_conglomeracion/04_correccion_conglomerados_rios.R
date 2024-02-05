
#
rm(list = ls())
#
library(readr)
library(tidyverse)
#
# Cargamos la base con el número de viviendas por edificio
precenso <- read_delim("D:/AG/Cartografía/cartografiaecuador/insumos/baseprecensal/BASE PRECENSAL/Base Precensal160323.txt_20230316_090503_0.csv",
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

pre_man <- precenso %>% 
  # filter(!is.na(c_ocup)) %>% 
  mutate(man_sec = ifelse(zon == "999", paste0(pro, can, par, zon, sec), paste0(pro, can, par, zon, sec, man))) %>% 
  group_by(man_sec) %>% 
  summarise(viv_tot_pre = sum(c_ocup != "Colectiva" & !is.na(c_ocup), na.rm = T),
            viv_ocu_pre = sum(c_ocup == "Particular - Ocupada", na.rm = T),
            per_pre = sum(n_hbt, na.rm = T))

sec <- readRDS("intermedios/02_conglomeracion/sectores_conglomerados_60.rds") %>% 
  rename(man_sec = id_sec, id_conglomerado = id_con)

con60 <- readRDS("intermedios/02_conglomeracion/manzanas_conglomerados_60.rds") %>% 
  rename(man_sec = man) %>% 
  rbind(sec) %>% 
  rename(id_conglomerado_60 = id_conglomerado, viv60 = viv)

con80 <- readRDS("intermedios/02_conglomeracion/manzanas_conglomerados_80.rds") %>% 
  rename(man_sec = man) %>% 
  rbind(sec) %>% 
  rename(id_conglomerado_80 = id_conglomerado, viv80 = viv)

con100 <- readRDS("intermedios/02_conglomeracion/manzanas_conglomerados_100.rds") %>% 
  rename(man_sec = man) %>% 
  rbind(sec) %>% 
  rename(id_conglomerado_100 = id_conglomerado, viv100 = viv)

n_distinct(con60$man_sec)
n_distinct(con80$man_sec)
n_distinct(con100$man_sec)

man_upm <- pre_man %>% 
  full_join(con60, by = "man_sec") %>% 
  full_join(con80, by = "man_sec") %>% 
  full_join(con100, by = "man_sec")

identical(man_upm$viv_ocu_pre, man_upm$viv60)
identical(man_upm$viv60, man_upm$viv80)
identical(man_upm$viv60, man_upm$viv100)
identical(man_upm$viv80, man_upm$viv100)

# correccion manzanas aisladas por estar completamente contenidas en rios

# man_sec           man_sec_cercana
# 080159001001006   080159001001005
# 080159001001007   080159001001008
# 080159902002010   080159902002011
# 160451001001010   160451001001009    
# 190551001001010   190551001001002

man_upm1 <- man_upm %>% 
  filter(man_sec %in% c("080159001001005", "080159001001008", "080159902002011",
                        "160451001001009", "190551001001002")) %>% 
  cbind(man_sec_perdida = c("080159001001006", "080159001001007",
                             "080159902002010","160451001001010",
                            "190551001001010")) %>% 
  select(man_sec = man_sec_perdida, c60 = id_conglomerado_60,
         c80 = id_conglomerado_80, c100 = id_conglomerado_100) %>% 
  full_join(man_upm, by = "man_sec") %>% 
  # identificacion de UPM
  mutate(id_conglomerado_60 = ifelse(is.na(id_conglomerado_60), c60, id_conglomerado_60),
         viv60 = ifelse(is.na(viv60), viv_ocu_pre, viv60),
         id_conglomerado_80 = ifelse(is.na(id_conglomerado_80), c80, id_conglomerado_80),
         viv80 = ifelse(is.na(viv80), viv_ocu_pre, viv80),
         id_conglomerado_100 = ifelse(is.na(id_conglomerado_100), c100, id_conglomerado_100),
         viv100 = ifelse(is.na(viv100), viv_ocu_pre, viv100)) %>% 
  select(-c60, -c80, -c100)

identical(man_upm1$viv_ocu_pre, man_upm1$viv60)
identical(man_upm1$viv60, man_upm1$viv80)
identical(man_upm1$viv60, man_upm1$viv100)
identical(man_upm1$viv80, man_upm1$viv100)

##### Comprobaciones tamaño UPM 60 80 y 100 #####

r60 <- man_upm1 %>% 
  group_by(id_conglomerado_60) %>% 
  summarise(viv_precenso = sum(viv_ocu_pre, na.rm = T),
            viv_upm = sum(viv60, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(control = viv_precenso - viv_upm,
         L = ifelse(viv_upm <60, 1, 0))

table(r60$control, useNA = "ifany")
hist(r60$viv_upm, breaks = 100)
table(r60$L, useNA = "ifany") #0.7

summary(r60$viv_upm)
quantile(r60$viv_upm, 0.005)  

r80 <- man_upm1 %>% 
  group_by(id_conglomerado_80) %>% 
  summarise(viv_precenso = sum(viv_ocu_pre, na.rm = T),
            viv_upm = sum(viv80, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(control = viv_precenso - viv_upm,
         L = ifelse(viv_upm <80, 1, 0))

table(r80$control, useNA = "ifany")
hist(r80$viv_upm, breaks = 100)
table(r80$L, useNA = "ifany") #1.5

summary(r80$viv_upm)
quantile(r80$viv_upm, 0.015)  

r100 <- man_upm1 %>% 
  group_by(id_conglomerado_100) %>% 
  summarise(viv_precenso = sum(viv_ocu_pre, na.rm = T),
            viv_upm = sum(viv100, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(control = viv_precenso - viv_upm,
         L = ifelse(viv_upm <100, 1, 0))

table(r100$control, useNA = "ifany")
hist(r100$viv_upm, breaks = 100)
table(r100$L, useNA = "ifany") #2%

summary(r100$viv_upm)
quantile(r100$viv_upm, 0.02)  

##### Guardado #####

man_upm2 <- man_upm1 %>% 
  select(man_sec, id_upm_60 = id_conglomerado_60, id_upm_80 = id_conglomerado_80,
         id_upm_100 = id_conglomerado_100, viv_tot_pre, viv_ocu_pre, per_pre)

saveRDS(man_upm2, "productos/02_conglomeracion/manzanas_sectores_upm.rds")





