#
rm(list = ls())
#
library(readr)
library(tidyverse)

peso_edif <- readRDS("intermedios/01_preparacion_validacion/precenso_edificios.rds")

pesos <- peso_edif %>% 
  group_by(mansec) %>% 
  summarise(viv = sum(viv_ocu)) %>% 
  ungroup() %>% 
  filter(substr(mansec, 1, 6) %in% c("170150", "170180"))

sec <- readRDS("pedidos/09_conglomeracion_dmq/intermedios/02_conglomeracion/sectores_conglomerados_60.rds") %>% 
  rename(man_sec = id_sec, id_conglomerado = id_con)

con60 <- readRDS("pedidos/09_conglomeracion_dmq/intermedios/02_conglomeracion/manzanas_conglomerados_60.rds") %>% 
  rename(man_sec = man) %>% 
  rbind(sec) %>% 
  rename(id_conglomerado_60 = id_conglomerado, viv60 = viv)

n_distinct(con60$man_sec)

man_upm <- pesos %>% 
  rename(man_sec = mansec) %>% 
  full_join(con60, by = "man_sec") 

identical(man_upm$viv, man_upm$viv60)

# correccion manzanas aisladas por estar completamente contenidas en rios

# man_sec           man_sec_cercana
# 080159001001006   080159001001005
# 080159001001007   080159001001008
# 080159902002010   080159902002011
# 160451001001010   160451001001009    
# 190551001001010   190551001001002

man_upm1 <- man_upm %>% 
  filter(man_sec %in% c()) %>% 
  mutate(man_sec_perdida = "") %>% 
  select(man_sec = man_sec_perdida, c60 = id_conglomerado_60) %>% 
  full_join(man_upm, by = "man_sec") %>% 
  # identificacion de UPM
  mutate(id_conglomerado_60 = ifelse(is.na(id_conglomerado_60), c60, id_conglomerado_60),
         viv60 = ifelse(is.na(viv60), viv_ocu_pre, viv60)) %>% 
  select(-c60)

identical(man_upm1$viv, man_upm1$viv60)

##### Comprobaciones tamaño UPM 60 80 y 100 #####

r60 <- man_upm1 %>% 
  group_by(id_conglomerado_60) %>% 
  summarise(viv_precenso = sum(viv, na.rm = T),
            viv_upm = sum(viv60, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(control = viv_precenso - viv_upm,
         L = ifelse(viv_upm <60, 1, 0))

table(r60$control, useNA = "ifany")
hist(r60$viv_upm, breaks = 100)
table(r60$L, useNA = "ifany") #0.7

summary(r60$viv_upm)
quantile(r60$viv_upm, 0.005)  

##### Guardado #####

man_upm2 <- man_upm1 %>% 
  select(man_sec, id_upm_60 = id_conglomerado_60, viv, cod_adz)

saveRDS(man_upm2, "pedidos/09_conglomeracion_dmq/productos/02_conglomeracion/manzanas_sectores_upm.rds")





