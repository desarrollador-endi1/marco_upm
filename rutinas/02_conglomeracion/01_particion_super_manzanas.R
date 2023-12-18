rm(list = ls())
# Cargamos las librerías a usar
library(tidyverse)
library(sf)

li = 100

peso_edif <- readRDS("intermedios/01_preparacion_validacion/precenso_edificios.rds") %>% 
  select(mansec, id_edif, viv = viv_ocu) %>% 
  filter(substr(mansec, 7, 9) != "999")

pesos <- peso_edif %>% 
  group_by(mansec) %>% 
  summarise(viv = sum(viv)) %>% 
  ungroup()

particion <- peso_edif %>% 
  filter(mansec %in% unique(pesos$mansec[pesos$viv >=2*li])) %>% 
  arrange(id_edif) %>% 
  group_by(mansec) %>% 
  mutate(freqa = cumsum(viv),
         ng = floor((sum(viv))/(1.5*li)),
         tam = 1 +ceiling(sum(viv)/ng),
         grupo = 1 + floor(freqa/tam),
         viv_man = sum(viv)) %>% 
  ungroup() %>% 
  group_by(mansec, grupo) %>% 
  mutate(viv_grupo = sum(viv)) %>% 
  ungroup()

control <- particion %>% 
  group_by(mansec, grupo) %>% 
  summarise(viv = sum(viv))

if(li == 60){
  auxiliar <- particion %>% 
    filter(mansec %in% control$mansec[control$viv < li]) %>% 
    mutate(grupo1 = case_when(id_edif %in% c("170150138004001060", "170150138004001061") ~ 2,
                              id_edif %in% c("170150144007001002") ~ 1,
                              id_edif %in% c("170150152005001016") ~ 1,
                              id_edif %in% c("170150154012001012") ~ 1,
                              mansec == "170150154012001" & grupo > 1 & id_edif != "170150154012001012" ~ grupo - 1, 
                              id_edif %in% c("170150169004001004") ~ 2,
                              mansec == "170150169004001" ~ 1,
                              id_edif %in% c("170150174001001075") ~ 3,
                              id_edif %in% c("170150174002001011") ~ 1,
                              id_edif %in% c("170150174004001021") ~ 2,
                              id_edif %in% c("170150174004001022") ~ 3,
                              mansec == "170150174004001" ~ 1,
                              id_edif %in% c("170150175009001008") ~ 1,
                              id_edif %in% c("170150176011001003") ~ 1,
                              mansec == "170150176011001" & grupo > 2 ~ grupo - 1,
                              mansec == "170150278010001" & grupo > 1 ~ grupo - 1,
                              T ~ grupo
    ))
}
if(li == 80){
  auxiliar <- particion %>% 
    filter(mansec %in% control$mansec[control$viv < li]) %>% 
    mutate(grupo1 = case_when(id_edif %in% c("170150176011001003") ~ 1,
                              T ~ grupo
    ))
}
if(li == 100){
  auxiliar <- particion %>% 
    filter(mansec %in% control$mansec[control$viv < li]) %>% 
    mutate(grupo1 = grupo)
}


control_aux <- auxiliar %>% 
  group_by(mansec, grupo1) %>% 
  summarise(viv = sum(viv))

particion_01 <- rbind(particion %>% 
                        select(mansec, id_edif, viv, grupo) %>% 
                        filter(!mansec %in% auxiliar$mansec),
                      auxiliar %>% 
                        select(mansec, id_edif, viv, grupo = grupo1))

control_01 <- particion_01 %>% 
  group_by(mansec, grupo) %>% 
  summarise(viv = sum(viv))

saveRDS(particion_01, 
        paste0("intermedios/02_conglomeracion/particion_manzanas_li_", li, ".rds"))

