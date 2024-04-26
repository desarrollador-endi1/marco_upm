rm(list = ls())

library(tidyverse)
library(openxlsx)
library(srvyr)
library(TeachingSampling)


tamanio <- read.xlsx("pedidos/04_pareto/escenario_03_muestra_endi_enighur.xlsx") %>% 
  select(dominio = estrato, n_upm_enlistar, n_upm_enighur, n_upm_endi)

manzanas_sectores_upm_final <- readRDS("productos/02_conglomeracion/manzanas_sectores_upm_final.rds")

particion_manzanas_li_60 <- readRDS("intermedios/02_conglomeracion/particion_manzanas_li_60.rds")

upm_ciu <- readRDS("pedidos/04_pareto/upm_ciu.rds") %>% 
  rbind(data.frame(cbind(id_upm = "1601590001", 
                         provincia = "16", 
                         dominio = "16", 
                         area = 2,
                         est_se = 1, 
                         estrato = "1621")),
        data.frame(cbind(id_upm = "0805559001", 
                         provincia = "08", 
                         dominio = "08",
                         area = 2,
                         est_se = 2, 
                         estrato = "0822"))) 

control <- upm_ciu %>%
  group_by(provincia, dominio, area = substr(estrato, 3, 3), estrato) %>%
  summarise(n_estrato = n()) %>%
  group_by(dominio, area) %>%
  mutate(prop = n_estrato/sum(n_estrato),
         n_dominio = sum(n_estrato)) %>%
  ungroup() %>% 
  mutate(control_estrato = case_when(n_estrato < 100 ~ 1,
                                     T ~ 0),
         control_dominio = case_when(n_dominio < 200 ~ 1,
                                     T ~ 0))

apoyo <- manzanas_sectores_upm_final %>% 
  rename(mansec = man_sec) %>% 
  left_join(particion_manzanas_li_60 %>% 
              group_by(mansec, grupo) %>% 
              summarise() %>% 
              ungroup(), by = "mansec") %>% 
  mutate(grupo = ifelse(is.na(grupo), 1, grupo),
         grupo = str_pad(grupo, 2, "left", "0"),
         id_upm_sub = paste0(id_upm, grupo)) %>% 
  left_join(upm_ciu, by = "id_upm") %>%
  select(-id_upm) %>% 
  group_by(id_upm = id_upm_sub, provincia, dominio, area, est_se, estrato) %>% 
  summarise(Mi = sum(viv_ocu)) %>% 
  ungroup() %>% 
  group_by(dominio, area) %>% 
  mutate(Nda = n()) %>% 
  ungroup() %>% 
  mutate(est_se = case_when(substr(estrato, 1, 3) %in% 
                              c("151", "161", "162", "191", "192", "201", "202", "242", "152") ~ "9",
                            T ~ est_se),
         estrato = case_when(substr(estrato, 1, 3) %in% 
                               c("151", "161", "162", "191", "192", "201", "202", "242", "152") ~ 
                               paste0(substr(estrato, 1, 3), "9"),
                             T ~ estrato),
         # luego de revisar la información en control
         estrato = case_when(estrato == "0823" ~ "0822",
                             estrato == "0421" ~ "0422",
                             T ~ estrato))

set.seed(20240416)

marco_ran_uni <- apoyo %>% 
  group_by(estrato) %>% 
  mutate(Mh = sum(Mi)) %>% 
  ungroup() %>% 
  mutate(nap = sample(1:10000000, 56252)/10000000)


distribucion_endi <- marco_ran_uni %>% 
  group_by(dominio, estrato) %>% 
  summarise(Nh = n(),
            Mh = first(Mh)) %>% 
  full_join(tamanio, by = "dominio") %>% 
  ungroup() %>% 
  group_by(dominio) %>% 
  mutate(nh_endi1 = case_when(floor(n_upm_endi * Mh/sum(Mh)) < 4 ~ 4,
                              T ~ floor(n_upm_endi * Mh/sum(Mh))),
         resto = n_upm_endi - sum(nh_endi1),
         diff = case_when(floor(n_upm_endi * Mh/sum(Mh)) < 4 ~ 0,
                          T ~ n_upm_endi * Mh/sum(Mh) - floor(n_upm_endi * Mh/sum(Mh))),
         aux = n_upm_endi * Mh/sum(Mh)) %>% 
  arrange(dominio, desc(diff)) %>% 
  group_by(dominio) %>% 
  mutate(orden1 = row_number()) %>% 
  ungroup() %>% 
  mutate(extra = ifelse(orden1 <= resto, 1, 0)) %>% 
  arrange(dominio, desc(nh_endi1)) %>% 
  group_by(dominio) %>% 
  mutate(orden2 = row_number()) %>% 
  ungroup() %>% 
  mutate(exceso = case_when(-orden2 == resto ~ -1,
                            T ~ 0),
         nh_endi = nh_endi1 + extra + exceso)
  

sum(distribucion_endi$nh_endi, na.rm = T)


distribucion_enighur <- marco_ran_uni %>% 
  group_by(dominio, estrato) %>% 
  summarise(Nh = n(),
            Mh = first(Mh)) %>% 
  full_join(tamanio, by = "dominio") %>% 
  ungroup() %>% 
  group_by(dominio) %>% 
  mutate(nh_enighur1 = case_when(floor(n_upm_enighur * Mh/sum(Mh)) < 4 ~ 4,
                              T ~ floor(n_upm_enighur * Mh/sum(Mh))),
         resto = n_upm_enighur - sum(nh_enighur1),
         diff = case_when(floor(n_upm_enighur * Mh/sum(Mh)) < 4 ~ 0,
                          T ~ n_upm_enighur * Mh/sum(Mh) - floor(n_upm_enighur * Mh/sum(Mh))),
         aux = n_upm_enighur * Mh/sum(Mh)) %>% 
  arrange(dominio, desc(diff)) %>% 
  group_by(dominio) %>% 
  mutate(orden1 = row_number()) %>% 
  ungroup() %>% 
  mutate(extra = ifelse(orden1 <= resto, 1, 0)) %>% 
  arrange(dominio, desc(nh_enighur1)) %>% 
  group_by(dominio) %>% 
  mutate(orden2 = row_number()) %>% 
  ungroup() %>% 
  mutate(exceso = case_when(-orden2 >= resto ~ -1,
                            T ~ 0),
         nh_enighur = nh_enighur1 + extra + exceso)

sum(distribucion_enighur$nh_enighur, na.rm = T)

muestra <- distribucion_endi %>% 
  select(dominio, estrato, nh_endi) %>% 
  left_join(distribucion_enighur %>% 
              select(estrato, nh_enighur), by = "estrato") %>% 
  mutate(nh = pmax(nh_endi, nh_enighur, na.rm = T), 
         nhmin = pmin(nh_endi, nh_enighur, na.rm = T))

marco <- marco_ran_uni %>% 
  left_join(muestra %>% 
              select(estrato, nh, nh_endi, nh_enighur),
            by = "estrato") %>% 
  mutate(nh_endi = ifelse(is.na(nh_endi), 0, nh_endi)) %>% 
  group_by(estrato) %>% 
  mutate(pii_endi = PikPPS(nh_endi, Mi),
         pii_enighur = PikPPS(nh_enighur, Mi)) %>%
  ungroup() %>% 
  mutate(#pii_endi = nh_endi * Mi/Mh,
         pareto_endi = (nap/(1-nap))/(pii_endi/(1-pii_endi)),
         #pii_enighur = nh_enighur * Mi/Mh,
         pareto_enighur = (nap/(1-nap))/(pii_enighur/(1-pii_enighur))) %>% 
  arrange(estrato, pareto_endi) %>% 
  group_by(estrato) %>% 
  mutate(orden_endi = row_number()) %>%
  ungroup() %>% 
  arrange(estrato, pareto_enighur) %>% 
  group_by(estrato) %>% 
  mutate(orden_enighur = row_number()) %>%
  ungroup() %>% 
  mutate(sel_endi = ifelse(orden_endi <= nh_endi, 1, 0),
         sel_enighur = ifelse(orden_enighur <= nh_enighur, 1, 0))


table(marco$sel_endi, marco$sel_enighur)

plot(marco$sel_endi)

tam_enemdu <- readRDS("pedidos/04_pareto/tam_enemdu.rds") %>% 
  cbind(pro = c("01", "02", "03", "04", "05", "06", "07", "08", "20", 
                      "09", "10", "11", "12", "13", "14", "15", "22", "16", 
                      "17", "24", "23", "21", "18", "19")) %>% 
  select(pro, n_upm_enemdu = n)
  
distribucion_enemdu <- marco_ran_uni %>% 
  mutate(pro = substr(id_upm, 1, 2)) %>% 
  group_by(pro, dominio, estrato) %>% 
  summarise(Nh = n(),
            Mh = first(Mh)) %>% 
  full_join(tam_enemdu, by = "pro") %>% 
  ungroup() %>% 
  group_by(pro) %>% 
  mutate(nh_enemdu1 = case_when(floor(n_upm_enemdu * Mh/sum(Mh)) < 4 ~ 4,
                              T ~ floor(n_upm_enemdu * Mh/sum(Mh))),
         resto = n_upm_enemdu - sum(nh_enemdu1),
         diff = case_when(floor(n_upm_enemdu * Mh/sum(Mh)) < 4 ~ 0,
                          T ~ n_upm_enemdu * Mh/sum(Mh) - floor(n_upm_enemdu * Mh/sum(Mh))),
         aux = n_upm_enemdu * Mh/sum(Mh)) %>% 
  arrange(dominio, desc(diff)) %>% 
  group_by(dominio) %>% 
  mutate(orden1 = row_number()) %>% 
  ungroup() %>% 
  mutate(extra = ifelse(orden1 <= resto, 1, 0)) %>% 
  arrange(dominio, desc(nh_enemdu1)) %>% 
  group_by(dominio) %>% 
  mutate(orden2 = row_number()) %>% 
  ungroup() %>% 
  mutate(exceso = case_when(-orden2 == resto ~ -1,
                            T ~ 0),
         nh_enemdu = nh_enemdu1 + extra + exceso)

marco_01 <- marco %>% 
  mutate(seleccionado = case_when(sel_endi == 1 | sel_enighur == 1 ~ 1,
                                  T ~ 0)) %>% 
  left_join(distribucion_enemdu %>% 
              select(estrato, nh_enemdu), 
            by = "estrato") %>% 
  group_by(estrato) %>% 
  mutate(pii_enemdu = PikPPS(nh_enemdu, Mi)) %>%
  ungroup() %>% 
  mutate(pareto_enemdu = (nap/(1-nap))/(pii_enemdu/(1-pii_enemdu))) %>% 
  arrange(estrato, pareto_enemdu) %>% 
  group_by(estrato) %>% 
  mutate(orden_enemdu = row_number()) %>% 
  ungroup() %>% 
  group_by(estrato, seleccionado) %>% 
  mutate(max_sel = max(orden_enemdu)) %>% 
  ungroup() %>% 
  group_by(estrato) %>% 
  mutate(max_sel = min(max_sel),
         Nh = n()) %>% 
  ungroup() %>% 
  mutate(orden_enemdu1 = orden_enemdu - max_sel,
         orden_enemdu1 = ifelse(orden_enemdu1 <= 0, Nh + orden_enemdu1, orden_enemdu1),
         sel_enemdu = ifelse(orden_enemdu1 <= nh_enemdu, 1, 0))
  
  