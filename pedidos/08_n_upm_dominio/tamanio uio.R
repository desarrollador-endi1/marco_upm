rm(list = ls())

library(tidyverse)
library(openxlsx)
library(srvyr)
library(TeachingSampling)

tamanio <- read.xlsx("pedidos/04_pareto/escenario_03_muestra_endi_enighur.xlsx") %>% 
  select(dominio = estrato, n_upm_enlistar, n_upm_enighur, n_upm_endi)

upm_ciu <- readRDS("D:/MAG/marco_upm/pedidos/04_pareto/upm_ciu.rds") %>% 
  filter(dominio %in% c("17", "25")) %>% 
  mutate(domarea = case_when(substr(id_upm, 1, 6) == "170150" & area == 1 ~ "251",
                             substr(id_upm, 1, 4) == "1701" & area == 2 ~ "352",
                             substr(id_upm, 1, 4) == "1701" & area == 1 ~ "351",
                             area == 1 ~ "171",
                             area == 2 ~"172",
                             T ~ "LOL")) %>% 
  group_by(domarea) %>% 
  summarise(n_upm = n()) %>% 
  mutate(estrato1 = ifelse(domarea == "251", 1, 2)) %>% 
  group_by(estrato1) %>% 
  mutate(n_upm_estrato1 = sum(n_upm)) %>% 
  ungroup() %>% 
  mutate(dis_prov = case_when(domarea == "251" ~ 164,
                              T ~ ceiling(92 * n_upm / n_upm_estrato1))) %>% 
  mutate(estrato2 = ifelse(domarea %in% c("171", "172"), 1, 2)) %>% 
  group_by(estrato2) %>% 
  mutate(n_upm_estrato2 = sum(n_upm)) %>% 
  ungroup() %>% 
  mutate(dis_dmq = case_when(domarea %in% c("171", "172") ~ 0,
                              T ~ ceiling(250 * n_upm / n_upm_estrato2))) 
  
