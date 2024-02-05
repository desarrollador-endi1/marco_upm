rm(list = ls())

library(tidyverse)

man_sec_upm <- readRDS("productos/02_conglomeracion/manzanas_sectores_upm.rds")

li = 60

lol <- man_sec_upm %>% 
  mutate(zon = ifelse(as.numeric(substr(man_sec, 7, 9)) < 900,paste0(substr(man_sec, 1,6), "000"), substr(man_sec, 1, 9))) %>% 
  filter(substr(id_upm_60, 7, 7) == "9") %>% 
  group_by(zon) %>% 
  mutate(n_man_sec = n()) %>% 
  ungroup() %>% 
  group_by(id_upm_60, zon, n_man_sec) %>% 
  summarise(viv = sum(viv_ocu_pre),
            n_man_sec_upm = n()) %>% 
  ungroup() %>% 
  group_by(id_upm_60) %>% 
  mutate(viv_upm = sum(viv)) %>% 
  ungroup() %>% 
  filter(viv_upm < li, n_man_sec != n_man_sec_upm)

