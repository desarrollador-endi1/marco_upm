rm(list = ls())

library(tidyverse)
library(openxlsx)

mansec_upm <- readRDS("productos/02_conglomeracion/manzanas_sectores_upm_final.rds")

n_upm_prov <- mansec_upm %>% 
  mutate(dominio = case_when(substr(id_upm, 1, 4) == "1701" ~ "25",
                             substr(id_upm, 1, 4) %in% c("0901", "0907", "0917") ~ "26",
                             T ~ substr(id_upm, 1, 2))) %>% 
  group_by(dominio, id_upm) %>% 
  summarise() %>% 
  group_by(dominio) %>% 
  summarise(n = n())

write.xlsx(n_upm_prov, "pedidos/08_n_upm_dominio/n_upm_dominio.xlsx")

upm_ciu <- readRDS("D:/MAG/marco_upm/pedidos/04_pareto/upm_ciu.rds")