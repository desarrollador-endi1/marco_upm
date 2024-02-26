rm(list = ls())

library(tidyverse)

# Abrir bases censo y guardado rds

viv_2022 <- read_delim("insumos/censo/viv_2022.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

base_viviendas_ocupadas <- viv_2022 %>%
  rename(prov = I01, canton = I02, parr = I03, zona = I04, sector = I05, man = I06,
         con_ocu_viv_par = V0201, con_ocu_viv_col = V0202, totper_censo = TOTPER) %>% 
  mutate(id_viv = paste0(prov, canton, parr, zona, sector, man, I07, I08, I10)) %>% 
  filter(con_ocu_viv_par %in% c(1, 2)) %>%
  select(id_viv)
    


per_2022 <- read_delim("insumos/censo/pob_2022.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

censo_per <- per_2022 %>% 
  rename(prov = I01, canton = I02, parr = I03, zona = I04, sector = I05, man = I06) %>% 
  mutate(id_viv = paste0(prov, canton, parr, zona, sector, man, I07, I08, I10),
         man_sec = ifelse(zona == "999", paste0(prov, canton, parr, zona, sector),
                          paste0(prov, canton, parr, zona, sector, man))) %>% 
  filter(id_viv %in% base_viviendas_ocupadas$id_viv) %>% 
  group_by(man_sec) %>% 
  summarise(per_pob = n())
  
censo_viv <- viv_2022 %>% 
  select(prov = I01, canton = I02, parr = I03, zona = I04, sector = I05, man = I06,
         con_ocu_viv_par = V0201, con_ocu_viv_col = V0202, totper_censo = TOTPER) %>% 
  mutate(man_sec = ifelse(zona == "999", paste0(prov, canton, parr, zona, sector),
                          paste0(prov, canton, parr, zona, sector, man))) %>% 
  group_by(man_sec) %>% 
  summarise(viv_ocu = sum(con_ocu_viv_par %in% c(1, 2)),
            per_viv = sum(totper_censo, na.rm = T)) %>% 
  full_join(censo_per, by = "man_sec")

saveRDS(censo_viv, "insumos/censo/viv_2022.rds")

manzanas_sectores_upm <- readRDS("productos/02_conglomeracion/manzanas_sectores_upm.rds")

control <- full_join(manzanas_sectores_upm, censo_viv, by = "man_sec") %>%
  select(-man_sec, -id_upm_80, -id_upm_100) %>% 
  group_by(id_upm_60) %>% 
  mutate(n_man_sec = n()) %>% 
  ungroup() %>% 
  group_by(id_upm_60) %>% 
  summarise_all(sum, na.rm = T) %>%
  ungroup() %>% 
  mutate(n_man_sec = sqrt(n_man_sec))

lol <- control %>% filter(viv_ocu < 60 | viv_ocu_pre< 60)

plot(lol$viv_ocu, lol$viv_ocu_pre)
abline(a = 0, b = 1)
