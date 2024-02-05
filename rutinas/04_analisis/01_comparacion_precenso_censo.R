#
rm(list = ls())
#
library(readr)
library(tidyverse)
#
# Cargamos la base de manzanas, sectores y upm 60, 80 y 100

man_sec_upm <- readRDS("productos/02_conglomeracion/manzanas_sectores_upm.rds")

viv_2022 <- read_delim("insumos/censo/viv_2022.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

censo <- viv_2022 %>% 
  select(prov = I01, canton = I02, parr = I03, zona = I04, sector = I05, man = I06,
         con_ocu_viv_par = V0201, con_ocu_viv_col = V0202, totper_censo = TOTPER) %>% 
  filter(!is.na(con_ocu_viv_par)) %>% 
  mutate(man_sec = ifelse(zona == "999", paste0(prov, canton, parr, zona, sector),
                          paste0(prov, canton, parr, zona, sector, man))) %>% 
  group_by(man_sec) %>% 
  summarise(viv_tot_censo = n(),
            viv_ocu_censo = sum(con_ocu_viv_par %in% c(1, 2)),
            per_censo = sum(totper_censo, na.rm = T))

c1 <- man_sec_upm %>% 
  full_join(censo, by = "man_sec")

dim(man_sec_upm)[1] == dim(c1)[1]

colSums(c1[,5:7])
colSums(c1[,8:10], na.rm = T)



