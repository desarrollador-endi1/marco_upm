rm(list = ls())

library(tidyverse)

bdd <- readRDS("insumos/01_preparacion_validacion/precenso_20221207.rds")

bdd_01 <- bdd %>% 
  mutate(pro = str_pad(as.character(pro), 2, "left", "0"),
         can = str_pad(as.character(can), 2, "left", "0"),
         par = str_pad(as.character(par), 2, "left", "0"),
         zon = str_pad(as.character(zon), 3, "left", "0"),
         sec = str_pad(as.character(sec), 3, "left", "0"),
         man = str_pad(as.character(man), 3, "left", "0"),
         n_loc = str_pad(as.character(n_loc), 3, "left", "0"),
         n_edif = str_pad(as.character(n_edif), 3, "left", "0"),
         id_edif = case_when(zon == "999" ~ paste0(pro, can, par, zon, sec, n_loc, n_edif),
                            zon != "999" ~ paste0(pro, can, par, zon, sec, man, n_edif),
                             T ~ "lol"),
         mansec = ifelse(zon == "999", paste0(pro, can, par, zon, sec), paste0(pro, can, par, zon, sec, man))) %>%
  group_by(mansec, id_edif) %>% 
  summarise(viv_tot = n(),
            viv_ocu = sum(c_ocup == "Particular - Ocupada"),
            viv_par = sum(c_ocup %in% c("Particular - Desocupada",  "Particular - En Construcción","Particular - Ocupada", "Particular - Temporal"))) %>% 
  ungroup()


saveRDS(bdd_01, "intermedios/01_preparacion_validacion/precenso_edificios.rds")

# lala <- precenso_edificios %>%
#   mutate(parr = substr(id_edif, 1, 6)) %>% 
#   group_by(parr) %>% 
#   summarise(n = n())
# 
# lala1 <- precenso_edificios %>%
#   filter(substr(id_edif, 7, 9)!="999") %>% 
#   mutate(parr = substr(id_edif, 1, 6)) %>% 
#   group_by(parr) %>% 
#   summarise(n = n())
# 
# lala$parr[!lala$parr %in% lala1$parr]
