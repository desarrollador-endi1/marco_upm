rm(list = ls())

library(tidyverse)
library(sf)

li <- 60

manzanas_sectores_upm <- readRDS("productos/02_conglomeracion/manzanas_sectores_upm.rds")

load("intermedios/02_conglomeracion/revisar.RData")

#index <- unique(substr(revisar$id_upm, 1, 6))

index <- unique(substr(revisar_01$id_upm, 1, 6))

#####

i = i+1

sectores <- read_sf(paste0("intermedios/01_preparacion_validacion/",
                           substr(index[i], 1, 2), "/", index[i],
                           "/sector.gpkg")) %>% 
  filter(substr(sec, 7, 9) == "999")

# manzanas <- read_sf(paste0("productos/01_preparacion_validacion/",
#                            substr(index[i], 1, 2), "/", index[i],
#                            "/manzanas_extendidas.gpkg"))
# 
# manzanas_se <- read_sf(paste0("intermedios/01_preparacion_validacion/",
#                            substr(index[i], 1, 2), "/", index[i],
#                            "/manzana.gpkg"))
# 
# 
# lala <- manzanas %>%
# left_join(manzanas_sectores_upm %>%
#             select(man = man_sec, id_upm = paste0("id_upm_", li)), by = "man") %>%



lala <- sectores %>% 
  left_join(manzanas_sectores_upm %>%
              select(sec = man_sec, id_upm = paste0("id_upm_", li)), by = "sec") %>%
  filter(id_upm %in% revisar$id_upm) %>% 
  group_by(id_upm) %>%
  summarise()

dim(lala)[1]

write_sf(lala, "upm.gpkg")
#write_sf(manzanas, "manzanas_ext.gpkg")
write_sf(sectores, "manzanas.gpkg")

revisar$id_upm[substr(revisar$id_upm, 1, 6) == index[i]]



#####

revisar_01 <- revisar %>% 
  mutate(obs = case_when(id_upm == "0113510001" ~ "ahí queda",
                         id_upm == "0304500164" ~ "ahí queda",
                         id_upm == "0501610010" ~ "ahí queda",
                         id_upm == "0706530002" ~ "ahí queda",
                         id_upm == "0709500002" ~ "ahí queda",
                         id_upm == "0709500027" ~ "ahí queda",
                         id_upm == "0712540002" ~ "ahí queda",
                         id_upm == "0801650003" ~ "ahí queda",
                         id_upm == "0801650005" ~ "ahí queda",
                         id_upm == "0803500007" ~ "ahí queda",
                         id_upm == "0803580011" ~ "ahí queda",
                         id_upm == "0901504675" ~ "ahí queda",
                         id_upm == "0901504903" ~ "ahí queda",
                         id_upm == "0904500020" ~ "ahí queda",
                         id_upm == "0906530013" ~ "división upm",
                         id_upm == "1004500175" ~ "cambio manzana",
                         id_upm == "1113560001" ~ "ahí queda",
                         id_upm == "1201530012" ~ "ahí queda",
                         id_upm == "1401510001" ~ "ahí queda",
                         id_upm == "1601610001" ~ "ahí queda",
                         id_upm == "1701500539" ~ "cambio manzana",
                         id_upm == "1701503189" ~ "ahí queda",
                         id_upm == "1904520001" ~ "ahí queda",
                         id_upm == "1907500003" ~ "ahí queda",
                         id_upm == "2003500048" ~ "ahí queda",
                         id_upm == "2103500003" ~ "ahí queda",
                         id_upm == "2204520002" ~ "ahí queda",
                         id_upm == "2301501178" ~ "cambio manzana",
                         id_upm == "2401500024" ~ "ahí queda",
                         id_upm == "0709539001" ~ "ahí queda",
                         id_upm == "0709539002" ~ "ahí queda",
                         id_upm == "0801509002" ~ "ahí queda",
                         id_upm == "0901539003" ~ "ahí queda",
                         id_upm == "0906509052" ~ "ahí queda",
                         id_upm == "1002509006" ~ "ahí queda",
                         id_upm == "1004509011" ~ "ahí queda",
                         id_upm == "1004509012" ~ "ahí queda",
                         id_upm == "1004529003" ~ "ahí queda",
                         id_upm == "1308509001" ~ "ahí queda",
                         id_upm == "1308509002" ~ "ahí queda",
                         id_upm == "1308509003" ~ "ahí queda",
                         id_upm == "1701509017" ~ "ahí queda",
                         id_upm == "1701509018" ~ "ahí queda",
                         id_upm == "1701509027" ~ "ahí queda",
                         id_upm == "1701529008" ~ "ahí queda",
                         id_upm == "1701639019" ~ "ahí queda",
                         id_upm == "1701659002" ~ "ahí queda",
                         id_upm == "1701759001" ~ "ahí queda",
                         id_upm == "1702509011" ~ "ahí queda",
                         id_upm == "1703539001" ~ "ahí queda",
                         T ~ "no se revisó"))

manzanas_sectores_upm_01 <- manzanas_sectores_upm %>% 
  mutate(id_upm_60 = case_when(man_sec %in% c("090653910001007", "090653910001008", "090653910001009",
                                              "090653910001010", "090653910001011") ~ "0906530014",
                               man_sec == "100450012003009" ~ "1004500176",
                               man_sec == "170150050004003" ~ "1701500552",
                               man_sec %in% c("230150086001030", "230150086001031", "230150086001032",
                                              "230150086001033", "230150086001034", "230150086001035",
                                              "230150086001036", "230150086001037", 
                                              "230150086001038") ~ "2301501156",
                               T ~ id_upm_60))

saveRDS(manzanas_sectores_upm_01 %>% 
          select(man_sec, id_upm = id_upm_60, viv_ocu = viv_ocu_pre),
        "productos/02_conglomeracion/manzanas_sectores_upm_final.rds")


resumen_upm <- manzanas_sectores_upm_01 %>% 
  mutate(id_zon = case_when(substr(man_sec, 7, 7) == "9" ~ substr(man_sec, 1, 9),
                            T ~ paste0(substr(man_sec, 1, 6), "000"))) %>% 
  group_by(id_zon) %>% 
  mutate(n_man_sec_zon = n()) %>% 
  ungroup() %>% 
  group_by(id_upm = id_upm_60, id_zon, n_man_sec_zon) %>% 
  summarise(viv = sum(viv_ocu_pre),
            n_man_upm = n()) %>% 
  ungroup() %>% 
  group_by(id_upm) %>% 
  mutate(n_partes_upm = n()) 
