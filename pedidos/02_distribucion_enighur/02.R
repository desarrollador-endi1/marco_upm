rm(list = ls())

library(tidyverse)
library(readr)
library(openxlsx)

precenso <- readRDS("intermedios/01_preparacion_validacion/precenso_edificios.rds")

viv_2022 <- read_delim("insumos/censo/viv_2022.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

area <- viv_2022 %>% 
  mutate(paramadis = case_when(I04 == "999" ~ paste0(I01, I02, I03, "D"),
                               T ~ paste0(I01, I02, I03, "A"))) %>% 
  group_by(paramadis, area = NAURV) %>% 
  summarise()

control <- area %>% 
  group_by(paramadis) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  group_by(paramadis) %>% 
  summarise(area = min(area))

n_distinct(control$paramadis)

area_precenso <- precenso %>% 
  mutate(paramadis = case_when(substr(mansec, 7, 9) == "999" ~ paste0(substr(mansec, 1, 6), "D"),
                               T ~ paste0(substr(mansec, 1, 6), "A")),
         pro = case_when(substr(mansec, 1, 6) == "170150" & substr(mansec, 7, 9) != "999" ~ "25",
                         substr(mansec, 1, 6) == "090150" & substr(mansec, 7, 9) != "999" ~ "26",
                         substr(mansec, 1, 6) == "010150" & substr(mansec, 7, 9) != "999" ~ "27",
                         substr(mansec, 1, 6) == "070150" & substr(mansec, 7, 9) != "999" ~ "28",
                         substr(mansec, 1, 6) == "180150" & substr(mansec, 7, 9) != "999" ~ "29",
                         substr(mansec, 1, 6) == "080150" & substr(mansec, 7, 9) != "999" ~ "30",
                         substr(mansec, 1, 6) == "230150" & substr(mansec, 7, 9) != "999" ~ "31",
                         substr(mansec, 1, 6) == "130850" & substr(mansec, 7, 9) != "999" ~ "32",
                         substr(mansec, 1, 6) == "110150" & substr(mansec, 7, 9) != "999" ~ "33",
                         T ~ substr(mansec, 1, 2))) %>% 
  left_join(control, by = "paramadis") %>% 
  group_by(pro, area) %>% 
  summarise(viv = sum(viv_ocu))

muestra <- read.xlsx("pedidos/02_distribucion_enighur/Muestra Final.xlsx") 


envio <- distribucion %>% 
  select(dominio, pro, area, muestra_upm = muestra)

write.xlsx(envio, "pedidos/02_distribucion_enighur/distribucion_area_enighur.xlsx")
