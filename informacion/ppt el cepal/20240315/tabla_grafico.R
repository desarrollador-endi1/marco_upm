rm(list = ls())

library(tidyverse)
library(openxlsx)

mansec_upm_80_100 <- readRDS("D:/MAG/marco_upm/productos/02_conglomeracion/manzanas_sectores_upm.rds") %>% 
  filter(nchar(man_sec) == 15)

mansec_upm_60 <- readRDS("D:/MAG/marco_upm/productos/02_conglomeracion/manzanas_sectores_upm_final.rds") %>% 
  filter(nchar(man_sec) == 15)

particion_manzanas_li_60 <- readRDS("D:/MAG/marco_upm/intermedios/02_conglomeracion/particion_manzanas_li_60.rds") %>% 
  group_by(man_sec = mansec) %>% 
  summarise(grupo_60 = max(grupo))
particion_manzanas_li_80 <- readRDS("D:/MAG/marco_upm/intermedios/02_conglomeracion/particion_manzanas_li_80.rds") %>% 
  group_by(man_sec = mansec) %>% 
  summarise(grupo_80 = max(grupo))
particion_manzanas_li_100 <- readRDS("D:/MAG/marco_upm/intermedios/02_conglomeracion/particion_manzanas_li_100.rds") %>% 
  group_by(man_sec = mansec) %>% 
  summarise(grupo_100 = max(grupo))

resumen_upm_60 <- mansec_upm_80_100 %>% 
  select(-id_upm_60) %>% 
  left_join(mansec_upm_60 %>% 
              select(man_sec, id_upm_60 = id_upm),
            by = "man_sec") %>% 
  left_join(particion_manzanas_li_60, by = "man_sec") %>% 
  replace(is.na(.), 1) %>% 
  group_by(id_upm_60) %>% 
  summarise(grupo = max(grupo_60),
            viv = sum(viv_ocu_pre)) %>% 
  mutate(viv = viv/grupo) %>% 
  summarise(n_upm = sum(grupo),
            n_upm_menor_li = sum(viv < 60),
            n_upm_mayor_3li = sum(viv > 180),
            min_viv = min(viv),
            pro_viv = mean(viv),
            max_viv = max(viv),
            qua_uno = quantile(viv, probs = 0.01))

resumen_upm_80 <- mansec_upm_80_100 %>% 
  left_join(particion_manzanas_li_80, by = "man_sec") %>% 
  replace(is.na(.), 1) %>% 
  group_by(id_upm_80) %>% 
  summarise(grupo = max(grupo_80),
            viv = sum(viv_ocu_pre)) %>% 
  mutate(viv = viv/grupo) %>% 
  summarise(n_upm = sum(grupo),
            n_upm_menor_li = sum(viv < 80),
            n_upm_mayor_3li = sum(viv > 240),
            min_viv = min(viv),
            pro_viv = mean(viv),
            max_viv = max(viv),
            qua_uno = quantile(viv, probs = 0.01))

resumen_upm_100 <- mansec_upm_80_100 %>% 
  left_join(particion_manzanas_li_100, by = "man_sec") %>% 
  replace(is.na(.), 1) %>% 
  group_by(id_upm_100) %>% 
  summarise(grupo = max(grupo_100),
            viv = sum(viv_ocu_pre)) %>% 
  mutate(viv = viv/grupo) %>% 
  summarise(n_upm = sum(grupo),
            n_upm_menor_li = sum(viv < 100),
            n_upm_mayor_3li = sum(viv > 300),
            min_viv = min(viv),
            pro_viv = mean(viv),
            max_viv = max(viv),
            qua_uno = quantile(viv, probs = 0.01))

resumen_aman <- cbind(data.frame(li = c(60,80,100)), 
        rbind(resumen_upm_60, resumen_upm_80, resumen_upm_100))

mansec_upm_60 <- readRDS("D:/MAG/marco_upm/productos/02_conglomeracion/manzanas_sectores_upm_final.rds") %>% 
  filter(nchar(man_sec) == 12)

resumen_disperso <- mansec_upm_60 %>% 
  rename(id_upm_60 = id_upm) %>% 
  left_join(particion_manzanas_li_60, by = "man_sec") %>% 
  replace(is.na(.), 1) %>% 
  group_by(id_upm_60) %>% 
  summarise(grupo = max(grupo_60),
            viv = sum(viv_ocu)) %>% 
  mutate(viv = viv/grupo) %>% 
  summarise(n_upm = sum(grupo),
            n_upm_menor_li = sum(viv < 60),
            n_upm_mayor_3li = sum(viv > 180),
            min_viv = min(viv),
            pro_viv = mean(viv),
            max_viv = max(viv),
            qua_uno = quantile(viv, probs = 0.01))

resumen <- cbind(data.frame(li = c("Aman 60","Aman 80","Aman 100", "Disp 60")), 
                      rbind(resumen_upm_60, resumen_upm_80, resumen_upm_100, resumen_disperso))

write.xlsx(resumen, "informacion/ppt el cepal/20240315/resumen_upm.xlsx")


mansec_upm_60 <- readRDS("D:/MAG/marco_upm/productos/02_conglomeracion/manzanas_sectores_upm_final.rds")

upm_60 <- mansec_upm_60 %>% 
  left_join(particion_manzanas_li_60, by = "man_sec") %>% 
  replace(is.na(.), 1) %>% 
  group_by(id_upm) %>% 
  summarise(grupo = max(grupo_60),
            viv = sum(viv_ocu)) %>% 
  mutate(viv = round(viv/grupo)) %>% 
  uncount(grupo) %>% 
  mutate(Tipo = case_when(substr(id_upm, 7, 7) == "9" ~ "Disperso",
                          T ~ "Amanzanado")) %>% 
  ggplot(aes(viv,  fill = Tipo, alpha = 0.6, colour = Tipo)) + 
  geom_histogram(bins = 50, position = "identity") + 
  geom_vline(xintercept = c(60, 120, 180), linetype = "dashed") + 
  labs(x = "Número de viviendas",
       y = "Frecuencia") +
  theme_minimal() +
  guides(alpha = "none") 

plot(upm_60)

a = 200
h = 100

ggsave(file ="upm_60.png",
       plot = upm_60,
       #device = cairo_ps,
       #device = "eps",
       path = paste0("informacion/ppt el cepal/20240315/"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)
