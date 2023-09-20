rm(list = ls())

library(tidyverse)
library(sf)

provincia <- list.dirs("productos/01_preparacion_validacion", 
                       full.names = F, 
                       recursive = F)

ca100 <- vector("list", 0)
ca80 <- vector("list", 0)
ca60 <- vector("list", 0)
cd <- vector("list", 0)

i = 13

#for(i in 1:length(provincia)){
  parroquia <- list.dirs(paste0("productos/01_preparacion_validacion/", 
                                provincia[i]), 
                         full.names = F, 
                         recursive = F)
  for(j in 1:length(parroquia)){
    ##########################################################################################
    # 01. Realizar la zonificación y numeración
    ##########################################################################################
    
    # Abrimos el shape de conglomerados
    
    ca100[[parroquia[j]]] <- readRDS(paste0("productos/02_conglomeracion/", provincia[i], "/",
                                    parroquia[j], "/man_sec_conglomerados_", "100", ".rds"))
    
    ca80[[parroquia[j]]] <- readRDS(paste0("productos/02_conglomeracion/", provincia[i], "/",
                                            parroquia[j], "/man_sec_conglomerados_", "80", ".rds"))
    
    ca60[[parroquia[j]]] <- readRDS(paste0("productos/02_conglomeracion/", provincia[i], "/",
                                           parroquia[j], "/man_sec_conglomerados_", "60", ".rds"))
    
    cd[[parroquia[j]]] <- readRDS(paste0("productos/02_conglomeracion/", provincia[i], "/",
                                                    parroquia[j], "/man_sec_conglomerados_disperso_60.rds"))
    
    
  }
#}

ca100 <- do.call(rbind, ca100) %>% 
  mutate(id_upm = paste0(substr(id_man, 1, 6), congf)) %>% 
  group_by(id_upm) %>% 
  summarise(npol = n(),
            nviv = sum(viv)) %>% 
  ungroup()

ca80 <- do.call(rbind, ca80) %>% 
  mutate(id_upm = paste0(substr(id_man, 1, 6), congf)) %>% 
  group_by(id_upm) %>% 
  summarise(npol = n(),
            nviv = sum(viv)) %>% 
  ungroup()

ca60 <- do.call(rbind, ca60) %>% 
  mutate(id_upm = paste0(substr(id_man, 1, 6), congf)) %>% 
  group_by(id_upm) %>% 
  summarise(npol = n(),
            nviv = sum(viv)) %>% 
  ungroup()

cd <- do.call(rbind, cd)%>% 
  mutate(id_upm = paste0(substr(id_sec, 1, 6), "9", substr(congf, 2, 6))) %>% 
  group_by(id_upm) %>% 
  summarise(npol = n(),
            nviv = sum(viv)) %>% 
  ungroup()

p13_1 <- ca100 %>% 
  rbind(cd)

p13_2 <- ca80 %>% 
  rbind(cd)

p13_3 <- ca60 %>% 
  rbind(cd)

ad100 <- p13_1 %>% 
  filter(substr(id_upm, 7,9) != "999") %>% 
  group_by(ad = substr(id_upm, 7, 7)) %>% 
  summarise(npol = n(),
            nviv_min = min(nviv),
            nviv_media = mean(nviv),
            nviv_max = max(nviv),
            nviv_tot = sum(nviv))

ad100_2 <- p13_1 %>% 
  filter(substr(id_upm, 7,9) == "999") %>% 
  group_by(id_upm) %>% 
  summarise(npol = n(),
            nviv = min(nviv))

g100 <- p13_1 %>% 
  mutate(Tipo = case_when(substr(id_upm, 7, 9)=="999" ~ "Aislado",
                          substr(id_upm, 7, 8)=="90" ~ "Disperso",
                          T ~ "Amanzanado")) %>% 
  ggplot(aes(x = nviv)) + 
  geom_histogram(aes(color = Tipo, fill = Tipo),  alpha = 0.25, position = "identity", binwidth = 5, bins = 20) +
  labs(x = "Número de viviendas",
         y = "Número de UPM") +
  theme(panel.background = element_blank())

a = 200
h = 100

ggsave(file ="img01.png",
       plot = g100,
       #device = cairo_ps,
       device = "png",
       path = paste0("informacion/ppt el cepal/"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)

ad80 <- p13_2 %>% 
  filter(substr(id_upm, 7,9) != "999") %>% 
  group_by(ad = substr(id_upm, 7, 7)) %>% 
  summarise(npol = n(),
            nviv_min = min(nviv),
            nviv_media = mean(nviv),
            nviv_max = max(nviv),
            nviv_tot = sum(nviv))

ad80_2 <- p13_2 %>% 
  filter(substr(id_upm, 7,9) == "999") %>% 
  group_by(id_upm) %>% 
  summarise(npol = n(),
            nviv = min(nviv))


g80 <- p13_2 %>% 
  mutate(Tipo = case_when(substr(id_upm, 7, 9)=="999" ~ "Aislado",
                          substr(id_upm, 7, 8)=="90" ~ "Disperso",
                          T ~ "Amanzanado")) %>% 
  ggplot(aes(x = nviv)) + 
  geom_histogram(aes(color = Tipo, fill = Tipo),  alpha = 0.25, position = "identity", binwidth = 5, bins = 20) +
  labs(x = "Número de viviendas",
       y = "Número de UPM") +
  theme(panel.background = element_blank())

a = 200
h = 100

ggsave(file ="img02.png",
       plot = g80,
       #device = cairo_ps,
       device = "png",
       path = paste0("informacion/ppt el cepal/"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)

ad60 <- p13_3 %>% 
  filter(substr(id_upm, 7,9) != "999") %>% 
  group_by(ad = substr(id_upm, 7, 7)) %>% 
  summarise(npol = n(),
            nviv_min = min(nviv),
            nviv_media = mean(nviv),
            nviv_max = max(nviv),
            nviv_tot = sum(nviv))

ad60_2 <- p13_3 %>% 
  filter(substr(id_upm, 7,9) == "999") %>% 
  group_by(id_upm) %>% 
  summarise(npol = n(),
            nviv = min(nviv))


g60 <- p13_3 %>% 
  mutate(Tipo = case_when(substr(id_upm, 7, 9)=="999" ~ "Aislado",
                          substr(id_upm, 7, 8)=="90" ~ "Disperso",
                          T ~ "Amanzanado")) %>% 
  ggplot(aes(x = nviv)) + 
  geom_histogram(aes(color = Tipo, fill = Tipo),  alpha = 0.25, position = "identity", binwidth = 5, bins = 20) +
  labs(x = "Número de viviendas",
       y = "Número de UPM") +
  theme(panel.background = element_blank())

a = 200
h = 100

ggsave(file ="img03.png",
       plot = g60,
       #device = cairo_ps,
       device = "png",
       path = paste0("informacion/ppt el cepal/"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)
