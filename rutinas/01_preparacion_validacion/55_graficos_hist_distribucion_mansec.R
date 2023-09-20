rm(list = ls())

library(tidyverse)

bdd <- readRDS("intermedios/01_preparacion_validacion/precenso_edificios.rds")

summary(bdd)

bdd_manzana <- bdd %>% 
  mutate(man_loc = substr(id_edif, 1, 15)) %>% 
  group_by(man_loc) %>% 
  summarise(viv_ocu = sum(viv_ocu, na.rm = T)) %>% 
  mutate(n_upm = floor(viv_ocu/100))

bdd_sector <- bdd_manzana %>% 
  mutate(sector = substr(man_loc, 1, 12)) %>% 
  group_by(sector) %>% 
  summarise(viv_ocu = sum(viv_ocu, na.rm = T),
            n_man_loc = n()) %>% 
  mutate(n_upm = floor(viv_ocu/100))

g1 <- bdd_sector %>% 
  mutate(UPM = case_when(viv_ocu < 100 ~ "Juntar (Menor a 100)",
                         viv_ocu >= 100 & viv_ocu < 200 ~ "Dentro del rango (100 - 200)",
                         viv_ocu >= 200 ~ "Dividir (Mayor a 200)")) %>% 
  ggplot(aes(x = viv_ocu, fill = UPM)) + 
  geom_histogram(bins = 300, color = "white")+
  # scale_color_manual(values = c( "#B81D13", "#EFB700", "#008450")) +
  scale_fill_manual(values = c("#008450", "#B81D13", "#EFB700")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_rect(fill="white"),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10),
        axis.ticks = element_blank(),
        legend.direction = "horizontal", 
        legend.position = "bottom",
        legend.box = "vertical",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.box.spacing = unit(0, "cm"),
        legend.title.align = 0.5,
        panel.background = element_blank()) 

plot(g1)

g2 <- bdd_manzana %>% 
  mutate(UPM = case_when(viv_ocu < 100 ~ "Juntar (Menor a 100)",
                         viv_ocu >= 100 & viv_ocu < 200 ~ "Dentro del rango (100 - 200)",
                         viv_ocu >= 200 ~ "Dividir (Mayor a 200)")) %>% 
  ggplot(aes(x = viv_ocu, fill = UPM)) + 
  geom_histogram(bins = 300, color = "white")+
  # scale_color_manual(values = c( "#B81D13", "#EFB700", "#008450")) +
  scale_fill_manual(values = c("#008450", "#B81D13", "#EFB700")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_rect(fill="white"),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10),
        axis.ticks = element_blank(),
        legend.direction = "horizontal", 
        legend.position = "bottom",
        legend.box = "vertical",
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.box.spacing = unit(0, "cm"),
        legend.title.align = 0.5,
        panel.background = element_blank())

plot(g2)
