rm(list = ls())
# Cargamos las librerías a usar
library(tidyverse)
library(sf)
library(foreach)
library(doParallel)
# Cargamos las funciones a utilizar
source("rutinas/funciones/matinc.R")
source("rutinas/funciones/conglomera2.R")
# Definimos el límite inferior del tamaño de los conglomerados
li = 60
# Cargamos la base con el número de viviendas por edificio
peso_edif <- readRDS("intermedios/01_preparacion_validacion/precenso_edificios.rds")
# Calculamos el número de viviendas por man_sec
pesos <- peso_edif %>% 
  group_by(mansec) %>% 
  summarise(viv = sum(viv_ocu)) %>% 
  ungroup()
# Se abre la división de manzanas en función del li
particion <- readRDS(paste0("intermedios/02_conglomeracion/particion_manzanas_li_", li, ".rds"))
manzanas_excluir <- unique(particion$mansec)
# Definimos el largo en metros mínimo para contar con incidencia entre manzanas
largo_man <- 10


cl <- makeCluster(9, outfile ="")
registerDoParallel(cl)

load("intermedios/lista_parroquias.RData")

#i = c(1:1042)[index == "091054"]

index <- index[!index %in% c("170150", "090150")]

# primer for de provincia
foreach(i=1:length(index),
        .packages = c("tidyverse", "sf"))%dopar%{
          if(file.exists(paste0("productos/01_preparacion_validacion/", 
                                substr(index[i], 1, 2), "/", index[i],
                                "/manzanas_extendidas.gpkg"))){
            # Cargamos las manzanas de cada parroquia
            manzanas <- read_sf(paste0("productos/01_preparacion_validacion/", 
                                       substr(index[i], 1, 2), "/", index[i],
                                       "/manzanas_extendidas.gpkg")) %>% 
              filter(!man %in% manzanas_excluir) %>% 
              rename(mansec = man)
            # Calculamos la matriz de incidencia de las manzanas de la parroquia
            matman <- matinc(manzanas, largo_man, "mansec")
            # Generamos la matriz de pesos de las manzanas 
            pesman <- cbind("mansec" = row.names(matman)) %>% 
              as.data.frame() %>% 
              left_join(pesos, by = "mansec") %>% 
              mutate(viv = ifelse(is.na(viv), 0, viv))
            # Aplicamos el algoritmo de conglomeración
            h <- conglomera2(matman, peso = pesman, sl = li, id = "mansec") %>% 
              mutate(congf = str_pad(congf, 6, "left", "0"))
            # Generamos el shape de conglomerados
            apoyo <- manzanas %>% 
              left_join(h, by = "mansec") %>% 
              mutate(parroquia = substr(mansec, 1, 6)) %>% 
              group_by(parroquia, congf) %>% 
              summarise(viv = sum(viv))
            # Guardado
            
            dir.create(paste0("productos/02_conglomeracion/", substr(index[i], 1, 2)), 
                       showWarnings = F)
            
            dir.create(paste0("productos/02_conglomeracion/", substr(index[i], 1, 2), "/",
                              index[i]), showWarnings = F)
            
            write_sf(apoyo, paste0("productos/02_conglomeracion/",
                                   substr(index[i], 1, 2), "/", index[i],
                                   "/conglomerados_", li, ".gpkg"))
            
            saveRDS(h, paste0("productos/02_conglomeracion/",
                              substr(index[i], 1, 2), "/", index[i],
                              "/man_sec_conglomerados_", li, ".rds"))
          }
        }

# control_conglomerado <- h %>% 
#   group_by(conglomerado = congf) %>% 
#   summarise(viv = sum(viv),
#             nman = n()) %>% 
#   mutate(tipo = "muestreo")
# 
# control_sector <- h %>% 
#   group_by(conglomerado = substr(id_man, 1, 12)) %>% 
#   summarise(viv = sum(viv),
#             nman = n()) %>% 
#   mutate(tipo = "censo")
# 
# control <- rbind(control_conglomerado, control_sector)
# 
# p<-ggplot(control, aes(x=viv, color=tipo, fill=tipo)) +
#   geom_histogram(position="identity", binwidth = 4, alpha = 0.3)+
#   # geom_vline(data=mu, aes(xintercept=grp.mean, color=sex),
#   #            linetype="dashed")+
#   theme(legend.position="top")
# 
# plot(p)