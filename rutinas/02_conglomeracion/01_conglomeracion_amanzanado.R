rm(list = ls())
# Cargamos las librerías a usar
library(tidyverse)
library(sf)
# Cargamos las funciones a utilizar
source("rutinas/funciones/matinc.R")
source("rutinas/funciones/conglomerar.R")
# Definimos el límite inferior del tamaño de los conglomerados
li = 60
# Cargamos la base con el número de viviendas por edificio
peso_edif <- readRDS("intermedios/01_preparacion_validacion/precenso_edificios.rds")
# Calculamos el número de viviendas por man_sec
pesos <- peso_edif %>% 
  group_by(man_sec = ifelse(substr(id_edif, 7, 9) == "999", 
                            substr(id_edif, 1, 12),
                            substr(id_edif, 1, 15))) %>% 
  summarise(viv = sum(viv_ocu)) %>% 
  ungroup()
# Identificamos la provincias de las que tenemos manzanas extendidas por parroquia
provincia <- list.dirs("productos/01_preparacion_validacion", 
                       full.names = F, 
                       recursive = F)
# Definimos el largo en metros mínimo para contar con incidencia entre manzanas
largo_man <- 10
#Iniciamos el laso for que recorre a las provincias
for (i in 1 : length(provincia)){
  # Identificamos las parroquias en cada provincia
  parroquia <- list.dirs(paste0("productos/01_preparacion_validacion/", 
                                provincia[i]), 
                         full.names = F, 
                         recursive = F)
  parroquia <- parroquia[!parroquia %in% c("090150", "170150")]
  # Iniciamos el laso for que recorre las parroquias
  for (j in 1 : length(parroquia)){
    # Cargamos las manzanas de cada parroquia
    manzanas <- read_sf(paste0("productos/01_preparacion_validacion/", 
                               provincia[i], "/" , parroquia[j],
                               "/manzanas_extendidas.gpkg"))
    # Calculamos la matriz de incidencia de las manzanas de la parroquia
    matman <- matinc(manzanas, largo_man, "man")
    # Generamos la matriz de pesos de las manzanas 
    pesman <- cbind("id_man" = row.names(matman)) %>% 
      as.data.frame() %>% 
      left_join(pesos, by = c("id_man" = "man_sec")) %>% 
      mutate(viv = ifelse(is.na(viv), 0, viv))
    # Aplicamos el algoritmo de conglomeración
    h <- conglomerar(matman, peso = pesman, sl = li, id = "id_man") %>% 
      mutate(congf = str_pad(congf, 6, "left", "0"))
    # Generamos el shape de conglomerados
    apoyo <- manzanas %>% 
      left_join(h, by = c("man" = "id_man")) %>% 
      mutate(parroquia = substr(man, 1, 6)) %>% 
      group_by(parroquia, congf) %>% 
      summarise(viv = sum(viv))
    # Guardado
    
    dir.create(paste0("productos/02_conglomeracion/", provincia[i]), 
               showWarnings = F)
    
    dir.create(paste0("productos/02_conglomeracion/", provincia[i], "/",
                      parroquia[j]), showWarnings = F)
    
    write_sf(apoyo, paste0("productos/02_conglomeracion/",
                           provincia[i], "/", parroquia[j],
                           "/conglomerados_", li, ".gpkg"))
    
    saveRDS(h, paste0("productos/02_conglomeracion/",
                           provincia[i], "/", parroquia[j],
                           "/man_sec_conglomerados_", li, ".rds"))
    
    cat("Se realizó la conglomeración de la parroquia", parroquia[j])
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