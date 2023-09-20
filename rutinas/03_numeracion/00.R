rm(list = ls())

library(tidyverse)
library(sf)

li = 80

provincia <- list.dirs("productos/01_preparacion_validacion", 
                       full.names = F, 
                       recursive = F)

for(i in 1:length(provincia)){
  parroquia <- list.dirs(paste0("productos/01_preparacion_validacion/", 
                                provincia[i]), 
                         full.names = F, 
                         recursive = F)
  for(j in 1:length(parroquia)){
    ##########################################################################################
    # 01. Realizar la zonificación y numeración
    ##########################################################################################
    
    # Abrimos el shape de conglomerados
    
    conglomerados <- read_sf(paste0("productos/02_conglomeracion/", provincia[i], "/",
                                    parroquia[j], "/conglomerados_", li, ".gpkg"))
    
    # Cargamos las funciones que permiten aplicar el algoritmo de kernigan lin
    source("Rutinas/Funciones/KerLin/ker_lin.r")
    ker_lin()
    
    # Cargamos la función zonificar
    source("Rutinas/Funciones/zonificar.r")
    apoyo <- zonificar(conglomerados, 10)
    
    # Creamos el shape de zonas
    zona <- conglomerados %>% 
      left_join(select(apoyo,zona=equipo,congf),by="congf") %>% 
      group_by(zona) %>% 
      summarise(ncong=n())
    
    # Se realiza la numeración de las zonas
    source("Rutinas/Funciones/serpenteante.r")
    zona_01 <- serpenteante(zona)%>% 
      mutate(orden=str_pad(orden,3,"left","0"))
    
    ##########################################################################################
    # 02. Realizar la numeración dentro de cada zona
    ##########################################################################################
    
    sector <- conglomerados %>% 
      left_join(select(apoyo,zona=equipo,congf),by="congf") %>% 
      left_join(select(as.data.frame(zona_01),zona,orden),by="zona") %>% 
      select(parroquia, zona=orden,congf)
    
    indice <- unique(sector$zona)
    
    for (k in 1:length(indice)){
      apoyo <- sector %>% 
        filter(zona==indice[k]) %>% 
        serpenteante() %>% 
        mutate(sector=str_pad(orden,3,"left","0")) %>% 
        select(-orden)
      if(k==1){
        sector_01=apoyo
      }
      else{
        sector_01=rbind(sector_01,apoyo)
      }
    }
    
    sector_01 <- sector_01 %>%
      mutate(congf=str_pad(congf,3,"left","0"))
    
    rm(zona_01,zona,sector)
    
    ##########################################################################################
    # 03. Realizar la numeración de las manzanas dentro de cada sector
    ##########################################################################################
    
    manext <- read_sf(paste0("productos/01_preparacion_validacion/", provincia[i], "/",
                             parroquia[j], "/manzanas_extendidas.gpkg"))
    
    # En el shape de manzanas extendidas se incluyen las variables de zona y sector
    manzanas <- manext %>% 
      st_join(sector_01, join = st_within) %>%  
      mutate(zonsec=paste0(zona,sector))
    
    
    
    indice <- unique(manzanas$zonsec)
    
    for (k in 1:length(indice)){
      apoyo <- manzanas %>% 
        filter(zonsec==indice[k]) %>% 
        serpenteante() %>% 
        mutate(manzana=str_pad(orden,3,"left","0")) %>% 
        select(-orden)
      if(k==1){
        manzanas_01=apoyo
      }
      else{
        manzanas_01=rbind(manzanas_01,apoyo)
      }
      print(k)
    }
    
    zonaupm <- sector_01 %>% 
      group_by(parroquia, zonam = zona) %>% 
      summarise()
    
    sectorupm <- sector_01 %>% 
      select(parroquia, zonam = zona, sectorm = sector)
    
    manzanaupm <- manzanas_01 %>% 
      select(man, parroquia, zonam = zona, sectorm = sector, manzanam = manzana)
    
    dir.create(paste0("productos/03_numeracion/", provincia[i]))
    
    dir.create(paste0("productos/03_numeracion/", provincia[i], "/", parroquia[j]))
    
    write_sf(zonaupm, paste0("productos/03_numeracion/", provincia[i], "/",
                             parroquia[j], "/zonas_upm.gpkg"))
    
    write_sf(sectorupm, paste0("productos/03_numeracion/", provincia[i], "/",
                               parroquia[j], "/sectores_upm.gpkg"))
    
    write_sf(manzanaupm, paste0("productos/03_numeracion/", provincia[i], "/",
                                parroquia[j], "/manzanas_upm.gpkg"))
  }
  
}

