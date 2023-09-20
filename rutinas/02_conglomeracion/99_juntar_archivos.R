parroquia <- list.dirs(paste0("productos/01_preparacion_validacion/", 
                              "18"), 
                       full.names = F, 
                       recursive = F)
for(j in 1:length(parroquia)){
  h <- readRDS(paste0("productos/02_conglomeracion/",
                         "18", "/", parroquia[j],
                         "/man_sec_conglomerados.rds"))
  if (j == 1){
    z <- h
  }else{
    z <- rbind(z,h)
  }
}

conglomerados <- z %>% 
  mutate(id_conglomerado = paste0(substr(id_man, 1, 6), congf)) %>% 
  select(id_man, id_conglomerado)

saveRDS(conglomerados, "conglomerados_tungurahua_preliminar.rds")
