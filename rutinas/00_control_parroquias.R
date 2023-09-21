load("intermedios/lista_parroquias.RData")

dir_local <- gsub("//", "/", list.files("productos/01_preparacion_validacion/", recursive = T))

lista_parroquias_extendidas <- substr(dir_local, 4, 9)

lista_no_extendidas <- index[!index %in% lista_parroquias_extendidas]

index <- lista_no_extendidas

read_sf("productos/01_preparacion_validacion/01/010150/manzanas_extendidas.gpkg")
j = 1

lol <- vector()

for(i in 1:length(index)){
  if(file.exists(paste0("productos/01_preparacion_validacion/",
                        substr(index[i], 1, 2), "/", index[i],
                        "/manzanas_extendidas.gpkg"))){
    lol[j] <- as.numeric(unique(st_geometry_type(read_sf(paste0("productos/01_preparacion_validacion/",
                                                     substr(index[i], 1, 2), "/", index[i],
                                                     "/manzanas_extendidas.gpkg")))))
    j = j+1
    print(as.numeric(unique(st_geometry_type(read_sf(paste0("productos/01_preparacion_validacion/",
                                                 substr(index[i], 1, 2), "/", index[i],
                                                 "/manzanas_extendidas.gpkg"))))))
  }
}


unique(st_geometry_type(read_sf("intermedios/01_preparacion_validacion/06/060650/rios.gpkg")))
