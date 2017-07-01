archivo = "/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Datos de Migración/Formato-WEB-PDs-2005-2016.csv"

if (!require(data.table)) install.packages("data.table")
if (!require(tidyverse)) install.packages("tidyverse")

datos = fread(archivo)

names(datos) = tolower(names(datos))

datos_nuble = datos %>% filter(provincia=="ÑUBLE")

tabla = datos_nuble %>% group_by(año, pais) %>% count()