library(data.table) # install.packages("data.table")
library(dplyr)      # install.packages("dplyr")
library(stringi)    # install.packages("stringi")
library(survey)     # install.packages("survey")
#-------------------------------------------------------------------------------
sessionInfo()
#-------------------------------------------------------------------------------
rm(list=ls())
#-------------------------------------------------------------------------------
# SE FIJA EL DIRECTORIO DE TRABAJO
#-------------------------------------------------------------------------------

#setwd("C:\\Users\\Miguelo\\Documents\\NENE\\")
setwd("/home/hector/GoogleDrivePersonal/Observatorio Regional/Bases NENE/NENE/")

#-------------------------------------------------------------------------------
t <- proc.time() # Inicia el cronometro
#
x=seq(1,82,1) # c(seq(1,10,3),seq(61,70,3))
nene = paste0("nene_",x,".csv")
# ENEs
# 2010:01-11: Central: 01,04,07,10 
# 2011:12-23: Central: 13,16,19,22
# 2012:24-35: Central: 25,28,31,34
# 2013:36-47: Central: 37,40,43,46
# 2014:48-59: Central: 49,52,55,58
# 2015:60-71: Central: 61,64,67,70
# 2016:72-83: Central: 73,76,79,82

# Se importan todas las bases establecidas en la secuencia de una sola vez
todas =  lapply(nene, function(x) fread(x, sep=",", header=TRUE,
                                        select=c("id_directorio", "estrato", 
                                                 "fact", "cae_general","b8","b9","b18_codigo",
                                                 "categoria_ocupacion", "r_p_c", "b14", "e18")))

#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLES region_e y prov_e
#-------------------------------------------------------------------------------
# Creamos una variable auxiliar que cuenta el numero de caracteres
for (i in 1:length(todas)){
  todas[[i]]$aux = stri_length(todas[[i]]$b18_codigo)
}

# Aqui el orden es muy importante. Primero el comando anterior, luego este
for (i in 1:length(todas)){
  todas[[i]]$region_e = todas[[i]]$b18_codigo
}

for (i in 1:length(todas)){
  todas[[i]]$region_e = ifelse(todas[[i]]$aux==5, 
                               stri_sub(todas[[i]]$region_e,1,2), 
                               todas[[i]]$region_e)
}

for (i in 1:length(todas)){
  todas[[i]]$region_e = ifelse(todas[[i]]$aux==4 
                               ,stri_sub(todas[[i]]$region_e,1,1),
                               todas[[i]]$region_e)
}

# Ahora la variable region_e contiene la region donde la persona trabaja.
for (i in 1:length(todas)){
  todas[[i]]$region_e<-strtoi(todas[[i]]$region_e)
}

for (i in 1:length(todas)){
  todas[[i]]$prov_e = ifelse(todas[[i]]$aux==5,                                 
                             stri_sub(todas[[i]]$b18_codigo,1,3), 
                             todas[[i]]$b18_codigo)
}

for (i in 1:length(todas)){
  todas[[i]]$prov_e = ifelse(todas[[i]]$aux==4 
                             ,stri_sub(todas[[i]]$prov_e,1,2),
                             todas[[i]]$prov_e)
}

#-------------------------------------------------------------------------------
for (i in 1:length(todas)){
  todas[[i]]$cae_general = recode(todas[[i]]$cae_general,`0`=0,`1` = 1,`2` = 1,
                                  `3` = 1, `4` = 2,`5` = 3,`6` = 4,
                                  `7` = 4,`8` = 4,`9` = 4)
}

for (i in 1:length(todas)){
  todas[[i]]$cae_general = factor(todas[[i]]$cae_general,levels = c(0,1,2,3,4), 
                                  labels = c("menor de 15","Ocupado", "Desocupado", 
                                             "Busca Trabajo Primera Vez","Inactivo"))
}

for (i in 1:length(todas)){
  todas[[i]]$sector = todas[[i]]$b14
}

for (i in 1:length(todas)){
  todas[[i]]$sector = ifelse(is.na(todas[[i]]$sector),
                             todas[[i]]$e18,
                             todas[[i]]$sector)
}

for (i in 1:length(todas)){
  todas[[i]]$sector<-factor(todas[[i]]$sector,
                            levels=c(1,2,3,4,5,6,7,8,9,10,
                                     11,12,13,14,15,16,17),
                            labels=c("Agricultura",
                                     "Pesca",
                                     "Explotacion",
                                     "Industrias",
                                     "Electricidad",
                                     "Construccion",
                                     "Comercio", 
                                     "Hoteles",
                                     "Transporte",
                                     "Financiero",
                                     "Inmobiliarias",
                                     "Administracion Publica",
                                     "Ensenanza",
                                     "Servicios sociales",
                                     "Otras actividades",
                                     "Hogares privados", 
                                     "Organizaciones extraterritoriales"))
}

# Segun la primera clasificacion, los sectores son: 
#   1. Agricultura 
#   2. Pesca
#   3. Mineria
#   4. Industria Manufacturera
#   5. Electricidad, Gas y Agua
#   6. Construccion 
#   7. Comercio
#   8. Restaurantes y Hoteles
#   9. Transporte y Comunicaciones
#   10. Servicios Financieros
#   11. Servicios Personales
#   12. Administracion Publica
#   13. Actividades Inmobiliarias, Empresariales y de Alquiler.
# 
# Segun el segundo manual, los sectores son: 
# 
# 1. Agropecuario silvicola 
# 2. Pesca
# 3. Mineria
# 4. Industria Manufacturera
# 5. Electricidad, Gas y Agua 
# 6. Construccion 
# 7. Comercio   
# 8. Hoteles y Restoranes
# 9. Transporte y Comunicaciones 
# 10. Intermediacion Financiera
# 11. Actividades Inmobiliarias, Empresariales y de Alquiler.
# 12. Servicios Sociales y Personales
# 13. Administracion Publica 

#-------------------------------------------------------------------------------
## Homologacion de Sectores con Banco Central. 
for (i in 1:length(todas)){
  todas[[i]]$sector<-recode_factor(todas[[i]]$sector, 
                                   'Agricultura'= "Agropecuario-Silvicola",
                                   'Pesca'="Pesca", 
                                   'Explotacion'="Mineria",
                                   'Industrias'="Industria Manufacturera", 
                                   'Electricidad'="Electricidad, Gas y Agua",
                                   'Construccion'="Construccion", 
                                   'Comercio'="Comercio",
                                   'Hoteles'="Hoteles y Restoranes", 
                                   'Transporte'="Transporte y Comunicaciones",
                                   'Financiero'="Intermediacion Financiera", 
                                   'Inmoviliarias'="Actividades Inmobiliarias, Empresariales y de Alquiler", 
                                   'Ensenanza'="Servicios Sociales y Personales",
                                   'Servicios sociales'="Servicios Sociales y Personales",
                                   'Otras actividades'="Servicios Sociales y Personales",
                                   'Hogares privados'="Servicios Sociales y Personales",
                                   'Administracion Publica'="Administracion Publica",
                                   'Organizaciones extraterritoriales'="Administracion Publica")
}

for (i in 1:length(todas)){
  todas[[i]]$sector2=as.numeric(todas[[i]]$sector)  
}

# Esta variable se creo para eliminar el sector mineria y pesca.
for (i in 1:length(todas)){
  todas[[i]]$sector3=ifelse(todas[[i]]$sector2!=2 & 
                              todas[[i]]$sector2!=3,
                            todas[[i]]$sector2,NA)  
}


#-------------------------------------------------------------------------------
# Creamos una variable auxiliar que cuenta el numero de caracteres
for (i in 1:length(todas)){
  todas[[i]]$aux<-stri_length(todas[[i]]$r_p_c)
}

# Este codigo es para extraer la provincia 
for (i in 1:length(todas)){
  todas[[i]]$prov = ifelse(todas[[i]]$aux==5,                                 
                           stri_sub(todas[[i]]$r_p_c,1,3), 
                           todas[[i]]$r_p_c)
}

for (i in 1:length(todas)){
  todas[[i]]$prov = ifelse(todas[[i]]$aux==4,                                 
                           stri_sub(todas[[i]]$r_p_c,1,2), 
                           todas[[i]]$prov)
}

#-------------------------------------------------------------------------------
# Incorporar el info y realizar inferencia para muestras complejas
#-------------------------------------------------------------------------------
#Para evitar clusters con una sola observacion
options(survey.lonely.psu = "certainty") 

# info muestral de la NENE
info = list()
for (i in 1:length(todas)){
  info[[i]] = svydesign(id = ~id_directorio, strata = ~estrato, weights  = 
                          ~fact,nest = TRUE, data = todas[[i]])
}

