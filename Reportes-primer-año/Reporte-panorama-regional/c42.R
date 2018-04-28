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
setwd("C:\\Users\\Miguelo\\Documents\\NENE\\")
#-------------------------------------------------------------------------------
t <- proc.time() # Inicia el cronometro
#
x=seq(73,82,3) # c(seq(1,10,3),seq(61,70,3))
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
                                        select=c("id_directorio", "estrato", "nacionalidad", 
                                                 "fact", "cae_general", "mes_central", 
                                                 "ano_trimestre", "b18_codigo", "region", 
                                                 "categoria_ocupacion", "b14", "b15_1", "b15_2", 
                                                 "b8", "b9", "b7_3", "b7_4", "b7_1", "b7_2", "b2", 
                                                 "nivel", "curso", "edad", "r_p_c","b1", "sexo",
                                                 "termino_nivel", "c1", "c10", "c11", "habituales", 
                                                 "e18", "e9", "e12", "e19")))

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
apiladas =do.call(rbind, todas)
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

# info muestral de la ENE apilada

info2 = svydesign(id = ~id_directorio, strata = ~estrato, weights  = 
                    ~fact,nest = TRUE, data = apiladas)
#-------------------------------------------------------------------------------
# FIN del script main.R

# SCript del cuadro o grafico xxxx

proc.time()-t    # Detiene el cronometro

#FIN 
