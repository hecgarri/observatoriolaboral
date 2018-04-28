#*************************************************************#
# Pre-procesamiento de los datos Encuesta CASEN
#*************************************************************#
 
 rm(list=ls())
 library(foreign)
 library(dplyr)
 library(survey)
 setwd("~/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Encuesta de Caracterización Socioeconómica Nacional (CASEN)/")

# Este procedimiento es bastante lento, y utiliza muchos recursos, no realizarlo si se dispone de los arvhicos .csv 

 casen = list.files(pattern="sav$") # todos los arcivos SPSS del directorio

 nombres = strsplit(casen, split=".sav$") # separamos los nombres para quitar .sav

 library(foreign)
 
 todas2 =  lapply(casen, function(x) read.spss(x,use.value.labels  =  FALSE,
          use.missings  =  TRUE, to.data.frame  =  TRUE))
 
 lapply(1:length(todas2), function(i) write.csv(todas2[[i]],paste0(nombres[[i]], ".csv")))

 