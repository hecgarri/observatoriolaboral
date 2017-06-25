######################################################################
# Creación de directorio de variables y match de las bases de datos 
######################################################################

t = proc.time() # Inicia el cronómetro

rm(list=ls())

library(foreign)
library(dplyr)

setwd("~/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/Ocupaciones página web/")

list.files()
directorio_local = "~/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Encuesta de Caracterización Socioeconómica Nacional (CASEN)/Casen_2015_SPSS.sav"
casen2015 = read.spss(directorio_local,use.value.labels = FALSE,
                      to.data.frame= FALSE, use.missings = TRUE)

etiquetas.casen = attr(casen2015, "label.table")
directorio.casen = attr(casen2015,"variable.labels")
directorio.casen = data.frame(names(directorio.casen),directorio.casen)

casen2015 = data.frame(casen2015)


library(survey)

diseño = svydesign(id = ~varunit, strata = ~varstrat,
                   weights = ~expr, nest = TRUE, data = casen2015)


# Códigos CIUO 

ciuo = read.csv("~/GoogleDrivePersonal/Observatorio Regional/Bases NENE/NENE/codigos_ciuo.csv")

ciuo = ciuo %>% group_by(etiqueta, codigo) %>% filter(row_number(codigo)==1)

numero = "/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/Ocupaciones - Página WEB/OLR-Nuble---Ocupaciones/1 - numero de trabajadores.R"
categoria = "/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/Ocupaciones - Página WEB/OLR-Nuble---Ocupaciones/2 - categoria ocupacional.R"
horas = "/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/Ocupaciones - Página WEB/OLR-Nuble---Ocupaciones/3 - horas promedio.R"
in_hora = "/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/Ocupaciones - Página WEB/OLR-Nuble---Ocupaciones/4 - ingreso por hora semanal.R"
