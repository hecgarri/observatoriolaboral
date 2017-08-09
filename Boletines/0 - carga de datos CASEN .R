######################################################################
# Creación de directorio de variables y match de las bases de datos 
######################################################################

t = proc.time() # Inicia el cronómetro

rm(list=ls())

library(foreign) # Para importar conjuntos de datos en múltiples formatos 
library(dplyr) # Para manipular objetos de tipo data frame
library(survey) # Para trabajar muestras complejas 

setwd("/home/hector/GoogleDrivePersonal/Observatorio Regional/Bases NENE/NENE/BasesCASEN/")

list.files()

casen2015 <- read.spss("~/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Encuesta de Caracterización Socioeconómica Nacional (CASEN)/Casen 2015 SPSS.sav",
                      use.value.labels = FALSE,
                      to.data.frame= FALSE, use.missings = TRUE)

etiquetas.casen = attr(casen2015, "label.table")
directorio.casen = attr(casen2015,"variable.labels")
directorio.casen = data.frame(names(directorio.casen),directorio.casen)

casen2015 = data.frame(casen2015)

diseño = svydesign(id = ~varunit, strata = ~varstrat,weights = ~expr, nest = TRUE, data = casen2015)

diseño$variables = mutate(diseño$variables, educ2 = ifelse(educ==0 | educ==1 | educ==2,"Básica o menor",
                                                    ifelse(educ==3 | educ==4, "Media incompleta", 
                                                    ifelse(educ==5 | educ==6 | educ==7 | educ==9, "Media completa",
                                                    ifelse(educ==6 | educ==8 | educ==10 | 
                                                           educ==11 | educ==12, "E. Superior", NA)))))

salarios = svyby(~yoprCor, by=~sexo+educ2,na.rm=TRUE, svymean,
                 design = subset(diseño, provincia==84)) %>% mutate(cv = round((se/yoprCor)*100,2))

