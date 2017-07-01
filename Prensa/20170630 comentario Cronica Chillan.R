# Aquí respaldo algunos cálculos breves que hice para una columna del diario la discusión. 

######################################################################
# Creación de directorio de variables y match de las bases de datos 
######################################################################

t = proc.time() # Inicia el cronómetro

rm(list=ls())

library(foreign)
library(dplyr)


list.files()
directorio_local = "~/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Encuesta de Caracterización Socioeconómica Nacional (CASEN)/Casen_2015_SPSS.sav"
casen2015 = read.spss(directorio_local,use.value.labels = FALSE,
                      to.data.frame= FALSE, use.missings = TRUE)

etiquetas.casen = attr(casen2015, "label.table")
directorio.casen = attr(casen2015,"variable.labels")
directorio.casen = data.frame(names(directorio.casen),directorio.casen)

casen2015 = data.frame(casen2015)


if (!require(survey)) insall.packages("survey")

casen2015 = mutate(casen2015, yautcorh2 = yautcorh/numper, 
                   dependiente = ifelse(o15==3 | o15==4 |
                                          o15==5 | o15==6 |
                                          o15==7,1,0)) 


diseño = svydesign(id = ~varunit, strata = ~varstrat,
                   weights = ~expr, nest = TRUE, data = casen2015)


# Códigos CIUO 

ciuo = read.csv("~/GoogleDrivePersonal/Observatorio Regional/Bases NENE/NENE/codigos_ciuo.csv")

ciuo = ciuo %>% group_by(etiqueta, codigo) %>% filter(row_number(codigo)==1)


if (!require(reldist)) install.packages("reldist")
if (!require(tidyverse)) install.packages("tidyverse")

svyratio(~I(yoprCor<=192800),~I(o1==1 | o2==1 | o3==1),
         design = subset(diseño, provincia==84), na.rm=TRUE)

svytotal(~I(yoprCor<=192800), design = subset(diseño, provincia==84), na.rm=TRUE)

svytotal(~I(o1==1 | o2==1 | o3==1), design = subset(diseño, provincia==84), na.rm=TRUE)

svymean(~I(yoprCor<=192800), design = subset(diseño, provincia==84), na.rm=TRUE)