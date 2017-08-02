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

#Porcentaje de mujeres jefas de hogar.
jefas_Chile =  svyratio(~I(pco1==1 & sexo==2), ~I(pco1==1), design = diseño, na.rm=TRUE)$ratio %>% as.numeric()

round(jefas_Chile*100, 1)

#Porcentaje de mujeres jefas de hogar en Ñuble
jefas_Chillán = svyratio(~I(pco1==1 & sexo==2), ~I(pco1==1), design = subset(diseño, provincia==84), na.rm=TRUE)$ratio %>% 
  as.numeric()

round(jefas_Chillán*100,1)

total_jefas_chillán = svytotal(~I(pco1==1 & sexo==2),
                               design = subset(diseño, provincia ==84)) 

svyby(~I(pco1==1 & sexo==2), denominator  =~I(pco1==1), by=~zona, design = subset(diseño, provincia ==84), FUN = svyratio)

svyby(~yautcorh2, by=~sexo, design = subset(diseño, provincia==84 & pco1==1),svymean, na.rm=TRUE)

svyby(~I(pobreza==2 | pobreza==1), 
      design = subset(diseño, provincia==84 & pco1==1), by=~sexo, svymean, na.rm=TRUE)

svyby(~I(pobreza==2 | pobreza==1), 
      design = subset(diseño, pco1==1), by=~sexo, svymean, na.rm=TRUE)

svyby(~I(activ==1 | activ==2), denominator = ~I(activ==1 | activ==2 | activ==3), 
         design = subset(diseño, provincia==84), by = ~sexo, FUN=svyratio, na.rm=TRUE)

svyby(~yautcor, by=~sexo, design = subset(diseño, provincia==84),svymean, na.rm=TRUE)

svyby(~yautcor, by=~sexo, design = diseño,svymean, na.rm=TRUE)


brecha = (281403.6/398922.5)-1