######################################################################
# Creación de directorio de variables y match de las bases de datos 
######################################################################

t = proc.time() # Inicia el cronómetro

rm(list=ls())

library(foreign) # Para importar conjuntos de datos en múltiples formatos 
library(dplyr) # Para manipular objetos de tipo data frame
library(survey) # Para trabajar muestras complejas 

#setwd("C:\\Users\\Usuario\\Google Drive\\OLR Ñuble - Observatorio laboral de Ñuble\\Bases de datos\\Encuesta de Caracterización Socioeconómica Nacional (CASEN)\\")
setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Encuesta de Caracterización Socioeconómica Nacional (CASEN)/")

list.files()

casen2015 = read.spss("Casen 2015 SPSS.sav",use.value.labels = FALSE,
                      to.data.frame= FALSE, use.missings = TRUE)

etiquetas.casen = attr(casen2015, "label.table")
directorio.casen = attr(casen2015,"variable.labels")
directorio.casen = data.frame(names(directorio.casen),directorio.casen)

casen2015 = data.frame(casen2015)

diseno = svydesign(id = ~varunit, strata = ~varstrat, weights = ~expr, nest = TRUE, data = casen2015)

diseno$variables$ingreso_hora = casen2015$yoprCor/(casen2015$o10*4)

diseno$variables$sexo2 = ifelse(diseno$variables$sexo==2,1,0) 

diseno$variables$ocup = ifelse(diseno$variables$oficio1==1,1,0) 

diseno$variables$ocup2 = ifelse(diseno$variables$oficio1==2,1,0) 

modelo = svyglm(log(ingreso_hora)~ocup+sexo2+esc+I(sexo2*ocup)+I(esc*ocup)+
                  I(sexo2*ocup2)+I(esc*ocup2), design = diseno)



