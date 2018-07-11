## ocupaciones.R
##
## Héctor Garrido Henríquez
## Analista Cuantitativo. Observatorio Laboral Ñuble
## Docente part-time. Facultad de Ciencias Empresariales
## Universidad del Bío-Bío
## Avenida Andrés Bello 720, Casilla 447, Chillán
## Teléfono: +56-942353973
## http://www.observatoriolaboralnuble.cl

rm(list=ls())

if (!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
if (!require(foreign)) install.packages("foreign"); require(foreign)
if (!require(srvyr)) install.packages("srvyr"); require(srvyr)
if (!require(lubridate)) install.packages("lubridate"); require(lubridate)

path = file.path("/home/hector/GoogleDriveUBB/",
                 "OLR Ñuble - Observatorio laboral de Ñuble","Bases de datos/",
                 "Encuesta de Caracterización Socioeconómica Nacional (CASEN)/Casen 2015 SPSS.sav")

casen2015 = read.spss(path,use.value.labels = FALSE,
                      to.data.frame= FALSE, use.missings = TRUE)


etiquetas.casen = attr(casen2015, "label.table")
directorio.casen = attr(casen2015,"variable.labels")
directorio.casen = data.frame(names(directorio.casen),directorio.casen)


casen2015 = tbl_df(casen2015) %>% filter(provincia ==84)

## Creación de variables relevantes para el análisis 

casen2015 = casen2015 %>% mutate(horas_trabajo = ifelse(o10!=999, o10, NA), 
            dependientes = recode_factor(o15, `3` = 11,`4` = 11,
            `5` = 11, `6` = 11, 
            `7` = 11),
            nivel_educ = ifelse(educ==0 | educ==1, "Básica completa o menor",
            ifelse(educ==2, "Básica completa o menor", 
            ifelse(educ==3 | educ==4, "Básica completa o menor", 
            ifelse(educ==5 | educ==6, "Media completa", 
            ifelse(educ==7, "Media completa", 
            ifelse(educ==8, "Técnico Nivel Superior o Profesional", 
            ifelse(educ==9, "Media completa", 
            ifelse(educ==10, "Técnico Nivel Superior o Profesional", 
                                                                                                      ifelse(educ==11, "Técnico Nivel Superior o Profesional", 
                                                                                                             ifelse(educ==12, "Técnico Nivel Superior o Profesional", NA)))))))))))

## Creación del diseño muestral 

diseno = casen2015 %>% as_survey_design(id = varunit, strata = varstrat,
                                        weights  = expr,nest = TRUE)


