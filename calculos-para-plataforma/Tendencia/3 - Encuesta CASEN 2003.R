#########################################################################################
# Encuesta CASEN 2003
#########################################################################################

rm(list=ls())
setwd("~/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Encuesta de Caracterización Socioeconómica Nacional (CASEN)/")

list.files(pattern = ".csv$")

library(data.table)
library(dplyr)
library(survey)


casen2003 =  fread("Casen2003.csv", sep=",", header=TRUE,
            select=c('R','P', 'COMU', 'SEGMENTO',
            'estrato','F', 'O',  'EXPR', 'EXPC', 'O7', 'ESC'))

ingresos2003 = fread('ingresos_originales_CASEN_2003_03122013.csv', sep=",", header = TRUE, 
                     select=c('f', 'segmento', 'o', 'o18'))

casen2003 = merge(casen2003, ingresos2003,
                  by.x = c('SEGMENTO', 'F', 'O'), by.y = c('segmento','f', 'o'))

names(casen2003) = tolower(names(casen2003))

options(survey.lonely.psu="certainty") # equivalente a singleunit = certainty en STATA
diseño2003 = svydesign(~segmento, strata = ~estrato, weights = ~expr, data = casen2003)


#################################################################################
# Creación de la variables provincia 
#################################################################################

library(stringi)

diseño2003$variables$aux = stri_length(diseño2003$variables$comu)

diseño2003$variables$provincia = diseño2003$variables$comu

diseño2003$variables = mutate(diseño2003$variables, provincia = 
                                ifelse(aux==5, stri_sub(comu,1,3), comu))

diseño2003$variables = mutate(diseño2003$variables, provincia = 
                                ifelse(aux==4, stri_sub(comu,1,2), provincia))

##########################################################################################
# Número de trabajadores por ocupación 2003
##########################################################################################

ciuo = read.csv("~/GoogleDrivePersonal/Observatorio Regional/Bases NENE/NENE/codigos_ciuo.csv")
ciuo = ciuo %>% group_by(etiqueta, codigo) %>% filter(row_number(codigo)==1)

ocupaciones4_2003 = svyby(~I(o7!=9999 & !is.na(o7)), by=~o7, design=subset(diseño2003, provincia==84),
                          svytotal, multicore=TRUE,
                          drop.empty.groups=FALSE, na.rm=TRUE) %>% 
  `colnames<-` (c("codigo", "no_ocupado", "ocupado", "se_no_ocupado", "se_ocupado")) %>% 
  select(codigo, ocupado, se_ocupado) %>% 
  mutate(cv = se_ocupado/ocupado) %>%   
  filter(codigo == 1314 | codigo == 2331 | codigo == 4115 |                                                                  
           codigo == 5122 | codigo == 5131 | codigo == 5220 | codigo == 6111 |
           codigo == 6112 | codigo == 6210 | codigo == 7124 | codigo == 8322 |
           codigo == 8324 | codigo == 9131 | codigo == 9132 | codigo == 9211) 

temp = xtabs(~o7, data=subset(diseño2003$variables, provincia==84)) %>% data.frame() 

ocupaciones4_2003 = merge(ocupaciones4_2003, temp, by.x = 'codigo', by.y = 'o7') %>% 
  mutate(nota = ifelse(cv>=0.25 | Freq<50, 'a',''), year = 2003) 

ocupaciones4_2003 = merge(ocupaciones4_2003, ciuo, by.x = 'codigo', by.y = 'codigo') 

write.csv(ocupaciones4_2003, "ocupados2003.csv")
##########################################################################################
# Concentración de la ocupación 2003
##########################################################################################
ocupaciones = c(1314,2331,4115,5122,5131,5220,6111,
                6112,6210,7124,8322,8324,9131,9132,9211)
concentracion_nuble2003 = lapply(ocupaciones,
                                 function(x) svyratio(~I(o7==x),
                                denominator=~I(o7!=9999 & !is.na(o7)),
                                subset(diseño2003, provincia==84), na.rm=TRUE))

concentracion_nuble2003. = unlist(lapply(concentracion_nuble2003, '[[', 1) ) 

ee.concentracion_nuble2003 = unlist(lapply(concentracion_nuble2003, SE))

concentracion_nuble2003__ = data.frame(concentracion_nuble2003., ee.concentracion_nuble2003) %>% 
  `colnames<-` (c("ocupados", "se")) %>% 
  mutate(cv=se/ocupados, ocupados=round(ocupados*100,1), codigo = ocupaciones)

temp = xtabs(~o7, subset(diseño2003$variables, provincia==84)) %>%
  data.frame() %>%   filter(o7 == 1314 |                                
                              o7 == 2331 | o7 == 4115 |                                                                  
                              o7 == 5122 | o7 == 5131 |                            
                              o7 == 5220 | o7 == 6111 |
                              o7 == 6112 | o7 == 6210 |                                                           
                              o7 == 7124 | o7 == 8322 |
                              o7 == 8324 | o7 == 9131 | 
                              o7 == 9132 | o7 == 9211)  

concentracion_nuble2003__$frecuencia = temp$Freq

concentracion_nuble2003 = merge(concentracion_nuble2003__, ciuo, by.x="codigo", by.y="codigo") %>% 
  mutate(year = 2003)

write.csv(concentracion_nuble2003, "concentracion2003.csv")
########################################################################
# Ingreso promedio 2003
########################################################################

# Con respecto a los ingresos originales, la encuesta CASEN tiene una particularidad. 
# aparecen varios individuos con ingreso 999999999, estos valores, claramente distorsionan 
# el ingreso promedio. 

max(subset(diseño2003$variables, provincia==84)$o18, na.rm=TRUE)

# me fue difícil encontrar el significado de esto en la documentación, por lo que asumiré
# que estos individuos se negaron a responder la encuesta, por 
# lo que serán tratados como valores perdidos, de esta forma no distorsionarán las cifras

diseño2003$variables = mutate(diseño2003$variables, o18 = ifelse(o18==99999999,NA, o18))

ingreso_nuble2003 = lapply(ocupaciones,
                           function(x) svyby(~o18, subset(diseño2003, provincia==84),
                                             by=~I(o7==x), svymean, na.rm=TRUE,
                                             na.rm.all = TRUE, drop.empty.groups = FALSE))

ingreso_nuble2003. = unlist(lapply(ingreso_nuble2003, '[[',2,2))
ee.ingreso_nuble2003 = unlist(lapply(ingreso_nuble2003,'[[',2,3))

ingreso_nuble2003__ = data.frame(ingreso = ingreso_nuble2003., se = ee.ingreso_nuble2003) %>% 
  mutate(cv = se/ingreso, ingreso=round(ingreso,0), codigo = ocupaciones) 

temp = xtabs(~o7, subset(diseño2003$variables, provincia==84)) %>%
  data.frame() %>%   filter(o7 == 1314 |                                
                              o7 == 2331 | o7 == 4115 |                                                                  
                              o7 == 5122 | o7 == 5131 |                            
                              o7 == 5220 | o7 == 6111 |
                              o7 == 6112 | o7 == 6210 |                                                           
                              o7 == 7124 | o7 == 8322 |
                              o7 == 8324 | o7 == 9131 | 
                              o7 == 9132 | o7 == 9211)  

ingreso_nuble2003__$frecuencia = temp$Freq

ingreso_nuble2003 = merge(ingreso_nuble2003__, ciuo, by.x ="codigo", by.y = "codigo") %>% 
  mutate(nota = ifelse(cv>=0.25 | frecuencia<50,"a",""), year = 2003)

write.csv(ingreso_nuble2003, "ingreso2003.csv")
########################################################################
# Promedio de años de escolaridad
########################################################################

escolaridad_nuble2003 = lapply(ocupaciones,
                               function(x) svyby(~esc, subset(diseño2003, provincia==84),
                                                 by=~I(o7==x), svymean, na.rm=TRUE,
                                                 drop.empty.groups = FALSE))

escolaridad_nuble2003. = unlist(lapply(escolaridad_nuble2003, '[[',2,2))
ee.escolaridad_nuble2003 = unlist(lapply(escolaridad_nuble2003,'[[',2,3))

escolaridad_nuble2003__ = data.frame(esc = escolaridad_nuble2003., se = ee.escolaridad_nuble2003) %>% 
  mutate(cv = se/esc, esc=round(esc,1), codigo = ocupaciones) 

temp = xtabs(~o7, subset(diseño2003$variables, provincia==84)) %>%
  data.frame() %>%   filter(o7 == 1314 |                                
                              o7 == 2331 | o7 == 4115 |                                                                  
                              o7 == 5122 | o7 == 5131 |                            
                              o7 == 5220 | o7 == 6111 |
                              o7 == 6112 | o7 == 6210 |                                                           
                              o7 == 7124 | o7 == 8322 |
                              o7 == 8324 | o7 == 9131 | 
                              o7 == 9132 | o7 == 9211)  

escolaridad_nuble2003__$frecuencia = temp$Freq

escolaridad_nuble2003 = merge(escolaridad_nuble2003__, ciuo, by.x ="codigo", by.y = "codigo") %>% 
  mutate(nota = ifelse(cv>=0.25 | frecuencia<50,"a",""), year = 2003)

write.csv(escolaridad_nuble2003, "escolaridad2003.csv")

# Para remover los objetos que no son de interés 
rm(list=ls(pattern="__$"))

