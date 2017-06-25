#########################################################################################
# Encuesta CASEN 2000
#########################################################################################
rm(list=ls())
setwd("~/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Encuesta de Caracterización Socioeconómica Nacional (CASEN)/")

library(data.table)
library(dplyr)
library(survey)

setwd("~/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Encuesta de Caracterización Socioeconómica Nacional (CASEN)/")

list.files(pattern = ".csv$")

library(data.table)

casen2000 =  fread("casen2000_Spss.csv", sep=",", header=TRUE,
                   select=c('activ','r','p', 'comu', 'segmento',
                  'estrato','folio', 'o',  'expr', 'expc', 'o8',
                  'esc', 'yopraj'))

ingresos2000 = fread('ingresos_originales_CASEN_2000.csv', sep=",", header = TRUE, 
                     select=c('f', 'segmento', 'o', 'o18'))

casen2000 = merge(casen2000, ingresos2000,
                  by.x = c('segmento', 'folio', 'o'), by.y = c('segmento','f', 'o'))

options(survey.lonely.psu="certainty") # equivalente a singleunit = certainty en STATA
diseño2000 = svydesign(~segmento, strata = ~estrato, weights = ~expr, data = casen2000)


#################################################################################
# Creación de la variables provincia 
#################################################################################

library(stringi)

diseño2000$variables$aux = stri_length(diseño2000$variables$comu)

diseño2000$variables$provincia = diseño2000$variables$comu

diseño2000$variables = mutate(diseño2000$variables, provincia = 
                                ifelse(aux==5, stri_sub(comu,1,3), comu))

diseño2000$variables = mutate(diseño2000$variables, provincia = 
                                ifelse(aux==4, stri_sub(comu,1,2), provincia))

##########################################################################################
# Número de trabajadores por ocupación 2000
##########################################################################################

ciuo = read.csv("~/GoogleDrivePersonal/Observatorio Regional/Bases NENE/NENE/codigos_ciuo.csv")
ciuo = ciuo %>% group_by(etiqueta, codigo) %>% filter(row_number(codigo)==1)

ocupaciones4_2000 = svyby(~I(!is.na(o8)), by=~o8, design=subset(diseño2000, provincia==84),
                          svytotal, multicore=TRUE,
                          drop.empty.groups=FALSE, na.rm=TRUE) %>% 
  `colnames<-` (c("codigo", "no_ocupado", "ocupado", "se_no_ocupado", "se_ocupado")) %>% 
  select(codigo, ocupado, se_ocupado) %>% 
  mutate(cv = se_ocupado/ocupado) %>%   
  filter(codigo == 1314 | codigo == 2331 | codigo == 4115 |                                                                  
           codigo == 5122 | codigo == 5131 | codigo == 5220 | codigo == 6111 |
           codigo == 6112 | codigo == 6210 | codigo == 7124 | codigo == 8322 |
           codigo == 8324 | codigo == 9131 | codigo == 9132 | codigo == 9211) 

temp = xtabs(~o8, data=subset(diseño2000$variables, provincia==84)) %>% data.frame() 

ocupaciones4_2000 = merge(ocupaciones4_2000, temp, by.x = 'codigo', by.y = 'o8') %>% 
  mutate(nota = ifelse(cv>=0.25 | Freq<50, 'a',''), year = 2000) 

ocupaciones4_2000 = merge(ocupaciones4_2000, ciuo, by.x = 'codigo', by.y = 'codigo') 

write.csv(ocupaciones4_2000, "ocupados2000.csv")
##########################################################################################
# Concentración de la ocupación 2000
##########################################################################################
ocupaciones = c(1314,2331,4115,5122,5131,5220,6111,
                6112,6210,7124,8322,8324,9131,9132,9211)
concentracion_nuble2000 = lapply(ocupaciones,
                             function(x) svyratio(~I(o8==x),denominator=~I(!is.na(o8)),
                                                  subset(diseño2000, provincia==84), na.rm=TRUE))

concentracion_nuble2000. = unlist(lapply(concentracion_nuble2000, '[[', 1) ) 

ee.concentracion_nuble2000 = unlist(lapply(concentracion_nuble2000, SE))

concentracion_nuble2000__ = data.frame(concentracion_nuble2000., ee.concentracion_nuble2000) %>% 
  `colnames<-` (c("ocupados", "se")) %>% 
  mutate(cv=se/ocupados, ocupados=round(ocupados*100,1), codigo = ocupaciones)

temp = xtabs(~o8, subset(diseño2000$variables, provincia==84)) %>%
  data.frame() %>%   filter(o8 == 1314 |                                
                    o8 == 2331 | o8 == 4115 |                                                                  
                    o8 == 5122 | o8 == 5131 |                            
                    o8 == 5220 | o8 == 6111 |
                    o8 == 6112 | o8 == 6210 |                                                           
                    o8 == 7124 | o8 == 8322 |
                    o8 == 8324 | o8 == 9131 | 
                    o8 == 9132 | o8 == 9211)  

concentracion_nuble2000__$frecuencia = temp$Freq

concentracion_nuble2000 = merge(concentracion_nuble2000__, ciuo, by.x="codigo", by.y="codigo") %>% 
  mutate(year = 2000)

write.csv(concentracion_nuble2000, "concentracion2000.csv")
########################################################################
# Ingreso promedio 2000
########################################################################

# Con respecto a los ingresos originales, la encuesta CASEN tiene una particularidad. 
# aparecen varios individuos con ingreso 999999999, estos valores, claramente distorsionan 
# el ingreso promedio. 

max(subset(diseño2000$variables, provincia==84)$o18, na.rm=TRUE)

# me fue difícil encontrar el significado de esto en la documentación, por lo que asumiré
# que estos individuos se negaron a responder la encuesta, por 
# lo que serán tratados como valores perdidos, de esta forma no distorsionarán las cifras

diseño2000$variables = mutate(diseño2000$variables, o18 = ifelse(o18==99999999,NA, o18))

ingreso_nuble2000 = lapply(ocupaciones,
                     function(x) svyby(~o18, subset(diseño2000, provincia==84),
                     by=~I(o8==x), svymean, na.rm=TRUE,
                     na.rm.all = TRUE, drop.empty.groups = FALSE))

ingreso_nuble2000. = unlist(lapply(ingreso_nuble2000, '[[',2,2))
ee.ingreso_nuble2000 = unlist(lapply(ingreso_nuble2000,'[[',2,3))

ingreso_nuble2000__ = data.frame(ingreso = ingreso_nuble2000., se = ee.ingreso_nuble2000) %>% 
  mutate(cv = se/ingreso, ingreso=round(ingreso,0), codigo = ocupaciones) 

temp = xtabs(~o8, subset(diseño2000$variables, provincia==84)) %>%
  data.frame() %>%   filter(o8 == 1314 |                                
                            o8 == 2331 | o8 == 4115 |                                                                  
                            o8 == 5122 | o8 == 5131 |                            
                            o8 == 5220 | o8 == 6111 |
                            o8 == 6112 | o8 == 6210 |                                                           
                            o8 == 7124 | o8 == 8322 |
                            o8 == 8324 | o8 == 9131 | 
                            o8 == 9132 | o8 == 9211)  

ingreso_nuble2000__$frecuencia = temp$Freq

ingreso_nuble2000 = merge(ingreso_nuble2000__, ciuo, by.x ="codigo", by.y = "codigo") %>% 
  mutate(nota = ifelse(cv>=0.25 | frecuencia<50,"a",""), year = 2000)

write.csv(ingreso_nuble2000, "ingreso2000.csv")

########################################################################
# Promedio de años de escolaridad
########################################################################

escolaridad_nuble2000 = lapply(ocupaciones,
                           function(x) svyby(~esc, subset(diseño2000, provincia==84),
                                             by=~I(o8==x), svymean, na.rm=TRUE,
                                             drop.empty.groups = FALSE))

escolaridad_nuble2000. = unlist(lapply(escolaridad_nuble2000, '[[',2,2))
ee.escolaridad_nuble2000 = unlist(lapply(escolaridad_nuble2000,'[[',2,3))

escolaridad_nuble2000__ = data.frame(esc = escolaridad_nuble2000., se = ee.escolaridad_nuble2000) %>% 
  mutate(cv = se/esc, esc=round(esc,1), codigo = ocupaciones) 

  temp = xtabs(~o8, subset(diseño2000$variables, provincia==84)) %>%
  data.frame() %>%   filter(o8 == 1314 |                                
                              o8 == 2331 | o8 == 4115 |                                                                  
                              o8 == 5122 | o8 == 5131 |                            
                              o8 == 5220 | o8 == 6111 |
                              o8 == 6112 | o8 == 6210 |                                                           
                              o8 == 7124 | o8 == 8322 |
                              o8 == 8324 | o8 == 9131 | 
                              o8 == 9132 | o8 == 9211)  

escolaridad_nuble2000__$frecuencia = temp$Freq

escolaridad_nuble2000 = merge(escolaridad_nuble2000__, ciuo, by.x ="codigo", by.y = "codigo") %>% 
mutate(nota = ifelse(cv>=0.25 | frecuencia<50,"a",""), year = 2000)

write.csv(escolaridad_nuble2000, "escolaridad2000.csv")

# Para remover los objetos que no son de interés 
rm(list=ls(pattern="__$"))
