#########################################################################################
# Encuesta CASEN 1998
#########################################################################################
rm(list=ls())
setwd("~/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Encuesta de Caracterización Socioeconómica Nacional (CASEN)/")

list.files(pattern = ".csv$")

library(data.table)
library(dplyr)
library(survey)

casen1998 =  fread("Casen1998.csv", sep=",", header=TRUE,
                   select=c('o21','r','p', 'comu', 'segmento',
                            'estrato','f', 'o',  'expr', 'expc', 'o6', 'esc', 'yopraj'))

ingresos1998 = fread('ingresos_originales_casen_1998.csv', sep=",", header = TRUE, 
                     select=c('FOLIO', 'SEGMENTO', 'O', 'o16'))

casen1998 = merge(casen1998, ingresos1998,
                  by.x = c('segmento', 'f', 'o'), by.y = c('SEGMENTO','FOLIO', 'O'))

options(survey.lonely.psu="certainty") # equivalente a singleunit = certainty en STATA
diseño1998 = svydesign(~segmento, strata = ~estrato, weights = ~expr, data = casen1998)


#################################################################################
# Creación de la variables provincia 
#################################################################################

library(stringi)

diseño1998$variables$aux = stri_length(diseño1998$variables$comu)

diseño1998$variables$provincia = diseño1998$variables$comu

diseño1998$variables = mutate(diseño1998$variables, provincia = 
                                ifelse(aux==5, stri_sub(comu,1,3), comu))

diseño1998$variables = mutate(diseño1998$variables, provincia = 
                                ifelse(aux==4, stri_sub(comu,1,2), provincia))

##########################################################################################
# Número de trabajadores por ocupación 1998
##########################################################################################

ciuo = read.csv("~/GoogleDrivePersonal/Observatorio Regional/Bases NENE/NENE/codigos_ciuo.csv")
ciuo = ciuo %>% group_by(etiqueta, codigo) %>% filter(row_number(codigo)==1)

ocupaciones4_1998 = svyby(~I(o21==1), by=~o6, design=subset(diseño1998, provincia==84),
                          svytotal, multicore=TRUE,
                          drop.empty.groups=FALSE, na.rm=TRUE) %>% 
  `colnames<-` (c("codigo", "no_ocupado", "ocupado", "se_no_ocupado", "se_ocupado")) %>% 
  select(codigo, ocupado, se_ocupado) %>% 
  mutate(cv = se_ocupado/ocupado) %>%   
  filter(codigo == 1314 | codigo == 2331 | codigo == 4115 |                                                                  
           codigo == 5122 | codigo == 5131 | codigo == 5220 | codigo == 6111 |
           codigo == 6112 | codigo == 6210 | codigo == 7124 | codigo == 8322 |
           codigo == 8324 | codigo == 9131 | codigo == 9132 | codigo == 9211) 

temp = xtabs(~o6, data=subset(diseño1998$variables, provincia==84)) %>% data.frame() 

ocupaciones4_1998 = merge(ocupaciones4_1998, temp, by.x = 'codigo', by.y = 'o6') %>% 
  mutate(nota = ifelse(cv>=0.25 | Freq<50, 'a',''), year = 1998) 

ocupaciones4_1998 = merge(ocupaciones4_1998, ciuo, by.x = 'codigo', by.y = 'codigo') 

write.csv(ocupaciones4_1998, "ocupados1998.csv")
##########################################################################################
# Concentración de la ocupación 1998
##########################################################################################
ocupaciones = c(1314,2331,4115,5122,5131,5220,6111,
                6112,6210,7124,8322,8324,9131,9132,9211)
concentracion_nuble1998 = lapply(ocupaciones,
                         function(x) svyratio(~I(o6==x),denominator=~I(!is.na(o6)),
                        subset(diseño1998, provincia==84), na.rm=TRUE))

concentracion_nuble1998. = unlist(lapply(concentracion_nuble1998, '[[', 1) ) 

ee.concentracion_nuble1998 = unlist(lapply(concentracion_nuble1998, SE))

concentracion_nuble1998__ = data.frame(concentracion_nuble1998., ee.concentracion_nuble1998) %>% 
  `colnames<-` (c("ocupados", "se")) %>% 
  mutate(cv=se/ocupados, ocupados=round(ocupados*100,1), codigo = ocupaciones)

temp = xtabs(~o6, subset(diseño1998$variables, provincia==84)) %>%
  data.frame() %>%   filter(o6 == 1314 |                                
                              o6 == 2331 | o6 == 4115 |                                                                  
                              o6 == 5122 | o6 == 5131 |                            
                              o6 == 5220 | o6 == 6111 |
                              o6 == 6112 | o6 == 6210 |                                                           
                              o6 == 7124 | o6 == 8322 |
                              o6 == 8324 | o6 == 9131 | 
                              o6 == 9132 | o6 == 9211)  

concentracion_nuble1998__$frecuencia = temp$Freq

concentracion_nuble1998 = merge(concentracion_nuble1998__, ciuo, by.x="codigo", by.y="codigo") %>% 
  mutate(year = 1998)

write.csv(concentracion_nuble1998, "concentracion1998.csv")
########################################################################
# Ingreso promedio 1998
########################################################################

# Con respecto a los ingresos originales, la encuesta CASEN tiene una particularidad. 
# aparecen varios individuos con ingreso 999999999, estos valores, claramente distorsionan 
# el ingreso promedio. 

max(subset(diseño1998$variables, provincia==84)$o16, na.rm=TRUE)

# me fue difícil encontrar el significado de esto en la documentación, por lo que asumiré
# que estos individuos se negaron a responder la encuesta, por 
# lo que serán tratados como valores perdidos, de esta forma no distorsionarán las cifras

diseño1998$variables = mutate(diseño1998$variables, o16 = ifelse(o16==99999999,NA, o16))

ingreso_nuble1998 = lapply(ocupaciones,
                       function(x) svyby(~o16, subset(diseño1998, provincia==84),
                                         by=~I(o6==x), svymean, na.rm=TRUE,
                                         na.rm.all = TRUE, drop.empty.groups = FALSE))

ingreso_nuble1998. = unlist(lapply(ingreso_nuble1998, '[[',2,2))
ee.ingreso_nuble1998 = unlist(lapply(ingreso_nuble1998,'[[',2,3))

ingreso_nuble1998__ = data.frame(ingreso = ingreso_nuble1998., se = ee.ingreso_nuble1998) %>% 
  mutate(cv = se/ingreso, ingreso=round(ingreso,0), codigo = ocupaciones) 

temp = xtabs(~o6, subset(diseño1998$variables, provincia==84)) %>%
  data.frame() %>%   filter(o6 == 1314 |                                
                              o6 == 2331 | o6 == 4115 |                                                                  
                              o6 == 5122 | o6 == 5131 |                            
                              o6 == 5220 | o6 == 6111 |
                              o6 == 6112 | o6 == 6210 |                                                           
                              o6 == 7124 | o6 == 8322 |
                              o6 == 8324 | o6 == 9131 | 
                              o6 == 9132 | o6 == 9211)  

ingreso_nuble1998__$frecuencia = temp$Freq

ingreso_nuble1998 = merge(ingreso_nuble1998__, ciuo, by.x ="codigo", by.y = "codigo") %>% 
  mutate(nota = ifelse(cv>=0.25 | frecuencia<50,"a",""), year = 1998)

write.csv(ingreso_nuble1998, "ingreso1998.csv")
########################################################################
# Promedio de años de escolaridad
########################################################################

escolaridad_nuble1998 = lapply(ocupaciones,
                           function(x) svyby(~esc, subset(diseño1998, provincia==84),
                                             by=~I(o6==x), svymean, na.rm=TRUE,
                                             drop.empty.groups = FALSE))

escolaridad_nuble1998. = unlist(lapply(escolaridad_nuble1998, '[[',2,2))
ee.escolaridad_nuble1998 = unlist(lapply(escolaridad_nuble1998,'[[',2,3))

escolaridad_nuble1998__ = data.frame(esc = escolaridad_nuble1998., se = ee.escolaridad_nuble1998) %>% 
  mutate(cv = se/esc, esc=round(esc,1), codigo = ocupaciones) 

temp = xtabs(~o6, subset(diseño1998$variables, provincia==84)) %>%
  data.frame() %>%   filter(o6 == 1314 |                                
                              o6 == 2331 | o6 == 4115 |                                                                  
                              o6 == 5122 | o6 == 5131 |                            
                              o6 == 5220 | o6 == 6111 |
                              o6 == 6112 | o6 == 6210 |                                                           
                              o6 == 7124 | o6 == 8322 |
                              o6 == 8324 | o6 == 9131 | 
                              o6 == 9132 | o6 == 9211)  

escolaridad_nuble1998__$frecuencia = temp$Freq

escolaridad_nuble1998 = merge(escolaridad_nuble1998__, ciuo, by.x ="codigo", by.y = "codigo") %>% 
  mutate(nota = ifelse(cv>=0.25 | frecuencia<50,"a",""), year = 1998)

write.csv(escolaridad_nuble1998, "escolaridad1998.csv")

# Para remover los objetos que no son de interés 
rm(list=ls(pattern="__$"))

