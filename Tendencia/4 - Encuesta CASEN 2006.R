#########################################################################################
# Encuesta CASEN 2006
#########################################################################################

rm(list=ls())
setwd("~/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Encuesta de Caracterización Socioeconómica Nacional (CASEN)/")

list.files(pattern = ".csv$")

library(data.table)
library(dplyr)
library(survey)

casen2006 =  fread("casen2006.csv") %>% 
  setNames(make.names(names(.), unique = TRUE)) %>% 
  select(F, SEG, R, COMUNA,O, P, ESTRATO, EXPR, EXPC, C_O11, ESC)

ingresos2006 = fread('Ingresos MN 2006.csv', sep=",", header = TRUE, 
                     select=c('f', 'seg', 'o', 'yoprCor'))

casen2006 = merge(casen2006, ingresos2006,
                  by.x = c('SEG', 'F', 'O'), by.y = c('seg','f', 'o'))

names(casen2006) = tolower(names(casen2006))

options(survey.lonely.psu="certainty") # equivalente a singleunit = certainty en STATA
diseño2006 = svydesign(~seg, strata = ~estrato, weights = ~expr, data = casen2006)

#################################################################################
# Creación de la variables provincia 
#################################################################################

library(stringi)

diseño2006$variables$aux = stri_length(diseño2006$variables$comuna)

diseño2006$variables$provincia = diseño2006$variables$comuna

diseño2006$variables = mutate(diseño2006$variables, provincia = 
                                ifelse(aux==5, stri_sub(comuna,1,3), comuna))

diseño2006$variables = mutate(diseño2006$variables, provincia = 
                                ifelse(aux==4, stri_sub(comuna,1,2), provincia))

##########################################################################################
# Número de trabajadores por ocupación 2006
##########################################################################################

ciuo = read.csv("~/GoogleDrivePersonal/Observatorio Regional/Bases NENE/NENE/codigos_ciuo.csv")
ciuo = ciuo %>% group_by(etiqueta, codigo) %>% filter(row_number(codigo)==1)

ocupaciones4_2006 = svyby(~I(c_o11!=9999 & !is.na(c_o11)), by=~c_o11, design=subset(diseño2006, provincia==84),
                          svytotal, multicore=TRUE,
                          drop.empty.groups=FALSE, na.rm=TRUE) %>% 
  `colnames<-` (c("codigo", "no_ocupado", "ocupado", "se_no_ocupado", "se_ocupado")) %>% 
  select(codigo, ocupado, se_ocupado) %>% 
  mutate(cv = se_ocupado/ocupado) %>%   
  filter(codigo == 1314 | codigo == 2331 | codigo == 4115 |                                                                  
           codigo == 5122 | codigo == 5131 | codigo == 5220 | codigo == 6111 |
           codigo == 6112 | codigo == 6210 | codigo == 7124 | codigo == 8322 |
           codigo == 8324 | codigo == 9131 | codigo == 9132 | codigo == 9211) 

temp = xtabs(~c_o11, data=subset(diseño2006$variables, provincia==84)) %>% data.frame() 

ocupaciones4_2006 = merge(ocupaciones4_2006, temp, by.x = 'codigo', by.y = 'c_o11') %>% 
  mutate(nota = ifelse(cv>=0.25 | Freq<50, 'a',''), year = 2006) 

ocupaciones4_2006 = merge(ocupaciones4_2006, ciuo, by.x = 'codigo', by.y = 'codigo') 

write.csv(ocupaciones4_2006, "ocupados2006.csv")
##########################################################################################
# Concentración de la ocupación 2006
##########################################################################################
ocupaciones = c(1314,2331,4115,5122,5131,5220,6111,
                6112,6210,7124,8322,8324,9131,9132,9211)
concentracion_nuble2006 = lapply(ocupaciones,
                                 function(x) svyratio(~I(c_o11==x),
                                                      denominator=~I(c_o11!=9999 & !is.na(c_o11)),
                                                      subset(diseño2006, provincia==84), na.rm=TRUE))

concentracion_nuble2006. = unlist(lapply(concentracion_nuble2006, '[[', 1) ) 

ee.concentracion_nuble2006 = unlist(lapply(concentracion_nuble2006, SE))

concentracion_nuble2006__ = data.frame(concentracion_nuble2006., ee.concentracion_nuble2006) %>% 
  `colnames<-` (c("ocupados", "se")) %>% 
  mutate(cv=se/ocupados, ocupados=round(ocupados*100,1), codigo = ocupaciones)

temp = xtabs(~c_o11, subset(diseño2006$variables, provincia==84)) %>%
  data.frame() %>%   filter(c_o11 == 1314 |                                
                              c_o11 == 2331 | c_o11 == 4115 |                                                                  
                              c_o11 == 5122 | c_o11 == 5131 |                            
                              c_o11 == 5220 | c_o11 == 6111 |
                              c_o11 == 6112 | c_o11 == 6210 |                                                           
                              c_o11 == 7124 | c_o11 == 8322 |
                              c_o11 == 8324 | c_o11 == 9131 | 
                              c_o11 == 9132 | c_o11 == 9211)  

concentracion_nuble2006__$frecuencia = temp$Freq

concentracion_nuble2006 = merge(concentracion_nuble2006__, ciuo, by.x="codigo", by.y="codigo") %>% 
  mutate(year = 2006)

write.csv(concentracion_nuble2006, "concentracion2006.csv")
########################################################################
# Ingreso promedio 2006
########################################################################

# Con respecto a los ingresos originales, la encuesta CASEN tiene una particularidad. 
# aparecen varios individuos con ingreso 999999999, estos valores, claramente distorsionan 
# el ingreso promedio. 

max(subset(diseño2006$variables, provincia==84)$yoprcor, na.rm=TRUE)

# me fue difícil encontrar el significado de esto en la documentación, por lo que asumiré
# que estos individuos se negaron a responder la encuesta, por 
# lo que serán tratados como valores perdidos, de esta forma no distorsionarán las cifras

diseño2006$variables = mutate(diseño2006$variables, yoprcor = ifelse(yoprcor==99999999,NA, yoprcor))

ingreso_nuble2006 = lapply(ocupaciones,
                           function(x) svyby(~yoprcor, subset(diseño2006, provincia==84),
                                             by=~I(c_o11==x), svymean, na.rm=TRUE,
                                             na.rm.all = TRUE, drop.empty.groups = FALSE))

ingreso_nuble2006. = unlist(lapply(ingreso_nuble2006, '[[',2,2))
ee.ingreso_nuble2006 = unlist(lapply(ingreso_nuble2006,'[[',2,3))

ingreso_nuble2006__ = data.frame(ingreso = ingreso_nuble2006., se = ee.ingreso_nuble2006) %>% 
  mutate(cv = se/ingreso, ingreso=round(ingreso,0), codigo = ocupaciones) 

temp = xtabs(~c_o11, subset(diseño2006$variables, provincia==84)) %>%
  data.frame() %>%   filter(c_o11 == 1314 |                                
                              c_o11 == 2331 | c_o11 == 4115 |                                                                  
                              c_o11 == 5122 | c_o11 == 5131 |                            
                              c_o11 == 5220 | c_o11 == 6111 |
                              c_o11 == 6112 | c_o11 == 6210 |                                                           
                              c_o11 == 7124 | c_o11 == 8322 |
                              c_o11 == 8324 | c_o11 == 9131 | 
                              c_o11 == 9132 | c_o11 == 9211)  

ingreso_nuble2006__$frecuencia = temp$Freq

ingreso_nuble2006 = merge(ingreso_nuble2006__, ciuo, by.x ="codigo", by.y = "codigo") %>% 
  mutate(nota = ifelse(cv>=0.25 | frecuencia<50,"a",""), year = 2006)

write.csv(ingreso_nuble2006, "ingreso2006.csv")
########################################################################
# Promedio de años de escolaridad
########################################################################

escolaridad_nuble2006 = lapply(ocupaciones,
                               function(x) svyby(~esc, subset(diseño2006, provincia==84),
                                                 by=~I(c_o11==x), svymean, na.rm=TRUE,
                                                 drop.empty.groups = FALSE))

escolaridad_nuble2006. = unlist(lapply(escolaridad_nuble2006, '[[',2,2))
ee.escolaridad_nuble2006 = unlist(lapply(escolaridad_nuble2006,'[[',2,3))

escolaridad_nuble2006__ = data.frame(esc = escolaridad_nuble2006., se = ee.escolaridad_nuble2006) %>% 
  mutate(cv = se/esc, esc=round(esc,1), codigo = ocupaciones) 

temp = xtabs(~c_o11, subset(diseño2006$variables, provincia==84)) %>%
  data.frame() %>%   filter(c_o11 == 1314 |                                
                              c_o11 == 2331 | c_o11 == 4115 |                                                                  
                              c_o11 == 5122 | c_o11 == 5131 |                            
                              c_o11 == 5220 | c_o11 == 6111 |
                              c_o11 == 6112 | c_o11 == 6210 |                                                           
                              c_o11 == 7124 | c_o11 == 8322 |
                              c_o11 == 8324 | c_o11 == 9131 | 
                              c_o11 == 9132 | c_o11 == 9211)  

escolaridad_nuble2006__$frecuencia = temp$Freq

escolaridad_nuble2006 = merge(escolaridad_nuble2006__, ciuo, by.x ="codigo", by.y = "codigo") %>% 
  mutate(nota = ifelse(cv>=0.25 | frecuencia<50,"a",""), year = 2006)

write.csv(escolaridad_nuble2006, "escolaridad2006.csv")

# Para remover los objetos que no son de interés 
rm(list=ls(pattern="__$"))

