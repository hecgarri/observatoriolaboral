#########################################################################################
# Encuesta CASEN 2011
#########################################################################################

rm(list=ls())
setwd("~/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Encuesta de Caracterización Socioeconómica Nacional (CASEN)/")

list.files(pattern = ".csv$")

library(data.table)
library(dplyr)
library(survey)

casen2011 =  fread("casen2011spss_03122013_spss.csv", sep=",", header=TRUE,
                   select=c('varunit','folio', 'o', 'region',
                            'comuna', 'varstrat','expr_r2', 'expc_r2', 'oficio4', 'esc'))

ingresos2011 = fread('Ingresos MN 2011.csv', sep=",", header = TRUE, 
                     select=c('folio', 'o', 'yoprCor'))

casen2011 = merge(casen2011, ingresos2011,
                  by.x = c('folio', 'o'),
                  by.y = c('folio','o'))

names(casen2011) = tolower(names(casen2011))

diseño2011 = svydesign(~varunit, strata = ~varstrat, weights = ~expr_r2, data = casen2011)

#################################################################################
# Creación de la variables provincia 
#################################################################################

library(stringi)

diseño2011$variables$aux = stri_length(diseño2011$variables$comuna)

diseño2011$variables$provincia = diseño2011$variables$comuna

diseño2011$variables = mutate(diseño2011$variables, provincia = 
                                ifelse(aux==5, stri_sub(comuna,1,3), comuna))

diseño2011$variables = mutate(diseño2011$variables, provincia = 
                                ifelse(aux==4, stri_sub(comuna,1,2), provincia))

##########################################################################################
# Número de trabajadores por ocupación 2011
##########################################################################################

ciuo = read.csv("~/GoogleDrivePersonal/Observatorio Regional/Bases NENE/NENE/codigos_ciuo.csv")
ciuo = ciuo %>% group_by(etiqueta, codigo) %>% filter(row_number(codigo)==1)

ocupaciones4_2011 = svyby(~I(oficio4!=9999 & !is.na(oficio4)), by=~oficio4, design=subset(diseño2011, provincia==84),
                          svytotal, multicore=TRUE,
                          drop.empty.groups=FALSE, na.rm=TRUE) %>% 
  `colnames<-` (c("codigo", "no_ocupado", "ocupado", "se_no_ocupado", "se_ocupado")) %>% 
  select(codigo, ocupado, se_ocupado) %>% 
  mutate(cv = se_ocupado/ocupado) %>%   
  filter(codigo == 1314 | codigo == 2331 | codigo == 4115 |                                                                  
           codigo == 5122 | codigo == 5131 | codigo == 5220 | codigo == 6111 |
           codigo == 6112 | codigo == 6210 | codigo == 7124 | codigo == 8322 |
           codigo == 8324 | codigo == 9131 | codigo == 9132 | codigo == 9211) 

temp = xtabs(~oficio4, data=subset(diseño2011$variables, provincia==84)) %>% data.frame() 

ocupaciones4_2011 = merge(ocupaciones4_2011, temp, by.x = 'codigo', by.y = 'oficio4') %>% 
  mutate(nota = ifelse(cv>=0.25 | Freq<50, 'a',''), year = 2011) 

ocupaciones4_2011 = merge(ocupaciones4_2011, ciuo, by.x = 'codigo', by.y = 'codigo') 

write.csv(ocupaciones4_2011, "ocupados2011.csv")
##########################################################################################
# Concentración de la ocupación 2011
##########################################################################################
ocupaciones = c(1314,2331,4115,5122,5131,5220,6111,
                6112,6210,7124,8322,8324,9131,9132,9211)
concentracion_nuble2011 = lapply(ocupaciones,
                                 function(x) svyratio(~I(oficio4==x),
                                                      denominator=~I(oficio4!=9999 & !is.na(oficio4)),
                                                      subset(diseño2011, provincia==84), na.rm=TRUE))

concentracion_nuble2011. = unlist(lapply(concentracion_nuble2011, '[[', 1) ) 

ee.concentracion_nuble2011 = unlist(lapply(concentracion_nuble2011, SE))

concentracion_nuble2011__ = data.frame(concentracion_nuble2011., ee.concentracion_nuble2011) %>% 
  `colnames<-` (c("ocupados", "se")) %>% 
  mutate(cv=se/ocupados, ocupados=round(ocupados*100,1), codigo = ocupaciones)

temp = xtabs(~oficio4, subset(diseño2011$variables, provincia==84)) %>%
  data.frame() %>%   filter(oficio4 == 1314 |                                
                              oficio4 == 2331 | oficio4 == 4115 |                                                                  
                              oficio4 == 5122 | oficio4 == 5131 |                            
                              oficio4 == 5220 | oficio4 == 6111 |
                              oficio4 == 6112 | oficio4 == 6210 |                                                           
                              oficio4 == 7124 | oficio4 == 8322 |
                              oficio4 == 8324 | oficio4 == 9131 | 
                              oficio4 == 9132 | oficio4 == 9211)  

concentracion_nuble2011__$frecuencia = temp$Freq

concentracion_nuble2011 = merge(concentracion_nuble2011__, ciuo, by.x="codigo", by.y="codigo") %>% 
  mutate(year = 2011)

write.csv(concentracion_nuble2011, "concentracion2011.csv")
########################################################################
# Ingreso promedio 2011
########################################################################

# Con respecto a los ingresos originales, la encuesta CASEN tiene una particularidad. 
# aparecen varios individuos con ingreso 999999999, estos valores, claramente distorsionan 
# el ingreso promedio. 

max(subset(diseño2011$variables, provincia==84)$yoprcor, na.rm=TRUE)

# me fue difícil encontrar el significado de esto en la documentación, por lo que asumiré
# que estos individuos se negaron a responder la encuesta, por 
# lo que serán tratados como valores perdidos, de esta forma no distorsionarán las cifras

diseño2011$variables = mutate(diseño2011$variables, yoprcor = ifelse(yoprcor==99999999,NA, yoprcor))

ingreso_nuble2011 = lapply(ocupaciones,
                           function(x) svyby(~yoprcor, subset(diseño2011, provincia==84),
                                             by=~I(oficio4==x), svymean, na.rm=TRUE,
                                             na.rm.all = TRUE, drop.empty.groups = FALSE))

ingreso_nuble2011. = unlist(lapply(ingreso_nuble2011, '[[',2,2))
ee.ingreso_nuble2011 = unlist(lapply(ingreso_nuble2011,'[[',2,3))

ingreso_nuble2011__ = data.frame(ingreso = ingreso_nuble2011., se = ee.ingreso_nuble2011) %>% 
  mutate(cv = se/ingreso, ingreso=round(ingreso,0), codigo = ocupaciones) 

temp = xtabs(~oficio4, subset(diseño2011$variables, provincia==84)) %>%
  data.frame() %>%   filter(oficio4 == 1314 |                                
                              oficio4 == 2331 | oficio4 == 4115 |                                                                  
                              oficio4 == 5122 | oficio4 == 5131 |                            
                              oficio4 == 5220 | oficio4 == 6111 |
                              oficio4 == 6112 | oficio4 == 6210 |                                                           
                              oficio4 == 7124 | oficio4 == 8322 |
                              oficio4 == 8324 | oficio4 == 9131 | 
                              oficio4 == 9132 | oficio4 == 9211)  

ingreso_nuble2011__$frecuencia = temp$Freq

ingreso_nuble2011 = merge(ingreso_nuble2011__, ciuo, by.x ="codigo", by.y = "codigo") %>% 
  mutate(nota = ifelse(cv>=0.25 | frecuencia<50,"a",""), year = 2011)

write.csv(ingreso_nuble2011, "ingreso2011.csv")
########################################################################
# Promedio de años de escolaridad
########################################################################

escolaridad_nuble2011 = lapply(ocupaciones,
                               function(x) svyby(~esc, subset(diseño2011, provincia==84),
                                                 by=~I(oficio4==x), svymean, na.rm=TRUE,
                                                 drop.empty.groups = FALSE))

escolaridad_nuble2011. = unlist(lapply(escolaridad_nuble2011, '[[',2,2))
ee.escolaridad_nuble2011 = unlist(lapply(escolaridad_nuble2011,'[[',2,3))

escolaridad_nuble2011__ = data.frame(esc = escolaridad_nuble2011., se = ee.escolaridad_nuble2011) %>% 
  mutate(cv = se/esc, esc=round(esc,1), codigo = ocupaciones) 

temp = xtabs(~oficio4, subset(diseño2011$variables, provincia==84)) %>%
  data.frame() %>%   filter(oficio4 == 1314 |                                
                              oficio4 == 2331 | oficio4 == 4115 |                                                                  
                              oficio4 == 5122 | oficio4 == 5131 |                            
                              oficio4 == 5220 | oficio4 == 6111 |
                              oficio4 == 6112 | oficio4 == 6210 |                                                           
                              oficio4 == 7124 | oficio4 == 8322 |
                              oficio4 == 8324 | oficio4 == 9131 | 
                              oficio4 == 9132 | oficio4 == 9211)  

escolaridad_nuble2011__$frecuencia = temp$Freq

escolaridad_nuble2011 = merge(escolaridad_nuble2011__, ciuo, by.x ="codigo", by.y = "codigo") %>% 
  mutate(nota = ifelse(cv>=0.25 | frecuencia<50,"a",""), year = 2011)

write.csv(escolaridad_nuble2011, "escolaridad2011.csv")

# Para remover los objetos que no son de interés 
rm(list=ls(pattern="__$"))

