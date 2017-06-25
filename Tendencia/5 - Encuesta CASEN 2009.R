#########################################################################################
# Encuesta CASEN 2009
#########################################################################################

rm(list=ls())
setwd("~/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Encuesta de Caracterización Socioeconómica Nacional (CASEN)/")

list.files(pattern = ".csv$")

library(data.table)
library(dplyr)
library(survey)

casen2009 =  fread("Casen2009Spss.csv", sep=",", header=TRUE,
              select=c('SEGMENTO', 'IDVIV','FOLIO', 'O', 'REGION',
              'PROVINCIA', 'COMUNA', 'ESTRATO','EXPR', 'EXPC', 'C_O12', 'ESC'))

ingresos2009 = fread('Ingresos MN 2009.csv', sep=",", header = TRUE, 
                     select=c('IDVIV', 'SEGMENTO', 'O', 'yoprCor'))

casen2009 = merge(casen2009, ingresos2009,
                  by.x = c('SEGMENTO','IDVIV', 'O'),
                  by.y = c('SEGMENTO','IDVIV' ,'O'))

names(casen2009) = tolower(names(casen2009))

options(survey.lonely.psu="certainty") # equivalente a singleunit = certainty en STATA
diseño2009 = svydesign(~segmento, strata = ~estrato, weights = ~expr, data = casen2009)

#################################################################################
# Creación de la variables provincia 
#################################################################################

library(stringi)

diseño2009$variables$aux = stri_length(diseño2009$variables$comuna)

diseño2009$variables$provincia = diseño2009$variables$comuna

diseño2009$variables = mutate(diseño2009$variables, provincia = 
                                ifelse(aux==5, stri_sub(comuna,1,3), comuna))

diseño2009$variables = mutate(diseño2009$variables, provincia = 
                                ifelse(aux==4, stri_sub(comuna,1,2), provincia))

##########################################################################################
# Número de trabajadores por ocupación 2009
##########################################################################################

ciuo = read.csv("~/GoogleDrivePersonal/Observatorio Regional/Bases NENE/NENE/codigos_ciuo.csv")
ciuo = ciuo %>% group_by(etiqueta, codigo) %>% filter(row_number(codigo)==1)

ocupaciones4_2009 = svyby(~I(c_o12!=9999 & !is.na(c_o12)), by=~c_o12, design=subset(diseño2009, provincia==84),
                          svytotal, multicore=TRUE,
                          drop.empty.groups=FALSE, na.rm=TRUE) %>% 
  `colnames<-` (c("codigo", "no_ocupado", "ocupado", "se_no_ocupado", "se_ocupado")) %>% 
  select(codigo, ocupado, se_ocupado) %>% 
  mutate(cv = se_ocupado/ocupado) %>%   
  filter(codigo == 1314 | codigo == 2331 | codigo == 4115 |                                                                  
           codigo == 5122 | codigo == 5131 | codigo == 5220 | codigo == 6111 |
           codigo == 6112 | codigo == 6210 | codigo == 7124 | codigo == 8322 |
           codigo == 8324 | codigo == 9131 | codigo == 9132 | codigo == 9211) 

temp = xtabs(~c_o12, data=subset(diseño2009$variables, provincia==84)) %>% data.frame() 

ocupaciones4_2009 = merge(ocupaciones4_2009, temp, by.x = 'codigo', by.y = 'c_o12') %>% 
  mutate(nota = ifelse(cv>=0.25 | Freq<50, 'a',''), year = 2009) 

ocupaciones4_2009 = merge(ocupaciones4_2009, ciuo, by.x = 'codigo', by.y = 'codigo') 

write.csv(ocupaciones4_2009, "ocupados2009.csv")
##########################################################################################
# Concentración de la ocupación 2009
##########################################################################################
ocupaciones = c(1314,2331,4115,5122,5131,5220,6111,
                6112,6210,7124,8322,8324,9131,9132,9211)
concentracion_nuble2009 = lapply(ocupaciones,
                                 function(x) svyratio(~I(c_o12==x),
                                                      denominator=~I(c_o12!=9999 & !is.na(c_o12)),
                                                      subset(diseño2009, provincia==84), na.rm=TRUE))

concentracion_nuble2009. = unlist(lapply(concentracion_nuble2009, '[[', 1) ) 

ee.concentracion_nuble2009 = unlist(lapply(concentracion_nuble2009, SE))

concentracion_nuble2009__ = data.frame(concentracion_nuble2009., ee.concentracion_nuble2009) %>% 
  `colnames<-` (c("ocupados", "se")) %>% 
  mutate(cv=se/ocupados, ocupados=round(ocupados*100,1), codigo = ocupaciones)

temp = xtabs(~c_o12, subset(diseño2009$variables, provincia==84)) %>%
  data.frame() %>%   filter(c_o12 == 1314 |                                
                              c_o12 == 2331 | c_o12 == 4115 |                                                                  
                              c_o12 == 5122 | c_o12 == 5131 |                            
                              c_o12 == 5220 | c_o12 == 6111 |
                              c_o12 == 6112 | c_o12 == 6210 |                                                           
                              c_o12 == 7124 | c_o12 == 8322 |
                              c_o12 == 8324 | c_o12 == 9131 | 
                              c_o12 == 9132 | c_o12 == 9211)  

concentracion_nuble2009__$frecuencia = temp$Freq

concentracion_nuble2009 = merge(concentracion_nuble2009__, ciuo, by.x="codigo", by.y="codigo") %>% 
  mutate(year = 2009)

write.csv(concentracion_nuble2009, "concentracion2009.csv")
########################################################################
# Ingreso promedio 2009
########################################################################

# Con respecto a los ingresos originales, la encuesta CASEN tiene una particularidad. 
# aparecen varios individuos con ingreso 999999999, estos valores, claramente distorsionan 
# el ingreso promedio. 

max(subset(diseño2009$variables, provincia==84)$yoprcor, na.rm=TRUE)

# me fue difícil encontrar el significado de esto en la documentación, por lo que asumiré
# que estos individuos se negaron a responder la encuesta, por 
# lo que serán tratados como valores perdidos, de esta forma no distorsionarán las cifras

diseño2009$variables = mutate(diseño2009$variables, yoprcor = ifelse(yoprcor==99999999,NA, yoprcor))

ingreso_nuble2009 = lapply(ocupaciones,
                           function(x) svyby(~yoprcor, subset(diseño2009, provincia==84),
                                             by=~I(c_o12==x), svymean, na.rm=TRUE,
                                             na.rm.all = TRUE, drop.empty.groups = FALSE))

ingreso_nuble2009. = unlist(lapply(ingreso_nuble2009, '[[',2,2))
ee.ingreso_nuble2009 = unlist(lapply(ingreso_nuble2009,'[[',2,3))

ingreso_nuble2009__ = data.frame(ingreso = ingreso_nuble2009., se = ee.ingreso_nuble2009) %>% 
  mutate(cv = se/ingreso, ingreso=round(ingreso,0), codigo = ocupaciones) 

temp = xtabs(~c_o12, subset(diseño2009$variables, provincia==84)) %>%
  data.frame() %>%   filter(c_o12 == 1314 |                                
                              c_o12 == 2331 | c_o12 == 4115 |                                                                  
                              c_o12 == 5122 | c_o12 == 5131 |                            
                              c_o12 == 5220 | c_o12 == 6111 |
                              c_o12 == 6112 | c_o12 == 6210 |                                                           
                              c_o12 == 7124 | c_o12 == 8322 |
                              c_o12 == 8324 | c_o12 == 9131 | 
                              c_o12 == 9132 | c_o12 == 9211)  

ingreso_nuble2009__$frecuencia = temp$Freq

ingreso_nuble2009 = merge(ingreso_nuble2009__, ciuo, by.x ="codigo", by.y = "codigo") %>% 
  mutate(nota = ifelse(cv>=0.25 | frecuencia<50,"a",""), year = 2009)

write.csv(ingreso_nuble2009, "ingreso2009.csv")
########################################################################
# Promedio de años de escolaridad
########################################################################

escolaridad_nuble2009 = lapply(ocupaciones,
                               function(x) svyby(~esc, subset(diseño2009, provincia==84),
                                                 by=~I(c_o12==x), svymean, na.rm=TRUE,
                                                 drop.empty.groups = FALSE))

escolaridad_nuble2009. = unlist(lapply(escolaridad_nuble2009, '[[',2,2))
ee.escolaridad_nuble2009 = unlist(lapply(escolaridad_nuble2009,'[[',2,3))

escolaridad_nuble2009__ = data.frame(esc = escolaridad_nuble2009., se = ee.escolaridad_nuble2009) %>% 
  mutate(cv = se/esc, esc=round(esc,1), codigo = ocupaciones) 

temp = xtabs(~c_o12, subset(diseño2009$variables, provincia==84)) %>%
  data.frame() %>%   filter(c_o12 == 1314 |                                
                              c_o12 == 2331 | c_o12 == 4115 |                                                                  
                              c_o12 == 5122 | c_o12 == 5131 |                            
                              c_o12 == 5220 | c_o12 == 6111 |
                              c_o12 == 6112 | c_o12 == 6210 |                                                           
                              c_o12 == 7124 | c_o12 == 8322 |
                              c_o12 == 8324 | c_o12 == 9131 | 
                              c_o12 == 9132 | c_o12 == 9211)  

escolaridad_nuble2009__$frecuencia = temp$Freq

escolaridad_nuble2009 = merge(escolaridad_nuble2009__, ciuo, by.x ="codigo", by.y = "codigo") %>% 
  mutate(nota = ifelse(cv>=0.25 | frecuencia<50,"a",""), year = 2009)

write.csv(escolaridad_nuble2009, "escolaridad2009.csv")

# Para remover los objetos que no son de interés 
rm(list=ls(pattern="__$"))

