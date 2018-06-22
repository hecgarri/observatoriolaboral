## ocupaciones.R
##
## Héctor Garrido Henríquez
## Analista Cuantitativo. Observatorio Laboral Ñuble
## Docente part-time. Facultad de Ciencias Empresariales
## Universidad del Bío-Bío
## Avenida Andrés Bello 720, Casilla 447, Chillán
## Teléfono: +56-942353973
## http://www.observatoriolaboralnuble.cl
#
#Indicadores disponibles en esta rutina de cálculo 
#
## Primera pestaña: Información Mercado Laboral 
#
# Ingreso por hora
# Ingreso mensual
# Años de escolaridad
# Años en mismo empleo
# Mujeres (%)
# Capacitación (%)
# Jornada Completa (%)
# Contrato a Plazo Indefinido (%)
# Con educación superior completa (%)
# Ingreso mensual ocupados sin educación superior completa
# Ingreso mensual ocupados con educación superior completa
#
## Segunda Pestaña: Ingresos 
# 
# Mediana de ingresos
# Mediana de ingresos por hora
# Mediana ingreso según jornada laboral: Completa y parcial 
# Mediana ingreso según genero: Hombre y Mujer 
# Mediana ingreso según nivel educacional: Media incompleta o menos, media completa, superior
# Mediana ingreso según categoria ocupacional: Empleados, Cuenta propia, Asalariado con contrato
# 
## Quinta Pestaña: Tendencia 
# 
# Número de ocupados y concentración de ocupación, 1998-2015
# Ingreso promedio, 1998-2015
# Años de escolaridad promedio, 1998-2015

rm(list=ls())

if (!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
if (!require(foreign)) install.packages("foreign"); require(foreign)
if (!require(srvyr)) install.packages("srvyr"); require(srvyr)

path = file.path("/home/hector/GoogleDriveUBB/",
"OLR Ñuble - Observatorio laboral de Ñuble","Bases de datos/",
"Encuesta de Caracterización Socioeconómica Nacional (CASEN)/Casen 2015 SPSS.sav")

casen2015 = read.spss(path,use.value.labels = FALSE,
                      to.data.frame= FALSE, use.missings = TRUE)

etiquetas.casen = attr(casen2015, "label.table")
directorio.casen = attr(casen2015,"variable.labels")
directorio.casen = data.frame(names(directorio.casen),directorio.casen)


## Lista de ocupaciones OLR Ñuble
# Ocupaciones página Web
codigo_ciuo = c(1314,2331,4115,5122,5131,5220,6111,6112,6210,7124,8322,8324,9131,9132,9211)

path_ciuo = file.path("/home/hector/GoogleDriveUBB/",
                      "OLR Ñuble - Observatorio laboral de Ñuble/",
                      "Bases de datos/codigos_ciuo.csv")

casen2015_ocup = tbl_df(casen2015) %>% filter(provincia == 84, 
                                         oficio4 %in% codigo_ciuo) %>% 
  mutate(anos_empleo = 2016-o13, anos_empleo = ifelse(anos_empleo==-7982,NA, 
                                                      anos_empleo),
         sexo = as.factor(sexo), o30 = as.factor(o30), 
         o18 = as.factor(o18), o16 = as.factor(o16))

casen2015 = tbl_df(casen2015) %>% filter(provincia == 84)

options(survey.lonely.psu = "certainty") 
diseno =  casen2015_ocup %>% as_survey_design(id = varunit, strata = varstrat,
                                     weights  = expr,nest = TRUE)

options(survey.lonely.psu = "certainty") 
diseno =  casen2015 %>% as_survey_design(id = varunit, strata = varstrat,
                                              weights  = expr,nest = TRUE)

ciuo = read.csv(path_ciuo) %>% filter(codigo %in% codigo_ciuo) %>% distinct(codigo, etiqueta)

# Ingreso por hora promedio 
ingreso_hora = diseno %>% group_by(oficio4) %>% 
  summarise(estimate =  survey_ratio(yoprCor,o10*4, na.rm = TRUE, vartype = "cv"),
            frecuencia = unweighted(n())) %>%
  mutate(quality = ifelse(estimate_cv<=0.25 & frecuencia>=50,"Ok",NA), 
         indicador = "Ingreso por hora") %>% 
  select(indicador, everything())
# Ingreso mensual 
ingreso_mensual = diseno %>% group_by(oficio4) %>% 
  summarise(estimate =  survey_mean(yoprCor, na.rm = TRUE, vartype = "cv"),
            frecuencia = unweighted(n())) %>%
  mutate(quality = ifelse(estimate_cv<=0.25 & frecuencia>=50,"Ok",NA), 
         indicador = "Ingreso mensual") %>% 
  select(indicador, everything())
# Años de escolaridad
años_escolaridad = diseno %>% group_by(oficio4) %>% 
  summarise(estimate =  survey_mean(esc, na.rm = TRUE, vartype = "cv"),
            frecuencia = unweighted(n())) %>%
  mutate(quality = ifelse(estimate_cv<=0.25 & frecuencia>=50,"Ok",NA), 
         indicador = "Años de escolaridad") %>% 
  select(indicador, everything())
# Años en el mismo empleo 
años_empleo = diseno %>% group_by(oficio4) %>% 
  summarise(estimate =  survey_mean(anos_empleo, na.rm = TRUE, vartype = "cv"),
            frecuencia = unweighted(n())) %>%
  mutate(quality = ifelse(estimate_cv<=0.25 & frecuencia>=50,"Ok",NA), 
         indicador = "Años en el mismo empleo") %>% 
  select(indicador, everything())
# Mujeres (%)
mujeres = diseno %>% group_by(oficio4, sexo) %>% 
  summarise(estimate =  survey_mean(vartype = "cv")) %>% filter(sexo==2) 

mujeres_freq = diseno %>% group_by(oficio4, sexo) %>% 
  summarise(frecuencia = unweighted(n()))

mujeres = left_join(mujeres, mujeres_freq) %>% 
  mutate(quality = ifelse(frecuencia>=50,"Ok",NA), 
         indicador = "Porcentaje de mujeres") %>% 
  select(indicador,everything(),-sexo)

# Capacitación (%)

capacita = diseno %>% group_by(oficio4,o30) %>% 
  summarise(estimate =  survey_mean(na.rm = TRUE, vartype = "cv")) %>% filter(o30==1)

capacita_freq = diseno %>% group_by(oficio4,o30) %>% 
  summarise(frecuencia = unweighted(n())) %>% filter(o30==1)


capacita = left_join(capacita, capacita_freq) %>%
  mutate(quality = ifelse(frecuencia>=50,"Ok",NA),
         indicador = "Porcentaje de capacitación") %>% 
  select(indicador,everything(),-o30)

# Jornada completa (%)


aux_diseno = casen2015[-which(is.na(casen2015$o18)),] %>% as_survey_design(id = varunit, strata = varstrat,
                                                      weights  = expr,nest = TRUE) 

jornada_completa = aux_diseno %>% group_by(oficio4,o18) %>% 
  summarise(estimate =  survey_mean(na.rm = TRUE, vartype = "cv")) %>% filter(o18==1)

jornada_completa_freq = aux_diseno %>% group_by(oficio4,o18) %>% 
  summarise(frecuencia = unweighted(n())) %>% filter(o18==1)


jornada_completa = left_join(jornada_completa, jornada_completa_freq) %>%
  mutate(quality = ifelse(frecuencia>=50,"Ok",NA),
         indicador = "Porcentaje de jornada_completa") %>% 
  select(indicador,everything(),-o18)

## Plazo indefinido (%)

aux_diseno = casen2015[-which(is.na(casen2015$o16)),] %>%
  as_survey_design(id = varunit, strata = varstrat,
                   weights  = expr,nest = TRUE) 

plazo_indefinido = aux_diseno %>% group_by(oficio4,o16) %>% 
  summarise(estimate =  survey_mean(na.rm = TRUE, vartype = "cv")) %>% filter(o16==1)

plazo_indefinido_freq = aux_diseno %>% group_by(oficio4,o16) %>% 
  summarise(frecuencia = unweighted(n())) %>% filter(o16==1)

plazo_indefinido = left_join(plazo_indefinido, plazo_indefinido_freq) %>%
  mutate(quality = ifelse(frecuencia>=50,"Ok",NA),
         indicador = "Porcentaje de plazo indefinido") %>% 
  select(indicador,everything(),-o16)

## Educación superior



# Ingreso por hora = ingreso_hora 
# Ingreso mensual = ingreso_mensual
# Años de escolaridad = años_escolaridad
# Años en mismo empleo = años_empleo 
# Mujeres (%) = mujeres
# Capacitación (%) = capacita 
# Jornada Completa (%)
# Contrato a Plazo Indefinido (%)
# Con educación superior completa (%)
# Ingreso mensual ocupados sin educación superior completa
# Ingreso mensual ocupados con educación superior completa

datos_ocupaciones = rbind(ingreso_hora, ingreso_mensual, 
                          años_escolaridad, años_empleo, 
                          mujeres, capacita, jornada_completa) %>%
  rename(codigo = oficio4)
ciuo = ciuo %>% mutate(codigo = as.character(codigo))
datos_ocupaciones = left_join(datos_ocupaciones, ciuo, by = "codigo")


# Ingresos de los ocupados de la Región de Ñuble según sector productivo y categoría ocupacional, año 2015.


## Cálculos sobre brechas de género para reportaje

## Brecha de ingresos promedio. 
ingresos_sexo = diseno %>% group_by(sexo) %>%
  summarise(estimate = survey_mean(yoprCor, na.rm = TRUE, vartype = "cv"))

frecuencia = diseno %>% filter(!is.na(yoprCor)) %>% group_by(sexo) %>% summarise(n = unweighted(n()))

ingresos_sexo = left_join(ingresos_sexo, frecuencia, by = "sexo")

brecha = (((ingresos_sexo[2,2]-ingresos_sexo[1,2])/(ingresos_sexo[2,2]))*100) %>% round(1)

## Brecha de ingresos por hora. 

ing_sex_hora = diseno %>% mutate(o10 = ifelse(o10==999,NA,o10)) %>% 
  group_by(sexo) %>% summarise(estimate = survey_ratio(numerator = yoprCor, denominator = o10*4, na.rm = TRUE))

brecha = (((ing_sex_hora[2,2]-ing_sex_hora[1,2])/(ing_sex_hora[2,2]))*100) %>% round(1) 