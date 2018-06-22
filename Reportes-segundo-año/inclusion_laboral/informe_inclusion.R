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

## Cuadro 4. Brecha de género en ingresos por hora para trabajadores dependientes, 
## ingresos de los dependientes según sexo y nivel educacional de Ñuble, 2016.

ingresos_hora = diseno %>% filter(dependientes == 11, !is.na(yoprCor), 
                                  !is.na(nivel_educ)) %>% group_by(sexo, nivel_educ) %>% 
  summarise(estimate = survey_ratio(yoprCor, horas_trabajo*4, na.rm = TRUE, vartype = "cv"), 
            frecuencia = unweighted(n()))
ingresos_hora2 = diseno %>% filter(dependientes == 11, !is.na(yoprCor)) %>% group_by(sexo) %>% 
  summarise(estimate = survey_ratio(yoprCor, horas_trabajo*4, na.rm = TRUE, vartype = "cv"), 
            frecuencia = unweighted(n()))

cuadro_4a = cbind(round(ingresos_hora[ingresos_hora$sexo==1,3],0),
                 round(ingresos_hora[ingresos_hora$sexo==2,3],0)) 
cuadro_4b =  round(cbind(ingresos_hora2[1,2],ingresos_hora2[2,2]),0)

cuadro_4 = rbind(cuadro_4a, cuadro_4b)
rownames(cuadro_4) = c("Básica completa o menor", "Media completa", 
                       "Técnico Nivel Superior o Profesional", "Total")
colnames(cuadro_4) = c("Hombres", "Mujeres")
cuadro_4 = cuadro_4 %>% mutate(brecha = (((Mujeres-Hombres)/Hombres)*100) %>% round(1))


## Cuadro 4 (opcional) ingresos según sexo y nivel educacional de Ñuble, 2016.

ingresos_hora_ = diseno %>% filter(!is.na(yoprCor), 
                                  !is.na(nivel_educ)) %>% group_by(sexo, nivel_educ) %>% 
  summarise(estimate = survey_ratio(yoprCor, horas_trabajo*4, na.rm = TRUE, vartype = "cv"), 
            frecuencia = unweighted(n()))
ingresos_hora2_ = diseno %>% filter(!is.na(yoprCor)) %>% group_by(sexo) %>% 
  summarise(estimate = survey_ratio(yoprCor, horas_trabajo*4, na.rm = TRUE, vartype = "cv"), 
            frecuencia = unweighted(n()))

cuadro_4a_ = cbind(round(ingresos_hora_[ingresos_hora_$sexo==1,3],0),
                  round(ingresos_hora_[ingresos_hora_$sexo==2,3],0)) 
cuadro_4b_ =  round(cbind(ingresos_hora2_[1,2],ingresos_hora2_[2,2]),0)

cuadro_4_ = rbind(cuadro_4a_, cuadro_4b_)
colnames(cuadro_4_) = c("Hombres", "Mujeres")
cuadro_4_ = cuadro_4_ %>% mutate(brecha = (((Mujeres-Hombres)/Hombres)*100) %>% round(1))
rownames(cuadro_4_) = c("Básica completa o menor", "Media completa", 
                        "Técnico Nivel Superior o Profesional", "Total")

path = file.path("/home/hector/GoogleDriveUBB",
"/OLR Ñuble - Observatorio laboral de Ñuble/",
"Análisis Cuantitativo/observatoriolaboral/Reportes-segundo-año/inclusion_laboral")
write.csv(cuadro_4_, paste0(path,"/cuadro_4_alt.csv"))

## Cuadro 4 (opcional) ingresos según sexo y nivel educacional de Ñuble, 2016. Mediana

ingresos_hora_quantiles = diseno %>% filter(!is.na(yoprCor), 
                                   !is.na(nivel_educ)) %>% group_by(sexo, nivel_educ) %>% 
  summarise(estimate = survey_quantile(yoprCor, quantiles = c(0.25,0.5,0.75), na.rm = TRUE), 
            frecuencia = unweighted(n())) %>% select(sexo, nivel_educ, 
                                                     estimate_q25, estimate_q50, 
                                                     estimate_q75)

options(scipen = 3)
write.csv(ingresos_hora_quantiles, paste0(path,"/cuadro_4_alt2.csv"))


### Ingresos de las jefas de hogar
### 
ingresos_jefas_ =  diseno %>% filter(!is.na(yoprCor), 
                                          pco1==1, !is.na(nivel_educ)) %>%
  group_by(sexo, nivel_educ) %>% 
  summarise(estimate = survey_mean(yoprCor, na.rm = TRUE, vartype = "cv"), 
            frecuencia = unweighted(n()))

ingresos_jefas_ =  diseno %>% filter(!is.na(yoprCor), 
                                          pco1==1, !is.na(nivel_educ)) %>%
  group_by(sexo, nivel_educ) %>% 
  summarise(estimate = survey_mean(yoprCor, na.rm = TRUE, vartype = "cv"), 
            frecuencia = unweighted(n()))

ingresos_jefas_2 =  diseno %>% filter(!is.na(yoprCor), 
                                          pco1==1) %>%
  group_by(sexo) %>% 
  summarise(estimate = survey_mean(yoprCor, na.rm = TRUE, vartype = "cv"), 
            frecuencia = unweighted(n()))

ingresos_jefas_2 = cbind(nivel_educ = c("Total", "Total"), ingresos_jefas_2)

ingresos_jefas_ = rbind(ingresos_jefas_, ingresos_jefas_2)

ingresos_jefas_ = cbind(ingresos_jefas_[c(1,2,3,7),c(2,3)], ingresos_jefas_[c(4,5,6,8),3]) 

colnames(ingresos_jefas_) = c("Nivel educacional", "Hombres", "Mujeres")

ingresos_jefas_ = ingresos_jefas_  %>% mutate(brecha = (((Mujeres-Hombres)/Hombres)*100) %>% round(2))

ingresos_jefas_[,c(2,3)] = round(ingresos_jefas_[,c(2,3)],0)

write.csv(ingresos_jefas_, paste0(path,"/cuadro_4_alt3.csv"))

#################################################################
ingresos_jefas_zona =  diseno %>% filter(!is.na(yoprCor), 
                                     pco1==1, !is.na(zona)) %>%
  group_by(sexo, zona) %>% 
  summarise(estimate = survey_mean(yoprCor, na.rm = TRUE, vartype = "cv"), 
            frecuencia = unweighted(n()))
ingresos_jefas_zona = ingresos_jefas_zona %>% select(sexo, zona, estimate) %>% 
  rename(salario = estimate)

ingresos_jefas_zona = cbind(ingresos_jefas_zona[c(1,2),c(2,3)], ingresos_jefas_zona[c(3,4),c(3)])
colnames(ingresos_jefas_zona) = c("Zona","Hombre", "Mujer")
ingresos_jefas_zona = ingresos_jefas_zona %>% mutate(brecha = (((Mujer-Hombre)/Hombre)*100) %>% round(1))

ingresos_jefas_zona[,c(2,3)] = round(ingresos_jefas_zona[,c(2,3)],0) 

write.csv(ingresos_jefas_zona, paste0(path,"/cuadro_4_alt4.csv"))


### 
### Ingresos de los hogares según jefatura

ingresos_jefas_hogar =  diseno %>% filter(!is.na(yoprCorh), 
                                          pco1==1, !is.na(nivel_educ)) %>%
  group_by(sexo, nivel_educ) %>% 
  summarise(estimate = survey_mean(yoprCorh, na.rm = TRUE, vartype = "cv"), 
            frecuencia = unweighted(n()))

ingresos_jefas_hogar =  diseno %>% filter(!is.na(yoprCor), 
                                          pco1==1, !is.na(nivel_educ)) %>%
  group_by(sexo, nivel_educ) %>% 
  summarise(estimate = survey_mean(yoprCorh, na.rm = TRUE, vartype = "cv"), 
            frecuencia = unweighted(n()))

ingresos_jefas_hogar2 =  diseno %>% filter(!is.na(yoprCor), 
                                           pco1==1) %>%
  group_by(sexo) %>% 
  summarise(estimate = survey_mean(yoprCor, na.rm = TRUE, vartype = "cv"), 
            frecuencia = unweighted(n()))

###
### Número de personas según jefatura de hogar

numero_ = diseno %>% filter(pco1==1) %>% group_by(sexo) %>% 
  summarise(estimate = survey_mean(numper), frecuencia = unweighted(n()))

## Proporción de hogares según jefatura

total_hogares = diseno %>% filter(pco1==1) %>% group_by(sexo) %>% 
  summarise(estimate = survey_mean())

path_seguro = file.path("/home/hector/GoogleDriveUBB/",
                        "OLR Ñuble - Observatorio laboral de Ñuble/",
"Análisis Cuantitativo/Boletines/datos_hist.csv")

datos_seguro = read.csv(path_seguro) %>% tbl_df() %>%
  select(date,contains("real")) %>% mutate(año = year(date), 
                                           mes = month(date))


sal_real_hom = ts(datos_seguro$sal_real_hom, start = c(2002,1), 
                  frequency = 12) 
sal_real_hom = aggregate(sal_real_hom, nfrequency = 4)/3

sal_real_muj = ts(datos_seguro$sal_real_muj, start = c(2002,1), 
                  frequency = 12) 
sal_real_muj = aggregate(sal_real_muj, nfrequency = 4)/3

brecha = (sal_real_muj-sal_real_hom)/(sal_real_hom)

ts.plot(brecha)


