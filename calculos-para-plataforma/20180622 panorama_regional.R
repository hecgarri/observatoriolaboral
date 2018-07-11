# 20180622 panorama_regional.R
##
## Héctor Garrido Henríquez
## Analista Cuantitativo. Observatorio Laboral Ñuble
## Docente part-time. Facultad de Ciencias Empresariales
## Universidad del Bío-Bío
## Avenida Andrés Bello 720, Casilla 447, Chillán
## Teléfono: +56-942353973
## http://www.observatoriolaboralnuble.cl
#
rm(list=ls())
gc()
################################################
# Carga de librerías necesarias para el análisis
################################################
if (!require(foreign)) install.packages("foreign"); require(foreign)
if (!require(parallel)) install.packages("parallel"); require(parallel)
if (!require(data.table)) install.packages("data.table"); require(data.table)
if (!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE); require(tidyverse)
if (!require(RSQLite)) install.packages("sqldf"); require(RSQLite)
if (!require(stringi))     install.packages("stringi"); require(stringi)
if (!require(lubridate)) install.packages("lubridate"); require(lubridate)
if (!require(srvyr)) install.packages("srvyr"); require(srvyr)
if (!require(labelled)) install.packages("labelled"); require(labelled)

##################################################################
# Importación de la base de datos CASEN2015
##################################################################


casen2015 = read_sav("casen2015.sav")
# Para mirar las etiquetas
directorio = read_csv("directorio_casen2015.csv")
attr(casen2015$o15,"label")
attr(casen2015$sexo,"labels")

#Ingresos de los ocupados de la Región de Ñuble según sector productivo y categoría ocupacional, año 2015.

attr(casen2015$rama1,"labels")
attr(casen2015$o15,"labels")

# (1:Empleador; 2:Cuenta Propia; 3:Trabajador dependiente)
casen2015 = casen2015 %>%
  mutate(cat_ocup = recode(o15, `1`= 1, 
                     `2`= 2, `3` =3, `4`=3, `5`=3, .default = 0, .missing = 99), 
        cat_ocup = ifelse(cat_ocup == 3 & o17 !=1,5, 
                   ifelse(cat_ocup == 3 & o17 == 1 & o16 == 2,6, 
                   ifelse(cat_ocup == 3 & o17 == 1 & o16 == 1,7,
                          cat_ocup))), 
        cat_ocup = labelled(cat_ocup,c(`Empleador` = 1,
                                       `Cuenta Propia` = 2,
                                       `Servicio Doméstico` = 4,
                                       `Dependiente sin contrato` = 5,
                                       `Dependiente contrato definido` = 6,
                                       `Dependiente contrato indefinido` = 7,
                                       `No aplica` = 99, 
                                       `Familiar no remunerado y Fuerzas Armadas y Servicio Doméstico` = 0)))

### Homologación de sectores para CIIU Rev.3 (Véase documento: 20170508 Manuel reporte regional 
# datos cuantitativo, página 16)

#"Los sectores de Enseñanza, Servicios Sociales, Otras actividades de Servicios
#Comunitarios, Sociales y Personales y Hogares Privados con Servicio
#Doméstico, clasificados en ENE, se suman y se asignan al sector “Servicios
#Sociales y Personales”.

casen2015 = casen2015 %>% mutate(sector = recode(rama1,
                                                 `13` = 12,
                                                 `14` = 12,
                                                 `15` = 12,
                                                 `16` = 12,
                                                 `12` = 13,
                                                 `17` = 13), 
                                 sector = labelled(sector,
                                                   c(`Silvoagropecuario` = 1,
                                                     `Pesca` = 2,
                                                     `Minería` = 3,
                                                     `Industria Manufacturera` = 4, 
                                                     `Electricidad,Gas y Agua` = 5,
                                                     `Construcción` = 6,
                                                     `Comercio` = 7,
                                                     `Hoteles y Restoranes` = 8, 
                                                     `Transporte y Comunicaciones` = 9,
                                                     `Intermediación Financiera` = 10, 
                                                     `Actividades Inmobiliarias,Empresariales y de Alquiler` = 11, 
                                                     `Administración Pública` = 13, 
                                                     `Servicios Sociales y Personales` = 12)))
attr(casen2015$sector,"labels")


casen2015 = casen2015 %>% mutate(educacion = recode(educ,`0`=1,
                                                    `1` = 1,
                                                    `2` = 2,
                                                    `3` = 2, 
                                                    `4` = 2, 
                                                    `5` = 3, 
                                                    `6` = 3, 
                                                    `7` = 3,
                                                    `8` = 4,
                                                    `9` = 3, 
                                                    `10` = 4, 
                                                    `11` = 4, 
                                                    `12` = 4,.default = 0), 
                                 educacion = labelled(educacion,
                                                      c(`Básica incompleta o menor` = 1,
                                                        `Básica completa` = 2,
                                                        `Media completa` = 3,
                                                        `Superior completa` = 4,
                                                        `Sin dato` = 0)))

casen2015 = casen2015 %>% mutate(jóvenes = ifelse(edad>=15 & edad<=29,1,
                                           ifelse(edad>29,2,0)), 
                                 jóvenes = labelled(jóvenes,
                                                    c(`Jóvenes` = 1, 
                                                      `Adultos` = 2,
                                                      `Menores` = 0)))
casen2015 = casen2015 %>% mutate(activo = ifelse(activ==1 | activ==2,1,0), 
                                 inactivo = ifelse(activ==3,1,0), 
                                 desocupado = ifelse(activ==2,1,0),
                                 ocupado = ifelse(activ==1,1,0),
                                 edad_activ = ifelse(edad>=15,1,0))

casen2015 = casen2015 %>% mutate(indigena = recode(r3, 
                                                   `10`= 1,
                                                   `99`=3, 
                                                   .default = 2), 
                                 indigena = labelled(indigena, 
                                                     c(`No indigena` = 1,
                                                       `Indígena` = 2, 
                                                       `Sin dato` = 3)))
#Ingresos de los ocupados de la Región de Ñuble según sector productivo y categoría ocupacional, año 2015.														DETALLE

design = casen2015 %>% as_survey_design(id = varunit, strata = varstrat, weights = expr)
options(scipen = 10)

ingresos_categoria_sector_nuble = design %>% 
  filter(provincia == 84, !is.na(yoprCor)) %>% 
  group_by(cat_ocup, sector) %>% 
  summarise(estimate = survey_mean(yoprCor, vartype ="cv", na.rm = TRUE), 
            frecuencia = unweighted(n())) %>%
  filter(cat_ocup!=4, cat_ocup!=99, cat_ocup!=0) %>% 
  mutate(quality = ifelse(estimate_cv<=0.25 & frecuencia>=50,1,0), 
         estimate = ifelse(quality==1, round(estimate,0),"S.I"))
tabla = ingresos_categoria_sector_nuble[,3] 
path = file.path("/home/hector/GoogleDriveUBB/",
                 "OLR Ñuble - Observatorio laboral de Ñuble/",
                 "Análisis Cuantitativo/GitHub/",
                 "calculos-para-plataforma/")

ingresos_categoria_sector_nuble = expand.grid(cat_ocup = unique(ingresos_categoria_sector_nuble$cat_ocup), 
            sector = unique(ingresos_categoria_sector_nuble$sector)) %>% 
  left_join(ingresos_categoria_sector_nuble, by = c("cat_ocup","sector")) %>% 
  arrange(cat_ocup,sector) %>% filter(sector!=99)

write_csv(ingresos_categoria_sector_nuble,paste0(path,"Ingresos por sector y categoría ocupacional Ñuble.csv"))


ingresos = design %>% 
  filter(provincia == 84, !is.na(yoprCor)) %>% 
  group_by(cat_ocup) %>% 
  summarise(estimate = survey_mean(yoprCor, vartype ="cv", na.rm = TRUE), 
            frecuencia = unweighted(n())) %>%
  filter(cat_ocup!=99, cat_ocup!=0) %>% 
  mutate(quality = ifelse(estimate_cv<=0.25 & frecuencia>=50,1,0), 
         estimate = ifelse(quality==1, round(estimate,0),"S.I"))

write_csv(ingresos,paste0(path,"Ingresos por categoría ocupacional Ñuble.csv"))

ingresos = design %>% 
  filter(!is.na(yoprCor)) %>% 
  group_by(cat_ocup) %>% 
  summarise(estimate = survey_mean(yoprCor, vartype ="cv", na.rm = TRUE), 
            frecuencia = unweighted(n())) %>%
  filter(cat_ocup!=99, cat_ocup!=0) %>% 
  mutate(quality = ifelse(estimate_cv<=0.25 & frecuencia>=50,1,0), 
         estimate = ifelse(quality==1, round(estimate,0),"S.I"))
write_csv(ingresos,paste0(path,"Ingresos por categoría ocupacional todo el país.csv"))

ingreso_nuble = design %>% filter(provincia == 84, !is.na(yoprCor)) %>% 
  summarise(estimate = survey_mean(yoprCor,vartype = "cv", 
                                   frecuencia = unweighted(n())))

ingreso_país = design %>% filter(!is.na(yoprCor)) %>% 
  summarise(estimate = survey_mean(yoprCor,vartype = "cv", 
                                   frecuencia = unweighted(n())))

#### 
# Edad promedio 
####

edad_promedio = design %>% filter(provincia==84 & activ==1) %>% 
  summarise(estimate = survey_mean(edad), 
            frecuencia = unweighted(n()))

escolaridad_promedio = design %>% filter(provincia==84 & activ==1) %>% 
  summarise(estimate = survey_mean(esc,na.rm =TRUE), 
            frecuencia = unweighted(n()))

sexo = design %>% filter(provincia == 84 & activ==1) %>%
  group_by(sexo) %>% 
  summarise(estimate = survey_mean(), 
            frecuencia = unweighted(n()))

superior = design %>% filter(provincia == 84 & activ==1) %>%
  group_by(educacion) %>% 
  summarise(estimate = survey_mean(), 
            frecuencia = unweighted(n()))

############################################################
# Tasas 
############################################################

# Tasa de participación 

options(scipen = 10)
participacion = design %>% filter(provincia==84, !is.na(activo)) %>% group_by(jóvenes) %>% 
  summarise(estimate = survey_ratio(numerator = activo,
                                    denominator = edad_activ, na.rm = TRUE))
participacion = design %>% filter(!is.na(activo)) %>% group_by(jóvenes) %>% 
  summarise(estimate = survey_ratio(numerator = activo,
                                    denominator = edad_activ, na.rm = TRUE))


desocupacion = design %>% filter(provincia==84, !is.na(activo)) %>% group_by(jóvenes) %>% 
  summarise(estimate = survey_ratio(numerator = desocupado,
                                    denominator = activo, na.rm = TRUE))

desocupacion = design %>% filter(!is.na(activo)) %>% group_by(jóvenes) %>% 
  summarise(estimate = survey_ratio(numerator = desocupado,
                                    denominator = activo, na.rm = TRUE))

ocupacion = design %>% filter(provincia==84, !is.na(activo)) %>% group_by(jóvenes) %>% 
  summarise(estimate = survey_ratio(numerator = ocupado,
                                    denominator = edad_activ, na.rm = TRUE))

ocupacion = design %>% filter(!is.na(activo)) %>% group_by(jóvenes) %>% 
  summarise(estimate = survey_ratio(numerator = ocupado,
                                    denominator = edad_activ, na.rm = TRUE))

######################
# Pueblos originarios
######################

participacion = design %>% filter(provincia==84) %>% group_by(indigena) %>% 
  summarise(estimate = survey_ratio(numerator = activo,denominator = edad_activ,na.rm=TRUE, 
                                    vartype = "cv"))

frecuencia = design %>% filter(provincia==84 & activo==1) %>% group_by(indigena) %>% 
  summarise(estimate = unweighted(n()))


ocupacion = design %>% filter(provincia==84) %>% group_by(indigena) %>% 
  summarise(estimate = survey_ratio(numerator = ocupado,denominator = edad_activ,na.rm=TRUE, 
                                    vartype = "cv"))

frecuencia = design %>% filter(provincia==84 & ocupado==1) %>% group_by(indigena) %>% 
  summarise(estimate = unweighted(n()))

desocupacion = design %>% filter(provincia==84) %>% group_by(indigena) %>% 
  summarise(estimate = survey_ratio(numerator = desocupado,denominator = activo,na.rm=TRUE, 
                                    vartype = "cv"))

frecuencia = design %>% filter(provincia==84 & desocupado==1) %>% group_by(indigena) %>% 
  summarise(estimate = unweighted(n()))