rm(list=ls())

if (!require(devtools)) install.packages("devtools"); require(devtools)
if (!require(data.table)) install.packages("data.table"); require(data.table) # version 1.10.4-3
if (!require(dplyr))       install.packages("dplyr"); require(dplyr)
if (!require(stringi))     install.packages("stringi"); require(stringi)
if (!require(survey))      install.packages("survey"); require(survey)
if (!require(purrr))      install.packages("purr"); require(purrr)

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# SE FIJA EL DIRECTORIO DE TRABAJO:
#setwd("C:\\Users\\Miguelo\\Documents\\ENE\\")
setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Nueva Encuesta Nacional de Empleo (ENE)")
#-------------------------------------------------------------------------------
#

load("etiquetas.RData")

nene = list.files(pattern = ".csv$")

todas =  lapply(nene, function(x) fread(x, sep=",", header=TRUE,
                      select=c("id_directorio", "estrato", "nacionalidad", 
                      "fact", "cae_general","cae_especifico", 
                      "mes_central", "b8", "b9", "ano_trimestre", 
                      "termino_nivel","b18_codigo", "region", "categoria_ocupacion",
                      "edad", "r_p_c", "c1", "c10", "c11", "habituales", 
                      "nivel", "curso","sexo", "b15_1", "b15_2", "b14", "e18", 
                      "e18_rev4cl_caenes","b13_rev4cl_caenes", 
                      "b14_rev4cl_caenes"),fill=TRUE, blank.lines.skip = TRUE, 
                      showProgress = FALSE))



#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLES region_e y prov_e
#-------------------------------------------------------------------------------
# Creamos una variable auxiliar que cuenta el numero de caracteres

todas = lapply(todas, function(x) mutate(x,
                     aux = stri_length(b18_codigo),
                     region_e = b18_codigo,
                     region_e = ifelse(aux==5, stri_sub(region_e,1,2),
                     ifelse(aux==4,stri_sub(region_e,1,1),region_e)),
                     region_e = strtoi(region_e)))

todas = lapply(todas, function(x) mutate(x,
                      aux = stri_length(b18_codigo),
                      prov_e = b18_codigo,
                      prov_e = ifelse(aux==5, stri_sub(prov_e,1,3),
                      ifelse(aux==4,stri_sub(prov_e,1,2),prov_e)),
                      prov_e = strtoi(prov_e)))

# cae general y cae específico 
todas = lapply(todas, function(x) mutate(x,cae_general = recode(cae_general,
                                  `0`=0,`1` = 1,`2` = 1,
                                  `3` = 1, `4` = 2,`5` = 3,`6` = 4,
                                  `7` = 5,`8` = 5,`9` = 6),
                                  cae_general = factor(cae_general,levels = c(0,1,2,3,4,5,6), 
                                  labels = c("menor de 15","Ocupado", "Desocupado", 
                                  "Busca Trabajo Primera Vez","Iniciadores",
                                  "Inactivos Potencialmente activos","Inactivos Habituales")),
                                  cae_especifico=as.numeric(cae_especifico)))


# Sector Económico

# aquí sólo utilizaré las encuestas cuya clasificación de los sectores corresponde
# la CIIU rev 3

todas[1:82] = lapply(todas[1:82], function(x) mutate(x,sector = b14,
                                         sector = ifelse(is.na(sector),e18,sector)))



todas[83:length(nene)] = lapply(todas[83:length(nene)], function(x) mutate(x,
                                         sector =b13_rev4cl_caenes))


todas[1:82] = lapply(todas[1:82], function(x) mutate(x, sector = factor(sector,
                    levels=c(1,2,3,4,5,6,7,8,9,10,
                             11,12,13,14,15,16,17),
                    labels=c("Agricultura",
                             "Pesca",
                             "Explotacion",
                             "Industrias",
                             "Electricidad",
                             "Construccion",
                             "Comercio", 
                             "Hoteles",
                             "Transporte",
                             "Financiero",
                             "Inmobiliarias",
                             "Administracion Publica",
                             "Ensenanza",
                             "Servicios sociales",
                             "Otras actividades",
                             "Hogares privados", 
                             "Organizaciones extraterritoriales"))))

todas[1:82] = lapply(todas[1:82], function(x) mutate(x, sector = factor(sector,
                    levels=c(1,2,3,4,5,6,7,8,9,10,
                             11,12,13,14,15,16,17),
                    labels=c("Agricultura",
                             "Pesca",
                             "Explotacion",
                             "Industrias",
                             "Electricidad",
                             "Construccion",
                             "Comercio", 
                             "Hoteles",
                             "Transporte",
                             "Financiero",
                             "Inmobiliarias",
                             "Administracion Publica",
                             "Ensenanza",
                             "Servicios sociales",
                             "Otras actividades",
                             "Hogares privados", 
                             "Organizaciones extraterritoriales"))))

todas[83:length(todas)] = lapply(todas[83:length(todas)], function(x) mutate(x, sector = factor(sector,
levels = c(21,20,19,18,17,16,15,
           14,13,12,11,10,9,8,7,
           6,5,4,3,2,1), 
labels = c("Actividades de organizaciones y órganos extraterritoriales", 
"Actividades de los hogares como empleadores; actividades no diferenciadas de los hogares como productores de bienes y se", 
"Otras actividades de servicios",
"Actividades artísticas, de entretenimiento y recreativas",
"Actividades de atención de la salud humana y de asistencia social",
"Enseñanza", 
"Administración pública y defensa; planes de seguridad social de afiliación obligatoria ",
"Actividades de servicios administrativos y de apoyo ",
"Actividades profesionales, científicas y técnicas ",
"Actividades inmobiliarias ",
"Actividades financieras y de seguros ",
"Información y comunicaciones ",
"Actividades de alojamiento y de servicio de comidas ",
"Transporte y almacenamiento ",
"Comercio al por mayor y al por menor; reparación de vehículos automotores y motocicletas ",
"Construcción ",
"Suministro de agua; evacuación de aguas residuales, gestión de desechos y descontaminación ",
"Suministro de electricidad, gas, vapor y aire acondicionado ",
"Industrias manufactureras ",
"Explotación de minas y canteras ",
"Agricultura, ganadería, silvicultura y pesca "))))


#-------------------------------------------------------------------------------
## Homologacion de Sectores con Banco Central. 

todas[1:82] = lapply(todas[1:82],
                          function(x) mutate(x, sector = recode_factor(sector, 
                         'Agricultura'= "Agropecuario-Silvicola",
                         'Pesca'="Pesca", 
                         'Explotacion'="Mineria",
                         'Industrias'="Industria Manufacturera", 
                         'Electricidad'="Electricidad, Gas y Agua",
                         'Construccion'="Construccion", 
                         'Comercio'="Comercio",
                         'Hoteles'="Hoteles y Restoranes", 
                         'Transporte'="Transporte y Comunicaciones",
                         'Financiero'="Intermediacion Financiera", 
                         'Inmoviliarias'="Actividades Inmobiliarias, Empresariales y de Alquiler", 
                         'Ensenanza'="Servicios Sociales y Personales",
                         'Servicios sociales'="Servicios Sociales y Personales",
                         'Otras actividades'="Servicios Sociales y Personales",
                         'Hogares privados'="Servicios Sociales y Personales",
                         'Administracion Publica'="Administracion Publica",
                         'Organizaciones extraterritoriales'="Administracion Publica")))



#-------------------------------------------------------------------------------

todas = lapply(todas, function(x) mutate(x, aux = stri_length(r_p_c),
                                         prov = ifelse(aux==5,stri_sub(r_p_c,1,3),
                                                ifelse(aux==4, stri_sub(r_p_c,1,2),r_p_c))))


# CREACION DE LA VARIABLE: educ, educ2 y esc
## Esta variable identifica el ultimo nivel que la persona curso, pero no si lo termino. 
# 0 - Sin estudios 
# 1 - Basica
# 2 - Media Humanista
# 3 - M. Tecnica Profesional
# 4 - Tecnico de nivel superior
# 5 - Profesional
# 6 - Postgrado
# 7 - Ignorado


todas =lapply(todas, function(x) mutate(x, lvl  = ifelse(nivel<=2,0, 
                                      ifelse(nivel==3,1, 
                                      ifelse(nivel==4 | nivel==6,2, 
                                      ifelse(nivel==5 | nivel==14,3, 
                                      ifelse(nivel==7 | nivel==8,4,
                                      ifelse(nivel==9,5, 
                                      ifelse(nivel>=10 & nivel<=12,6, 
                                      ifelse(nivel==999,7,NA))))))))))

#-------------------------------------------------------------------------------
# 0 - Basica incompleta o sin estudios
# 1 - Basica
# 2 - Media incompleta
# 3 - Media completa
# 4 - Tecnico Nivel superior incompleto
# 5 - Tecnico Nivel superio completo
# 6 - Profesional incompleta
# 7 - Profesional completa 
# 8 - Postgrado incompleto 
# 9 - Postgrado completo 


todas = lapply(todas, function(x) mutate(x, educ = ifelse(lvl==0,0, 
                      ifelse(lvl==1 & termino_nivel==2,0, 
                      ifelse(lvl==1 & termino_nivel==1,1,
                      ifelse(lvl==2 & termino_nivel==2,2, 
                      ifelse(lvl==3 & termino_nivel==2,2, 
                      ifelse(lvl==2 & termino_nivel==1,3,
                      ifelse(lvl==3 & termino_nivel==1,3,  
                      ifelse(lvl==4 & termino_nivel==2,4, 
                      ifelse(lvl==4 & termino_nivel==1,5, 
                      ifelse(lvl==5 & termino_nivel==2,6,
                      ifelse(lvl==6 & termino_nivel==2,8,
                      ifelse(lvl==5 & termino_nivel==1,7,
                      ifelse(lvl==6 & termino_nivel==1,9,
                      ifelse(lvl==7,99,NA))))))))))))))) )


#-------------------------------------------------------------------------------
# 0 - Basica incompleta o sin estudios 
# 1 - Basica completa
# 2 - Media completa
# 3 - Tecnico nivel superior completa 
# 4 - Profesional
# 5 - Postgrado 


todas = lapply(todas, function(x) mutate(x, educ2 = ifelse(educ==0,0, 
                                      ifelse(educ==1,1,
                                      ifelse(educ==2,1,
                                      ifelse(educ==3,2,
                                      ifelse(educ==4,2,
                                      ifelse(educ==5,3,
                                      ifelse(educ==6,2,
                                      ifelse(educ==7,4,
                                      ifelse(educ==8,4,
                                      ifelse(educ==9,5,NA))))))))))))   


#-------------------------------------------------------------------------------

# 0 - Basica incompleta o sin estudios 
# 1 - Basica completa
# 2 - Media completa
# 3 - Tecnico nivel superior completo
# 4 - Profesional o mas



todas = lapply(todas, function(x) mutate(x, educ3 = 
                      ifelse(educ==0,0, 
                      ifelse(educ==1,1,
                      ifelse(educ==2,1,
                      ifelse(educ==3,2,
                      ifelse(educ==4,2,
                      ifelse(educ==5,3,
                      ifelse(educ==6,2,
                      ifelse(educ==7,4,
                      ifelse(educ==8,4,
                      ifelse(educ==9,4,NA))))))))))))   

#-------------------------------------------------------------------------------
#

todas = lapply(todas, function(x) mutate(x,esc = ifelse(nivel<=3, 0,
                    ifelse(nivel==4, 8,
                    ifelse(nivel==5, 8,
                    ifelse(nivel==6, 6,
                    ifelse(nivel==7, 12,
                    ifelse(nivel==8, 12,
                    ifelse(nivel==9, 12,
                    ifelse(nivel==10, 16,
                    ifelse(nivel==11, 16,
                    ifelse(nivel==12, 16,
                    ifelse(nivel==14, 6,
                    ifelse(nivel==999, NA,NA)))))))))))),
                    esc = ifelse(nivel<=9 | nivel==14,
                                 esc+curso, esc),
                    esc = ifelse(nivel==11,
                                 esc+pmin(2,curso),
                                 esc),
                    esc = ifelse(nivel==12,
                                 esc+pmin(5,curso),
                                 esc)))


#-------------------------------------------------------------------------------
# Incorporar el info y realizar inferencia para muestras complejas
#-------------------------------------------------------------------------------

#La encuesta 78 tiene una observación perdida, lo que generará un error al ncorporar el disño muestral de la encuesta. 

which(is.na(todas[[78]]$fact))

todas[[78]] = todas[[78]][-10429,] # quito la observación problemática.

#Para evitar clusters con una sola observacion
options(survey.lonely.psu = "certainty") 

# info muestral de la NENE
info =lapply(1:length(nene), function(i) svydesign(id = ~id_directorio, strata = ~estrato, weights  = ~fact,nest = TRUE, data = todas[[i]]))

#-------------------------------------------------------------------------------
# Tasa SU1 Nacional (Subutilización de la fuerza de trabajo)
#-------------------------------------------------------------------------------
#

n = length(todas)

SU1 = lapply(1:n, function(i)  svyratio(~I(cae_general=="Desocupado" | 
                              cae_general=="Busca Trabajo Primera Vez" |
                              cae_especifico==10), 
                              denominator=~I(cae_general=="Ocupado" | 
                              cae_general=="Desocupado" |
                              cae_general=="Busca Trabajo Primera Vez"|
                              cae_especifico==10), 
                              info[[i]], multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)) %>% 
  flatten()


tasa.desocupacion.combinada.SU1 = unlist(lapply(tasa.desocupacion.combinada.SU1., '[[', 1) ) 

ee.tasa.desocupacion.combinada.SU1 = unlist(lapply(tasa.desocupacion.combinada.SU1., SE))

tasa.desocupacion.combinada.SU1.nacional = data.frame(tasa.desocupacion.combinada.SU1, ee.tasa.desocupacion.combinada.SU1)

write.csv(tasa.desocupacion.combinada.SU1.nacional, "tasa_desocupacion_combinada_SU1_nacional.csv")

#-------------------------------------------------------------------------------
# Tasa SU2 Nacional
#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLE: SUBEMPLEO
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, subempleo =
                                 ifelse(c1==2 & c10==1 & (c11==1 | c11==2) & habituales<=30,1,0))
}

tasa.desocupacion.combinada. = list()
for (i in 1:length(info)){
  tasa.desocupacion.combinada.[[i]] = svyratio(~I(subempleo==1 | 
                                         cae_general=="Desocupado" | 
                                          cae_general=="Busca Trabajo Primera Vez"| 
                                           cae_especifico==10), 
                                    denominator=~I(cae_general=="Ocupado" | 
                                                   cae_general=="Desocupado" |
                                                    cae_general=="Busca Trabajo Primera Vez"| 
                                                     cae_especifico==10), info[[i]], 
                                    multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.desocupacion.combinada = unlist(lapply(tasa.desocupacion.combinada., '[[', 1) ) 

ee.tasa.desocupacion.combinada = unlist(lapply(tasa.desocupacion.combinada., SE))

tasa.desocupacion.combinada.nacional = data.frame(tasa.desocupacion.combinada, ee.tasa.desocupacion.combinada)

write.csv(tasa.desocupacion.combinada.nacional, "tasa_desocupacion_combinada_SU2_nacional.csv")
#-------------------------------------------------------------------------------
# Tasa SU3 Nacional
#-------------------------------------------------------------------------------
#
tasa.desocupacion.combinada.SU3. = list()
for (i in 1:length(info)){
  tasa.desocupacion.combinada.SU3.[[i]] = svyratio(~I(cae_general=="Desocupado" | 
                                                    cae_general=="Busca Trabajo Primera Vez" |
                                                    cae_especifico==10 |
                                                    cae_general=="Inactivos Potencialmente activos"), 
                                               denominator=~I(cae_general=="Ocupado" | 
                                                                cae_general=="Desocupado" |
                                                                 cae_general=="Busca Trabajo Primera Vez"|
                                                                  cae_especifico==10 |
                                                                   cae_general=="Inactivos Potencialmente activos"), 
                                               info[[i]], 
                                               multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.desocupacion.combinada.SU3 = unlist(lapply(tasa.desocupacion.combinada.SU3., '[[', 1) ) 

ee.tasa.desocupacion.combinada.SU3 = unlist(lapply(tasa.desocupacion.combinada.SU3., SE))

tasa.desocupacion.combinada.SU3.nacional = data.frame(tasa.desocupacion.combinada.SU3, ee.tasa.desocupacion.combinada.SU3)

write.csv(tasa.desocupacion.combinada.SU3.nacional, "tasa_desocupacion_combinada_SU3_nacional.csv")

#-------------------------------------------------------------------------------
# Tasa SU4 Nacional
#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLE: SUBEMPLEO
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, subempleo =
                                 ifelse(c1==2 & c10==1 & (c11==1 | c11==2) & habituales<=30,1,0))
}

#
tasa.desocupacion.combinada.SU4. = list()
for (i in 1:length(info)){
  tasa.desocupacion.combinada.SU4.[[i]] = svyratio(~I(subempleo==1 | cae_general=="Desocupado" | 
                                                        cae_general=="Busca Trabajo Primera Vez" |
                                                        cae_especifico==10 |
                                                        cae_general=="Inactivos Potencialmente activos"), 
                                                   denominator=~I(cae_general=="Ocupado" | 
                                                                    cae_general=="Desocupado" |
                                                                    cae_general=="Busca Trabajo Primera Vez"|
                                                                    cae_especifico==10 |
                                                                    cae_general=="Inactivos Potencialmente activos"), 
                                                   info[[i]], 
                                                   multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.desocupacion.combinada.SU4 = unlist(lapply(tasa.desocupacion.combinada.SU4., '[[', 1) ) 

ee.tasa.desocupacion.combinada.SU4 = unlist(lapply(tasa.desocupacion.combinada.SU4., SE))

tasa.desocupacion.combinada.SU4.nacional = data.frame(tasa.desocupacion.combinada.SU4, ee.tasa.desocupacion.combinada.SU4)

write.csv(tasa.desocupacion.combinada.SU4.nacional, "tasa_desocupacion_combinada_SU4_nacional.csv")



#-------------------------------------------------------------------------------
# Tasa SU1 Nuble
#-------------------------------------------------------------------------------
#
tasa.desocupacion.combinada.SU1. = list()
for (i in 1:length(info)){
  tasa.desocupacion.combinada.SU1.[[i]] = svyratio(~I(cae_general=="Desocupado" | 
                                                        cae_general=="Busca Trabajo Primera Vez" |
                                                        cae_especifico==10), 
                                                   denominator=~I(cae_general=="Ocupado" | 
                                                                    cae_general=="Desocupado" |
                                                                    cae_general=="Busca Trabajo Primera Vez"|
                                                                    cae_especifico==10), 
                                                   subset(info[[i]],prov==84), multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.desocupacion.combinada.SU1 = unlist(lapply(tasa.desocupacion.combinada.SU1., '[[', 1) ) 

ee.tasa.desocupacion.combinada.SU1 = unlist(lapply(tasa.desocupacion.combinada.SU1., SE))

tasa.desocupacion.combinada.SU1.nacional = data.frame(tasa.desocupacion.combinada.SU1, ee.tasa.desocupacion.combinada.SU1)

write.csv(tasa.desocupacion.combinada.SU1.nacional, "tasa_desocupacion_combinada_SU1_nacional.csv")

#-------------------------------------------------------------------------------
# Tasa SU2 Nuble
#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLE: SUBEMPLEO
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, subempleo =
                                 ifelse(c1==2 & c10==1 & (c11==1 | c11==2) & habituales<=30,1,0))
}

tasa.desocupacion.combinada. = list()
for (i in 1:length(info)){
  tasa.desocupacion.combinada.[[i]] = svyratio(~I(subempleo==1 | 
                                                    cae_general=="Desocupado" | 
                                                    cae_general=="Busca Trabajo Primera Vez"| 
                                                    cae_especifico==10), 
                                               denominator=~I(cae_general=="Ocupado" | 
                                                                cae_general=="Desocupado" |
                                                                cae_general=="Busca Trabajo Primera Vez"| 
                                                                cae_especifico==10), subset(info[[i]],prov==84), 
                                               multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.desocupacion.combinada = unlist(lapply(tasa.desocupacion.combinada., '[[', 1) ) 

ee.tasa.desocupacion.combinada = unlist(lapply(tasa.desocupacion.combinada., SE))

tasa.desocupacion.combinada.nacional = data.frame(tasa.desocupacion.combinada, ee.tasa.desocupacion.combinada)

write.csv(tasa.desocupacion.combinada.nacional, "tasa_desocupacion_combinada_SU2_nacional.csv")
#-------------------------------------------------------------------------------
# Tasa SU3 Nuble
#-------------------------------------------------------------------------------
#
tasa.desocupacion.combinada.SU3. = list()
for (i in 1:length(info)){
  tasa.desocupacion.combinada.SU3.[[i]] = svyratio(~I(cae_general=="Desocupado" | 
                                                        cae_general=="Busca Trabajo Primera Vez" |
                                                        cae_especifico==10 |
                                                        cae_general=="Inactivos Potencialmente activos"), 
                                                   denominator=~I(cae_general=="Ocupado" | 
                                                                    cae_general=="Desocupado" |
                                                                    cae_general=="Busca Trabajo Primera Vez"|
                                                                    cae_especifico==10 |
                                                                    cae_general=="Inactivos Potencialmente activos"), 
                                                   subset(info[[i]],prov==84), 
                                                   multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.desocupacion.combinada.SU3 = unlist(lapply(tasa.desocupacion.combinada.SU3., '[[', 1) ) 

ee.tasa.desocupacion.combinada.SU3 = unlist(lapply(tasa.desocupacion.combinada.SU3., SE))

tasa.desocupacion.combinada.SU3.nacional = data.frame(tasa.desocupacion.combinada.SU3, ee.tasa.desocupacion.combinada.SU3)

write.csv(tasa.desocupacion.combinada.SU3.nacional, "tasa_desocupacion_combinada_SU3_nacional.csv")

#-------------------------------------------------------------------------------
# Tasa SU4 Nuble
#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLE: SUBEMPLEO
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, subempleo =
                                 ifelse(c1==2 & c10==1 & (c11==1 | c11==2) & habituales<=30,1,0))
}

#
tasa.desocupacion.combinada.SU4. = list()
for (i in 1:length(info)){
  tasa.desocupacion.combinada.SU4.[[i]] = svyratio(~I(subempleo==1 | cae_general=="Desocupado" | 
                                                        cae_general=="Busca Trabajo Primera Vez" |
                                                        cae_especifico==10 |
                                                        cae_general=="Inactivos Potencialmente activos"), 
                                                   denominator=~I(cae_general=="Ocupado" | 
                                                                    cae_general=="Desocupado" |
                                                                    cae_general=="Busca Trabajo Primera Vez"|
                                                                    cae_especifico==10 |
                                                                    cae_general=="Inactivos Potencialmente activos"), 
                                                   subset(info[[i]],prov==84), 
                                                   multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.desocupacion.combinada.SU4 = unlist(lapply(tasa.desocupacion.combinada.SU4., '[[', 1) ) 

ee.tasa.desocupacion.combinada.SU4 = unlist(lapply(tasa.desocupacion.combinada.SU4., SE))

tasa.desocupacion.combinada.SU4.nacional = data.frame(tasa.desocupacion.combinada.SU4, ee.tasa.desocupacion.combinada.SU4)

write.csv(tasa.desocupacion.combinada.SU4.nacional, "tasa_desocupacion_combinada_SU4_nacional.csv")

#-------------------------------------------------------------------------------
# SU2 por sexo Nuble
#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLE: SUBEMPLEO
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, subempleo =
                                 ifelse(c1==2 & c10==1 & (c11==1 | c11==2) & habituales<=30,1,0))
}

#
SU2.sexo = list()
for (i in 1:length(info)){
      SU2.sexo[[i]]=svyby(~I(subempleo==1 | 
                               cae_general=="Desocupado" | 
                               cae_general=="Busca Trabajo Primera Vez"| 
                               cae_especifico==10),
                          denominator=~I(cae_general=="Ocupado" | 
                                           cae_general=="Desocupado" |
                                           cae_general=="Busca Trabajo Primera Vez"| 
                                           cae_especifico==10),
                                 by=~sexo, subset(info[[i]],prov==84), 
                                 svyratio, multicore = TRUE, 
                                 drop.empty.groups = FALSE, na.rm=TRUE)
}

SU2.sexo. = 
  do.call(rbind, SU2.sexo)

names(SU2.sexo.) = 
  c("sexo","SU2", "error estandar")

write.csv(SU2.sexo., "SU2_sexo.csv")

#-------------------------------------------------------------------------------
# SU2 por sexo Nacional
#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLE: SUBEMPLEO
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, subempleo =
                                 ifelse(c1==2 & c10==1 & (c11==1 | c11==2) & habituales<=30,1,0))
}

#
SU2.sexo = list()
for (i in 1:length(info)){
  SU2.sexo[[i]]=svyby(~I(subempleo==1 | 
                           cae_general=="Desocupado" | 
                           cae_general=="Busca Trabajo Primera Vez"| 
                           cae_especifico==10),
                      denominator=~I(cae_general=="Ocupado" | 
                                       cae_general=="Desocupado" |
                                       cae_general=="Busca Trabajo Primera Vez"| 
                                       cae_especifico==10),
                      by=~sexo, info[[i]], 
                      svyratio, multicore = TRUE, 
                      drop.empty.groups = FALSE, na.rm=TRUE)
}

SU2.sexo. = 
  do.call(rbind, SU2.sexo)

names(SU2.sexo.) = 
  c("sexo","SU2", "error estandar")

write.csv(SU2.sexo., "SU2_sexo.csv")


#-------------------------------------------------------------------------------
# SU2 por educ3 Nuble
#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLE: SUBEMPLEO
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, subempleo =
                                 ifelse(c1==2 & c10==1 & (c11==1 | c11==2) & habituales<=30,1,0))
}

#
SU2.educ = list()
for (i in 1:length(info)){
  SU2.educ[[i]]=svyby(~I(subempleo==1 | 
                           cae_general=="Desocupado" | 
                           cae_general=="Busca Trabajo Primera Vez"| 
                           cae_especifico==10),
                      denominator=~I(cae_general=="Ocupado" | 
                                       cae_general=="Desocupado" |
                                       cae_general=="Busca Trabajo Primera Vez"| 
                                       cae_especifico==10),
                      by=~educ3,svyratio, subset(info[[i]],prov==84), multicore = TRUE, 
                      drop.empty.groups = FALSE, na.rm=TRUE)
}

SU2.educ. = 
  do.call(rbind, SU2.educ)

names(SU2.educ.) = 
  c("educ3","SU2", "error estandar")

write.csv(SU2.educ., "SU2_educ.csv")

#-------------------------------------------------------------------------------
# SU2 por educ3 Nacional
#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLE: SUBEMPLEO
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, subempleo =
                                 ifelse(c1==2 & c10==1 & (c11==1 | c11==2) & habituales<=30,1,0))
}

#
SU2.educ = list()
for (i in 1:length(info)){
  SU2.educ[[i]]=svyby(~I(subempleo==1 | 
                           cae_general=="Desocupado" | 
                           cae_general=="Busca Trabajo Primera Vez"| 
                           cae_especifico==10),
                      denominator=~I(cae_general=="Ocupado" | 
                                       cae_general=="Desocupado" |
                                       cae_general=="Busca Trabajo Primera Vez"| 
                                       cae_especifico==10),
                      by=~educ3, info[[i]], 
                      svyratio, multicore = TRUE, 
                      drop.empty.groups = FALSE, na.rm=TRUE)
}

SU2.educ. = 
  do.call(rbind, SU2.educ)

names(SU2.educ.) = 
  c("educ","SU2", "error estandar")

write.csv(SU2.educ., "SU2_educ.csv")

# FIN