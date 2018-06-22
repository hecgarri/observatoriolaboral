## main.R
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



rm(list = ls())
gc()

if (!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE); require(tidyverse)
if (!require(RSQLite)) install.packages("sqldf"); require(RSQLite)
if (!require(stringr))     install.packages("stringr"); require(stringr)
if (!require(stringi))     install.packages("stringi"); require(stringi)
if (!require(survey)) install.packages("survey"); require(survey)
if (!require(lubridate)) install.packages("lubridate"); require(lubridate)
if (!require(data.table)) install.packages("data.table"); require(data.table)
if (!require(srvyr)) install.packages("srvyr"); require(srvyr)
require(parallel)

path = file.path("/home/hector/GoogleDriveUBB",
                  "OLR Ñuble - Observatorio laboral de Ñuble",
                  "Bases de datos",
                  "Nueva Encuesta Nacional de Empleo (ENE)/")

setwd(path)

# ENEs
# 2010:01-11: Central: 01,04,07,10 
# 2011:12-23: Central: 13,16,19,22
# 2012:24-35: Central: 25,28,31,34
# 2013:36-47: Central: 37,40,43,46
# 2014:48-59: Central: 49,52,55,58
# 2015:60-71: Central: 61,64,67,70
# 2016:72-83: Central: 73,76,79,82
# 2017:85-94: Central: 85, 88,91,94

nene = list.files(pattern = ".csv$")
nenes = gsub(".csv","", nene)
x = seq(85,94,3)
nenes = nenes[x]
n = length(nenes)


# Se importan todas las bases establecidas en la secuencia de una sola vez
##todas =  mclapply(mc.cores=4,nene[1:n], function(x) fread(x, sep=",", header=TRUE))

# La variable e19_otro a6_otro b16_otro e19_otro estaban dando problemas con las comas, tuve
# que eliminarlas en cada una de las bases de datos
##todas = mclapply(mc.cores = 4,1:n,function(x) todas[[x]] %>% select(-a6_otro,-b16_otro,-e19_otro))
##mclapply(1:n, mc.cores = 1, function(x) fwrite(todas[[x]],nene[x]))

db = dbConnect(RSQLite::SQLite(), dbname = "ENE")

# Creo una base de datos con todas las encuestas disponibles
#lapply(91, function(x) dbWriteTable(conn = db, name = nenes[x], value =nene[x],
#                                     sep = ",", overwrite = TRUE))

dbListTables(db)

dbListFields(db, nenes[1])

todas = lapply(1:n, function(x) tbl(db, nenes[x])) 

todas = lapply(1:n, 
        function(x) todas[[x]] %>% 
          select("id_directorio", "estrato", "nacionalidad", 
          "fact", "cae_general", "mes_central", 
          "ano_trimestre", "b18_codigo", "region", 
          "categoria_ocupacion","b15_1", "b15_2", 
          "b8", "b9", "b2",
          "nivel", "curso", "edad", "r_p_c","b1", "sexo",
          "termino_nivel", "c1", "c10", "c11", "habituales",
          "e9", "e12", "e19"))

todas = lapply(1:n, function(x) todas[[x]] %>% tbl_df)


todas = lapply(1:n, function(x) todas[[x]] %>% 
                 mutate(cat_ocup = 
                  recode(categoria_ocupacion, `1`= 1, 
                `2`= 2, `3` =3, `4`=3, `5`=3, `6`=3,
                `7`=4, .default = 0, .missing = 99),
                cat_ocup = ifelse(cat_ocup==3 & b8==2,3,
                ifelse(cat_ocup==3 & b8==1 & b9==1,4, 
                ifelse(cat_ocup==3 & b8==1 & b9==2,5, 
                ifelse(cat_ocup==1,1, 
                ifelse(cat_ocup==2,2, 
                ifelse(cat_ocup==4,6,NA)))))),
                cat_ocup = factor(cat_ocup, 
                levels=c(1,2,3,4,5,6),
                labels=c("empleador","cuenta propia",
                         "asalariado sin contrato", 
                "asalariado con contrato definido",
                "asalariado con contrato indefinido",
                "no remunerado")), 
                aux = stri_length(r_p_c), 
                prov = ifelse(aux==4, stri_sub(r_p_c,1,2),
                              stri_sub(r_p_c,1,3)),
                aux2 = stri_length(b18_codigo), 
                prov_e = ifelse(aux2==4, stri_sub(b18_codigo,1,2),
                         ifelse(aux==5, stri_sub(b18_codigo,1,3),NA)), 
                conmutante_nuble = ifelse(prov == 84 & prov_e == 84, "residente", 
                                          ## Vive y trabaja 
                                   ifelse(prov == 84 & prov_e != 84, "Conmutante a", 
                                          ## Vive en Ñuble y trabaja en otro lugar
                                   ifelse(prov != 84 & prov_e == 84, "Conmutante b"
                                          ## Viven en otro lugar, pero trabaja en Ñuble
                                          , NA))),
                cae_general = recode(cae_general,`0`=0,`1` = 1,`2` = 1,
                                     `3` = 1, `4` = 2,`5` = 3,`6` = 4,
                                     `7` = 4,`8` = 4,`9` = 4), 
                cae_general = factor(cae_general,levels = c(0,1,2,3,4), 
                                     labels = c("Menor de 15","Ocupado", "Cesante", 
                                                "Busca Trabajo Primera Vez","Inactivo")),
                activo = ifelse(cae_general != "Inactivo" & 
                                  cae_general != "Menor de 15",1,0), 
                edad_activ = ifelse(cae_general != "Menor de 15",1,0), 
                desocupado = ifelse(cae_general == "Cesante" | 
                                    cae_general == "Busca Trabajo Primera Vez",1,0), 
                ocupado = ifelse(cae_general == "Ocupado",1,0)))


options(survey.lonely.psu = "certainty") 
diseno = mclapply(mc.cores = 4, 1:n, function(x) todas[[x]] %>% 
                    as_survey_design(id = id_directorio, strata = estrato,
                                     weights  = fact,nest = TRUE))

## Tasa de participación por sexo en Ñuble 

tasa_participacion = lapply(1:n, function(x) diseno[[x]] %>% 
                              filter(prov == 84 | (conmutante_nuble!= "Conmutante a" & conmutante_nuble != "Conmutante b")) %>% 
                              group_by(sexo) %>% 
                              summarise(estimate = survey_ratio(numerator = activo,
                                                                denominator = edad_activ, 
                                                                na.rm = TRUE, 
                                                                vartype = "cv")) %>% 
                              mutate(encuesta = nenes[x], 
                                     indicador = "Tasa de participación")) %>% bind_rows()

## Tasa de desocupación por sexo en Ñuble 
tasa_desocupacion = lapply(1:n, function(x) diseno[[x]] %>% 
                              filter(prov == 84 | (conmutante_nuble!= "Conmutante a" & conmutante_nuble != "Conmutante b")) %>% 
                              group_by(sexo) %>% 
                              summarise(estimate = survey_ratio(numerator = desocupado,
                                                                denominator = activo, 
                                                                na.rm = TRUE, 
                                                                vartype = "cv")) %>% 
                              mutate(encuesta = nenes[x], 
                                     indicador = "Tasa de desocupación")) %>% bind_rows()

## Tasa de ocupación por sexo en Ñuble 
tasa_ocupacion = lapply(1:n, function(x) diseno[[x]] %>% 
                             filter(prov == 84 | (conmutante_nuble!= "Conmutante a" & conmutante_nuble != "Conmutante b")) %>% 
                             group_by(sexo) %>% 
                             summarise(estimate = survey_ratio(numerator = ocupado,
                                                               denominator = edad_activ, 
                                                               na.rm = TRUE, 
                                                               vartype = "cv")) %>% 
                             mutate(encuesta = nenes[x], 
                                    indicador = "Tasa de ocupación")) %>% bind_rows()

indicadores_c1 = rbind(tasa_participacion, tasa_ocupacion, tasa_desocupacion)

cuadro_1 = indicadores_c1 %>% group_by(sexo, indicador) %>% 
  summarise(anual = (mean(estimate)*100) %>% round(1))

cuadro_1 = cbind(cuadro_1[c(1,2,3),c(2,3)], cuadro_1[c(4,5,6),3])

path_file = file.path("/home/hector/GoogleDriveUBB/",
"OLR Ñuble - Observatorio laboral de Ñuble/",
"Análisis Cuantitativo/observatoriolaboral/",
"Reportes-segundo-año/")

write.csv(cuadro_1, paste0(path_file,"cuadro_1.csv"))