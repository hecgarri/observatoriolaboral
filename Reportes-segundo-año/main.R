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

nene = list.files(pattern = ".csv$")
nenes = gsub(".csv","", nene)
n = length(nenes)

# Se importan todas las bases establecidas en la secuencia de una sola vez
##todas =  mclapply(mc.cores=4,nene[1:n], function(x) fread(x, sep=",", header=TRUE))

# La variable e19_otro a6_otro b16_otro e19_otro estaban dando problemas con las comas, tuve
# que eliminarlas en cada una de las bases de datos
##todas = mclapply(mc.cores = 4,1:n,function(x) todas[[x]] %>% select(-a6_otro,-b16_otro,-e19_otro))
##mclapply(1:n, mc.cores = 1, function(x) fwrite(todas[[x]],nene[x]))

db = dbConnect(RSQLite::SQLite(), dbname = "ENE")

# Creo una base de datos con todas las encuestas disponibles
##lapply(1:n, function(x) dbWriteTable(conn = db, name = nenes[x], value =nene[x],
##                                     sep = ",", overwrite = TRUE))

dbListTables(db)

dbListFields(db, nenes[1])

todas = lapply(1:n, function(x) tbl(db, nenes[x])) 

todas = lapply(1:n, function(x) todas[[x]] %>% select("id_directorio", "estrato", "nacionalidad", 
                                                        "fact", "cae_general", "mes_central", 
                                                        "ano_trimestre", "b18_codigo", "region", 
                                                        "categoria_ocupacion", "b14", "b15_1", "b15_2", 
                                                        "b8", "b9", "b7_3", "b7_4", "b7_1", "b7_2", "b2", 
                                                        "nivel", "curso", "edad", "r_p_c","b1", "sexo",
                                                        "termino_nivel", "c1", "c10", "c11", "habituales", 
                                                        "e18", "e9", "e12", "e19"))

todas = mclapply(mc.cores = 4, 1:n, function(x) todas[[x]] %>% tbl_df)


todas = lapply(1:n, recode(info[[i]]$variables$categoria_ocupacion, `1`= 1, 
                                        `2`= 2, `3` =3, `4`=3, `5`=3, `6`=3,
                                        `7`=4, .default = 0, .missing = 99)


for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, cat.ocup = ifelse(cat.ocup==3 & b8==2,3,
                                                                      ifelse(cat.ocup==3 & b8==1 & b9==1,4, 
                                                                             ifelse(cat.ocup==3 & b8==1 & b9==2,5, 
                                                                                    ifelse(cat.ocup==1,1, 
                                                                                           ifelse(cat.ocup==2,2, 
                                                                                                  ifelse(cat.ocup==4,6,NA)))))))
}

for (i in 1:length(info)){
  info[[i]]$variables$cat.ocup = factor(info[[i]]$variables$cat.ocup, 
                                        levels=c(1,2,3,4,5,6),
                                        labels=c("empleador","cuenta propia","asalariado sin contrato", 
                                                 "asalariado con contrato definido",
                                                 "asalariado con contrato indefinido",
                                                 "no remunerado"))
}


options(survey.lonely.psu = "certainty") 
diseno = mclapply(mc.cores = 4, 1:n, function(x) todas[[x]] %>% 
                    as_survey_design(id = id_directorio, strata = estrato,
                                     weights  = fact,nest = TRUE))



# Cuadro 11. Categoría ocupacional de los ocupados de Ñuble según sector, 2016