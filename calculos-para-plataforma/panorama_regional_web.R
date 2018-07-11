## NO USAR ESTE ARCHIVO DE MOMENTO ¡TIENE ERRORES!
## panorama_regional_web.R
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

path = file.path("/home/hector/GoogleDriveUBB",
                 "OLR Ñuble - Observatorio laboral de Ñuble",
                 "Bases de datos",
                 "Encuesta de Caracterización Socioeconómica Nacional (CASEN)/")

setwd(path)

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


################################################
## Extracción de directorios y etiquetas
################################################
#casen  = list.files(pattern=".sav$")
#n = length(casen)
#sub = casen[1:n]
###############################################
## Se importan todas las bases de una sola vez
###############################################
#todas =  mclapply(mc.cores = 4,sub,
#                  function(x) read.spss(x, use.value.labels = FALSE, 
#                                        to.data.frame = FALSE, use.missings = FALSE))
#etiquetas = lapply(1:n, function(x) etiquetas.casen = attr(todas[[x]], "label.table"))
#directorio.casen = lapply(1:n, function(x) attr(todas[[x]],"variable.labels"))
#directorio.casen = lapply(1:n, function(x) data.frame(names(directorio.casen[[x]]),
#                                                      directorio.casen[[x]]))
#sub = gsub(".sav","",sub)
#mclapply(1:length(sub), mc.cores = 4,
#         function(x) fwrite(directorio.casen[[x]],paste0("directorio_",sub[x],".csv")))
#lapply(1:n, function(x) saveRDS(todas[[x]], file=paste0("etiquetas_",sub[x],".rds")))

#################################################
# Para evitar que las variables numéricas usen notación científica
# Lo que haré es reemplazar todas las variables nuémricas a tipo entero
#################################################
casen  = list.files(pattern=".sav$")
n = length(casen)
sub = casen[1:n]
todas =  mc.lapply(sub,function(x) read_sav(x))
todas = lapply(1:n, function(x) todas[[x]] %>% mutate_if(is.numeric,as.integer))
################################################
# Creación de una base de datos 
################################################
# Conexión para consultas SQL 

casen = list.files(pattern = ".csv$")
n = length(casen)
db = dbConnect(RSQLite::SQLite(), dbname = "CASEN")
casen_ = gsub(".csv","", casen)
## Lleno la base de datos con los archivos .csv
lapply(8, function(x) dbWriteTable(conn = db, name = casen_[x], value =casen[x],
                                     sep = ";", overwrite = TRUE))

dbListTables(db)


casen2015 = fread("casen2015.csv")
dbListFields(db, "casen2013")

apply(casen2015,2,class)


