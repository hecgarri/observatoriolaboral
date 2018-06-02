rm(list = ls())
gc()

if (!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE); require(tidyverse)
if (!require(RSQLite)) install.packages("sqldf"); require(RSQLite)
if (!require(stringr))     install.packages("stringr"); require(stringr)
if (!require(survey)) install.packages("survey"); require(survey)
if (!require(lubridate)) install.packages("lubridate"); require(lubridate)
if (!require(data.table)) install.packages("data.table"); require(data.table)
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

com_nuble = c(8401,8402,84,03,8404,8405,8406,8407,8408,8409,
              8410,8411,8412,8413,8414,8415,8416,8417,8418,
              8419,8420,8421)

todas = lapply(1:n, function(x) todas[[x]] %>% dplyr::filter(r_p_c %in% com_nuble))

todas = lapply(1:n, function(x) tbl_df(todas[[x]]))

