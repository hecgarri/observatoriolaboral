rm(list=ls())

path = file.path("/home/hector/GoogleDriveUBB",
                 "OLR Ñuble - Observatorio laboral de Ñuble",
                 "Bases de datos/Base de Datos Seguro de Cesantía",
                 "muestraasc5%") 
setwd(path)

if (!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
if (!require(sqldf)) install.packages("sqldf"); require(sqldf)
if (!require(XLConnect)) install.packages("XLConnect"); require(XLConnect)

####################################
## Creacioń de la base de datos 
####################################

db = dbConnect(SQLite(), dbname = "seguro")

dbWriteTable(conn = db, name = "afiliados", value ="1_afiliados.csv",
             row.names=FALSE, header = TRUE, sep = ";")
dbWriteTable(conn = db, name = "rentas_imponibles", value ="5_rentas_imponibles.csv",
             row.names=FALSE, header = TRUE, sep = ";")

dbListTables(db)
dbListFields(db, "afiliados") 
afil  = dbReadTable(db, "afiliados") 
##################################
## Análisis de la base de datos
##################################

