rm(list=ls())
#Fijo el directorio de trabajo 
setwd("~/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/GitHub/observatoriolaboral/Redes Sociales")
require(parallel)

codepages <- setNames(iconvlist(), iconvlist())

x <- mclapply(codepages, 
    function(enc) try(read.table('2017-07-31 Datos de trabajo en Chillan.csv',
    fileEncoding=enc, nrows=3, header=TRUE,
    sep="")), mc.cores = 3) # you get lots of errors/warning here