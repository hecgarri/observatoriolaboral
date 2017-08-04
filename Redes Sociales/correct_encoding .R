rm(list=ls())
#Fijo el directorio de trabajo 
setwd("~/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/GitHub/observatoriolaboral/Redes Sociales")
require(parallel)
require(magrittr)

codepages <- setNames(iconvlist(), iconvlist())

x <- mclapply(codepages, 
    function(enc) try(read.table('2017-07-31 Datos de trabajo en Chillan.csv',
    fileEncoding=enc, nrows=3, header=TRUE,
    sep=",")), mc.cores = 2) # you get lots of errors/warning here

X = do.call(rbind, sapply(x, dim)) %>% data.frame()

write.csv(X, "a lot of encodings-csv")


unique(do.call(rbind, sapply(x, dim)))

maybe_ok <- sapply(x, function(x) isTRUE(all.equal(dim(x), c(3,12))))

posibles = codepages[maybe_ok] %>% data.frame()

write.csv(posibles, "posibles encodings.csv")