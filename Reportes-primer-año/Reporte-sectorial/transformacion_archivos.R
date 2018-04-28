#Limpia espacio de trabajo
rm(list = ls())

# Fijar directorio de trabajo
setwd("C:\\Users\\Miguelo\\Documents\\NENE\\")
#setwd("/home/hector/Dropbox/Observatorio Regional/Bases NENE/NENE/")

# Para importar bases de datos en multiples formatos
library(foreign)

# Se realiza un listado de todos los archivos que comienzan con nene2010

nene  = c("nene20172.sav")

# Se importan todas las bases del a±o 2010 de una sola vez
todas =  lapply(nene, function(x) read.spss(x,use.value.labels  =  FALSE,
                                            use.missings  =  TRUE,
                                            to.data.frame  =  TRUE))
for (i in 1:length(todas)){
  write.csv(todas[[i]],paste0("nene_",85, ".csv", collapse=""))
}

# FIN
