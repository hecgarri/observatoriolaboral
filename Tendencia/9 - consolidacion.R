setwd("~/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Encuesta de Caracterización Socioeconómica Nacional (CASEN)/")
ocupados = list.files(pattern="ocupados")
ocupados_ = lapply(ocupados, function(x) read.csv(x))
ocupados__ = do.call(rbind, ocupados_)

write.csv(ocupados__, "ocupados.csv")

concentracion = list.files(pattern="concentracion")
concentracion_ = lapply(concentracion, function(x) read.csv(x))
concentracion__ = do.call(rbind, concentracion_)

write.csv(concentracion__, "concentracion.csv")

ingreso = c("ingreso1998.csv", "ingreso2000.csv", "ingreso2003.csv", "ingreso2006.csv", 
            "ingreso2009.csv", "ingreso2011.csv", "ingreso2013.csv", "ingreso2015.csv")
ingreso_ = lapply(ingreso, function(x) read.csv(x))
ingreso__ = do.call(rbind, ingreso_)

write.csv(ingreso__,"ingreso.csv")

escolaridad = list.files(pattern = "escolaridad")
escolaridad_ = lapply(escolaridad, function(x) read.csv(x))
escolaridad__ = do.call(rbind, escolaridad_)

write.csv(escolaridad__, "escolaridad.csv")
