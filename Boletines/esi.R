rm(list=ls())
if (!require(foreign)) install.packages("foreign"); require(foreign)
if (!require(dplyr)) install.packages("dplyr"); require(dplyr)

ESI <- read.spss("GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Encuesta Suplementaria de Ingresos (ESI)/ESI 2015 Personas.sav", 
                 to.data.frame = FALSE, use.value.labels = FALSE)

ESI.df = data.frame(ESI)
nombres = names(ESI.df[,grep(pattern = "^ING", names(ESI.df))])

etiquetas = attr(ESI, "variable.labels")

ingresos = data.frame(names = names(ESI.df), labels = etiquetas) %>% filter(grepl(pattern = "^ING", names(ESI.df)))

