direct = "https://raw.githubusercontent.com/hecgarri/Reporte-panorama-regional/master/"

x = c("c10.R", "c11.R")

download.file(paste0(direct, x),x)

file.edit(x)