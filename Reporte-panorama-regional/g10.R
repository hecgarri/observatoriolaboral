#-------------------------------------------------------------------------------
# GRAFICO 10: Evolucion ocupados por sector economico de Nuble, 2010-2016.
#-------------------------------------------------------------------------------
# Residentes por sector en nuble
trabajan.nuble.sector = list()
for (i in 1:length(info)){
  trabajan.nuble.sector[[i]] = svyby(~I(prov_e==84),
                                     by=~sector2, info[[i]], svytotal,
                                     drop.empty.groups = FALSE, na.rm=TRUE, multicore = TRUE)
}

trabajan.nuble.sector. = 
  do.call(rbind, trabajan.nuble.sector)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(prov_e==84)+sector2, data=info[[i]]$variables)
}
freq. = do.call(cbind, freq)[2,]

trabajan.nuble.sector.$cv = 
  trabajan.nuble.sector.$`se.I(prov_e == 84)TRUE`/trabajan.nuble.sector.$`I(prov_e == 84)TRUE`

trabajan.nuble.sector.$frecuencia = freq.

trabajan.nuble.sector. = 
  trabajan.nuble.sector.[,c("sector2", "I(prov_e == 84)TRUE", "se.I(prov_e == 84)TRUE","cv","frecuencia")]

write.csv(trabajan.nuble.sector., "ocupados_nuble_sector_conmutacion.csv")

# FIN GRAFICO 10:
