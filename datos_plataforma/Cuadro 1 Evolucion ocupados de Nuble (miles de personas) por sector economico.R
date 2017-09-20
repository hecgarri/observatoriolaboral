#-------------------------------------------------------------------------------
# GRAFICO 10: Evolucion ocupados por sector economico de Nuble, 2010-2016.
#-------------------------------------------------------------------------------
# Residentes por sector en nuble

setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/GitHub/observatoriolaboral/datos_plataforma/")

trabajan_nuble_sector = lapply(1:length(info), function(i) svyby(~I(cae_general=="Ocupado"),
                                     by=~sector2, subset(info[[i]], prov_e==84),
                                     svytotal,drop.empty.groups = FALSE,
                                     na.rm=TRUE, multicore = TRUE)) %>% rbindlist()


freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(prov_e==84)+sector2, data=info[[i]]$variables)
}
freq. = do.call(cbind, freq)[2,]

trabajan_nuble_sector_$cv = 
  trabajan_nuble_sector_$`se.I(prov_e == 84)TRUE`/trabajan_nuble_sector_$`I(prov_e == 84)TRUE`

trabajan_nuble_sector_$frecuencia = freq.

trabajan_nuble_sector_ = 
  trabajan_nuble_sector_[,c("sector2", "I(prov_e == 84)TRUE", "se.I(prov_e == 84)TRUE","cv","frecuencia")]

write.csv(trabajan_nuble_sector_, "ocupados_nuble_sector.csv")


# FIN GRAFICO 10:
