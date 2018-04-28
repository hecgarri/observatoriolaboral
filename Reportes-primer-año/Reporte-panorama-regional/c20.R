#-------------------------------------------------------------------------------
### CUADRO 20: Ocupados de nuble segun nivel educacional 2016
#-------------------------------------------------------------------------------
###################################################################
# numero de ocupados a nivel nacional by nivel educacional
###################################################################
ocupados.nacional.educ = list()
for (i in 1:length(info)){
  ocupados.nacional.educ[[i]] = svyby(~I(cae_general=="Ocupado"),
                                      by=~educ3, info[[i]], svytotal,
                                      multicore = TRUE, drop.empty.groups = FALSE, 
                                      na.rm=TRUE)
}

ocupados.nacional.educ. = 
  do.call(rbind, ocupados.nacional.educ)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(cae_general=="Ocupado")+educ3, data=info[[i]]$variables)
}
freq. = do.call(cbind, freq)[2,]

ocupados.nacional.educ.$cv = 
  ocupados.nacional.educ.$`se.I(cae_general == \"Ocupado\")TRUE`/ocupados.nacional.educ.$`I(cae_general == \"Ocupado\")TRUE`

ocupados.nacional.educ.$frecuencia = freq.

ocupados.nacional.educ. = 
  ocupados.nacional.educ.[, c("educ3", "I(cae_general == \"Ocupado\")TRUE" , "se.I(cae_general == \"Ocupado\")TRUE", "cv", "frecuencia")]

write.csv(ocupados.nacional.educ., "ocupados_nacional_educ.csv")

###################################################################
# numero de ocupados a nivel de nuble by nivel educacional
###################################################################
ocupados.nuble.educ = list()
for (i in 1:length(info)){
  ocupados.nuble.educ[[i]] = svyby(~I(prov_e==84),
                                   by=~educ3, info[[i]], svytotal,
                                   multicore = TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

ocupados.nuble.educ. = do.call(rbind, ocupados.nuble.educ)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(prov_e==84)+educ3, data=info[[i]]$variables)
}
freq. = do.call(cbind, freq)[2,]

ocupados.nuble.educ.$cv = 
  ocupados.nuble.educ.$`se.I(prov_e == 84)TRUE`/ocupados.nuble.educ.$`I(prov_e == 84)TRUE`

ocupados.nuble.educ.$frecuencia = freq.

ocupados.nuble.educ. = 
  ocupados.nuble.educ.[,c("educ3", "I(prov_e == 84)TRUE", "se.I(prov_e == 84)TRUE", "cv","frecuencia")]

write.csv(ocupados.nuble.educ., "ocupados_nuble_educ.csv")

### FIN CUADRO 20
