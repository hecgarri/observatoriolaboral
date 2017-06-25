# CREACION DE LA VARIABLE: CONMUTANTE A NIVEL PROVINCIAL
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, conmutante2 =
                                 ifelse(prov!=prov_e,1,0))
}

#-------------------------------------------------------------------------------
### CUADRO 23: Nivel educacional trajadores de nuble segun residencia
#-------------------------------------------------------------------------------
###################################################################
# Residen, pero no trabajan en la provincia by nivel educacional
###################################################################
#
conmuta.otra.nuble.educ = list()
for (i in 1:length(info)){
  conmuta.otra.nuble.educ[[i]] = svyby(~I(conmutante2==1 & prov==84),
                                       by=~educ, info[[i]], svytotal,
                                       multicore = TRUE, 
                                       drop.empty.groups = FALSE, na.rm=TRUE)
}

conmuta.otra.nuble.educ. = 
  do.call(rbind, conmuta.otra.nuble.educ)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(conmutante2==1 & prov==84)+educ, data=info[[i]]$variables)
}
freq. = do.call(cbind, freq)[2,]

conmuta.otra.nuble.educ.$cv = 
  conmuta.otra.nuble.educ.$`se.I(conmutante2 == 1 & prov == 84)TRUE`/conmuta.otra.nuble.educ.$`I(conmutante2 == 1 & prov == 84)TRUE`

conmuta.otra.nuble.educ.$frecuencia = freq.

conmuta.otra.nuble.educ. = 
  conmuta.otra.nuble.educ.[, c("educ", "I(conmutante2 == 1 & prov == 84)TRUE", "se.I(conmutante2 == 1 & prov == 84)TRUE", "cv", "frecuencia")]

write.csv(conmuta.otra.nuble.educ., "conmuta_otra_nuble_educ.csv")

###################################################################
# No Residen, pero trabajan en la provincia by nivel educacional
###################################################################
#
conmuta.nuble.educ = list()
for (i in 1:length(info)){
  conmuta.nuble.educ[[i]] = svyby(~I(conmutante2==1 & prov_e==84),
                                  by=~educ, info[[i]], svytotal,
                                  multicore = TRUE, 
                                  drop.empty.groups = FALSE, na.rm=TRUE)
}

conmuta.nuble.educ. = 
  do.call(rbind, conmuta.nuble.educ)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(conmutante2==1 & prov_e==84)+educ, data=info[[i]]$variables)
}
freq. = do.call(cbind, freq)[2,]

conmuta.nuble.educ.$cv = 
  conmuta.nuble.educ.$`se.I(conmutante2 == 1 & prov_e == 84)TRUE`/conmuta.nuble.educ.$`I(conmutante2 == 1 & prov_e == 84)TRUE`

conmuta.nuble.educ.$frecuencia = freq.

conmuta.nuble.educ. = 
  conmuta.nuble.educ.[,c("educ", "I(conmutante2 == 1 & prov_e == 84)TRUE", "se.I(conmutante2 == 1 & prov_e == 84)TRUE", "cv", "frecuencia")]

write.csv(conmuta.nuble.educ., "conmuta_nuble_educ.csv")

###################################################################
# Residen y trabajan en la provincia by nivel educacional
###################################################################
#
residentes.nuble.educ = list()
for (i in 1:length(info)){
  residentes.nuble.educ[[i]] = svyby(~I(prov==84 & prov_e==84),
                                     by=~educ, info[[i]], svytotal,
                                     multicore = TRUE, 
                                     drop.empty.groups = FALSE, na.rm=TRUE)
}

residentes.nuble.educ. = 
  do.call(rbind, residentes.nuble.educ)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(prov==84 & prov_e==84)+educ, data=info[[i]]$variables)
}
freq. = do.call(cbind, freq)[2,]

residentes.nuble.educ.$cv = 
  residentes.nuble.educ.$`se.I(prov == 84 & prov_e == 84)TRUE`/residentes.nuble.educ.$`I(prov == 84 & prov_e == 84)TRUE`

residentes.nuble.educ.$frecuencia = freq.

residentes.nuble.educ. = 
  residentes.nuble.educ.[, c("educ", "I(prov == 84 & prov_e == 84)TRUE", "se.I(prov == 84 & prov_e == 84)TRUE", "cv", "frecuencia")]

write.csv(residentes.nuble.educ., "residentes_nuble_educ.csv")

### FIN CUADRO 23
