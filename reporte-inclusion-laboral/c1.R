#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLE: POBLACION EN EDAD DE TRABAJAR
for (i in 1:length(todas)){
  info[[i]]$variables = mutate(info[[i]]$variables, pet =
                  ifelse(edad>=15,1, 0)) 
}

# CREACION DE LA VARIABLE: POBLACION EN EDAD DE TRABAJAR
info2$variables = mutate(info2$variables, pet =
                                 ifelse(edad>=15,1, 0))

#-------------------------------------------------------------------------------
### CUADRO 1: Tasas de participación, ocupación y desempleo de Ñuble, según género, 2016. Sólo residentes. 
#-------------------------------------------------------------------------------
#################################################################
# tasa de participacion by sexo a nivel nacional
#################################################################
#
tasa.part.residente.nacional.sexo = svyby(~I(cae_general=="Ocupado" | 
                                               cae_general=="Desocupado" | 
                                               cae_general=="Busca Trabajo Primera Vez"), 
                                          by=~sexo, denominator=~I(pet==1),
                                          info2, svyratio, multicore= TRUE, 
                                          drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cae_general=="Ocupado" | cae_general=="Desocupado" | cae_general=="Busca Trabajo Primera Vez")+sexo, data=info2$variables)[2,]

tasa.part.residente.nacional.sexo$cv = 
  tasa.part.residente.nacional.sexo$`se.I(cae_general == \"Ocupado\" | cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\")/I(pet == 1)`/tasa.part.residente.nacional.sexo$`I(cae_general == \"Ocupado\" | cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\")/I(pet == 1)`

tasa.part.residente.nacional.sexo$frecuencia = freq

names(tasa.part.residente.nacional.sexo) = 
  c("sexo", "tasa.part.residente.nacional.sexo", "error estandar","cv","frecuencia")

write.csv(tasa.part.residente.nacional.sexo, "tasa_part_residente_nacional_sexo.csv")

#
tasa.promedio.part.residente.nacional.sexo = list()
for (i in 1:length(info)){
  tasa.promedio.part.residente.nacional.sexo[[i]] = svyratio(~I(cae_general=="Ocupado" | 
                                                                  cae_general=="Desocupado" | 
                                                                  cae_general=="Busca Trabajo Primera Vez"), 
                                                             denominator=~I(pet==1),
                                                             info[[i]], multicore= TRUE, 
                                                             drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.part.residente.nacional.sexo. = unlist(lapply(tasa.promedio.part.residente.nacional.sexo, '[[', 1) ) 

ee.tasa.promedio.part.residente.nacional.sexo = unlist(lapply(tasa.promedio.part.residente.nacional.sexo, SE))

tasa.promedio.part.residente.nacional.sexo.. = data.frame(tasa.promedio.part.residente.nacional.sexo., ee.tasa.promedio.part.residente.nacional.sexo)

write.csv(tasa.promedio.part.residente.nacional.sexo.., "tasa_promedio_part_residente_nacional_sexo.csv")

#################################################################
# tasa de participacion by sexo a nivel de nuble
#################################################################
#
tasa.part.residente.nuble.sexo = svyby(~I((cae_general=="Ocupado" | 
                                             cae_general=="Desocupado" | 
                                             cae_general=="Busca Trabajo Primera Vez") & 
                                            prov==84), by=~sexo, denominator=~I(pet==1 & prov==84),
                                       info2, svyratio, multicore= TRUE, 
                                       drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I((cae_general=="Ocupado" | cae_general=="Desocupado" | cae_general=="Busca Trabajo Primera Vez") & prov==84)+sexo, data=info2$variables)[2,]

tasa.part.residente.nuble.sexo$cv = 
  tasa.part.residente.nuble.sexo$`se.I((cae_general == \"Ocupado\" | cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & prov == 84)/I(pet == 1 & prov == 84)`/tasa.part.residente.nuble.sexo$`I((cae_general == \"Ocupado\" | cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & prov == 84)/I(pet == 1 & prov == 84)`

tasa.part.residente.nuble.sexo$frecuencia = freq

names(tasa.part.residente.nuble.sexo) = 
  c("sexo", "tasa.part.residente.nuble.sexo", "error estandar","cv","frecuencia")

write.csv(tasa.part.residente.nuble.sexo, "tasa_part_residente_nuble_sexo.csv")

#
tasa.promedio.part.residente.nuble.sexo = list()
for (i in 1:length(info)){
  tasa.promedio.part.residente.nuble.sexo[[i]] = svyratio(~I((cae_general=="Ocupado" | 
                                                                cae_general=="Desocupado" | 
                                                                cae_general=="Busca Trabajo Primera Vez") & 
                                                               prov==84), denominator=~I(pet==1 & prov==84),
                                                          info[[i]], multicore= TRUE, 
                                                          drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.part.residente.nuble.sexo. = unlist(lapply(tasa.promedio.part.residente.nuble.sexo, '[[', 1) ) 

ee.tasa.promedio.part.residente.nuble.sexo = unlist(lapply(tasa.promedio.part.residente.nuble.sexo, SE))

tasa.promedio.part.residente.nuble.sexo.. = data.frame(tasa.promedio.part.residente.nuble.sexo., ee.tasa.promedio.part.residente.nuble.sexo)

write.csv(tasa.promedio.part.residente.nuble.sexo.., "tasa_promedio_part_residente_nuble_sexo.csv")

#################################################################
# tasa de ocupacion by sexo a nivel nacional
#################################################################
#
tasa.ocup.residente.nacional.sexo = svyby(~I(cae_general=="Ocupado"), by=~sexo, 
                                          denominator=~I(pet==1), info2, svyratio, 
                                          multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cae_general=="Ocupado")+sexo, data=info2$variables)[2,]

tasa.ocup.residente.nacional.sexo$cv = 
  tasa.ocup.residente.nacional.sexo$`se.I(cae_general == \"Ocupado\")/I(pet == 1)`/tasa.ocup.residente.nacional.sexo$`I(cae_general == \"Ocupado\")/I(pet == 1)`

tasa.ocup.residente.nacional.sexo$frecuencia = freq

names(tasa.ocup.residente.nacional.sexo) = 
  c("sexo", "tasa.ocup.residente.nacional.sexo", "error estandar","cv","frecuencia")

write.csv(tasa.ocup.residente.nacional.sexo, "tasa_ocup_residente_nacional_sexo.csv")

#
tasa.promedio.ocup.residente.nacional.sexo = list()
for (i in 1:length(info)){
  tasa.promedio.ocup.residente.nacional.sexo[[i]] = svyratio(~I(cae_general=="Ocupado"), 
                                                             denominator=~I(pet==1), info[[i]], 
                                                             multicore= TRUE, drop.empty.groups = FALSE, 
                                                             na.rm=TRUE)
}

tasa.promedio.ocup.residente.nacional.sexo. = unlist(lapply(tasa.promedio.ocup.residente.nacional.sexo, '[[', 1) ) 

ee.tasa.promedio.ocup.residente.nacional.sexo = unlist(lapply(tasa.promedio.ocup.residente.nacional.sexo, SE))

tasa.promedio.ocup.residente.nacional.sexo.. = data.frame(tasa.promedio.ocup.residente.nacional.sexo., ee.tasa.promedio.ocup.residente.nacional.sexo)

write.csv(tasa.promedio.ocup.residente.nacional.sexo.., "tasa_promedio_ocup_residente_nacional_sexo.csv")

#################################################################
# tasa de ocupacion by sexo a nivel de nuble
#################################################################
#
tasa.ocup.residente.nuble.sexo = svyby(~I(cae_general=="Ocupado" & prov==84), 
                                       by=~sexo, denominator=~I(pet==1 & prov==84),
                                       info2, svyratio, multicore= TRUE, 
                                       drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cae_general=="Ocupado" & prov==84)+sexo, data=info2$variables)[2,]

tasa.ocup.residente.nuble.sexo$cv = 
  tasa.ocup.residente.nuble.sexo$`se.I(cae_general == \"Ocupado\" & prov == 84)/I(pet == 1 & prov == 84)`/tasa.ocup.residente.nuble.sexo$`I(cae_general == \"Ocupado\" & prov == 84)/I(pet == 1 & prov == 84)`

tasa.ocup.residente.nuble.sexo$frecuencia = freq

names(tasa.ocup.residente.nuble.sexo) = 
  c("sexo", "tasa.ocup.residente.nuble.sexo", "error estandar","cv","frecuencia")

write.csv(tasa.ocup.residente.nuble.sexo, "tasa_ocup_residente_nuble_sexo.csv")

#
tasa.promedio.ocup.residente.nuble.sexo = list()
for (i in 1:length(info)){
  tasa.promedio.ocup.residente.nuble.sexo[[i]] = svyratio(~I(cae_general=="Ocupado" & prov==84), 
                                                          denominator=~I(pet==1 & prov==84),
                                                          info[[i]], multicore= TRUE, 
                                                          drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.ocup.residente.nuble.sexo. = unlist(lapply(tasa.promedio.ocup.residente.nuble.sexo, '[[', 1) ) 

ee.tasa.promedio.ocup.residente.nuble.sexo = unlist(lapply(tasa.promedio.ocup.residente.nuble.sexo, SE))

tasa.promedio.ocup.residente.nuble.sexo.. = data.frame(tasa.promedio.ocup.residente.nuble.sexo., ee.tasa.promedio.ocup.residente.nuble.sexo)

write.csv(tasa.promedio.ocup.residente.nuble.sexo.., "tasa_promedio_ocup_residente_nuble_sexo.csv")

#################################################################
# tasa de desocupacion by sexo a nivel nacional
#################################################################
#
tasa.desocup.residente.nacional.sexo = svyby(~I(cae_general=="Desocupado" | 
                                                  cae_general=="Busca Trabajo Primera Vez"), 
                                             by=~sexo, denominator=~I(cae_general=="Ocupado" | 
                                                                        cae_general=="Desocupado" | 
                                                                        cae_general=="Busca Trabajo Primera Vez"),
                                             info2, svyratio, multicore= TRUE, 
                                             drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cae_general=="Desocupado" | cae_general=="Busca Trabajo Primera Vez")+sexo, data=info2$variables)[2,]

tasa.desocup.residente.nacional.sexo$cv = 
  tasa.desocup.residente.nacional.sexo$`se.I(cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\")/I(cae_general == \"Ocupado\" | cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\")`/tasa.desocup.residente.nacional.sexo$`I(cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\")/I(cae_general == \"Ocupado\" | cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\")`

tasa.desocup.residente.nacional.sexo$frecuencia = freq

names(tasa.desocup.residente.nacional.sexo) = 
  c("sexo", "tasa.desocup.residente.nacional.sexo", "error estandar", "cv", "frecuencia")

write.csv(tasa.desocup.residente.nacional.sexo, "tasa_desocup_residente_nacional_sexo.csv")

#
tasa.promedio.desocup.residente.nacional.sexo = list()
for (i in 1:length(info)){
  tasa.promedio.desocup.residente.nacional.sexo[[i]] = svyratio(~I(cae_general=="Desocupado" | 
                                                                     cae_general=="Busca Trabajo Primera Vez"), 
                                                                denominator=~I(cae_general=="Ocupado" | 
                                                                                 cae_general=="Desocupado" | 
                                                                                 cae_general=="Busca Trabajo Primera Vez"),
                                                                info[[i]], multicore= TRUE, 
                                                                drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.desocup.residente.nacional.sexo. = unlist(lapply(tasa.promedio.desocup.residente.nacional.sexo, '[[', 1) ) 

ee.tasa.promedio.desocup.residente.nacional.sexo = unlist(lapply(tasa.promedio.desocup.residente.nacional.sexo, SE))

tasa.promedio.desocup.residente.nacional.sexo.. = data.frame(tasa.promedio.desocup.residente.nacional.sexo., ee.tasa.promedio.desocup.residente.nacional.sexo)

write.csv(tasa.promedio.desocup.residente.nacional.sexo.., "tasa_promedio_desocup_residente_nacional_sexo.csv")

#################################################################
# tasa de desocupacion by sexo a nivel de nuble
#################################################################
#
tasa.desocup.residente.nuble.sexo = svyby(~I((cae_general=="Desocupado" | 
                                                cae_general=="Busca Trabajo Primera Vez") & 
                                               prov==84), by=~sexo, 
                                          denominator=~I((cae_general=="Ocupado" | 
                                                            cae_general=="Desocupado" | 
                                                            cae_general=="Busca Trabajo Primera Vez") & 
                                                           prov==84),
                                          info2, svyratio, multicore= TRUE, 
                                          drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I((cae_general=="Desocupado" | cae_general=="Busca Trabajo Primera Vez") & prov==84)+sexo, data=info2$variables)[2,]

tasa.desocup.residente.nuble.sexo$cv = 
  tasa.desocup.residente.nuble.sexo$`se.I((cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & prov == 84)/I((cae_general == \"Ocupado\" | cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & prov == 84)`/tasa.desocup.residente.nuble.sexo$`I((cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & prov == 84)/I((cae_general == \"Ocupado\" | cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & prov == 84)`

tasa.desocup.residente.nuble.sexo$frecuencia = freq

names(tasa.desocup.residente.nuble.sexo) = 
  c("sexo", "tasa.desocup.residente.nuble.sexo", "error estandar","cv","frecuencia")

write.csv(tasa.desocup.residente.nuble.sexo, "tasa_desocup_residente_nuble_sexo.csv")

#
tasa.promedio.desocup.residente.nuble.sexo = list()
for (i in 1:length(info)){
  tasa.promedio.desocup.residente.nuble.sexo[[i]] = svyratio(~I((cae_general=="Desocupado" | 
                                                                   cae_general=="Busca Trabajo Primera Vez") & 
                                                                  prov==84), 
                                                             denominator=~I((cae_general=="Ocupado" | 
                                                                               cae_general=="Desocupado" | 
                                                                               cae_general=="Busca Trabajo Primera Vez") & 
                                                                              prov==84),
                                                             info[[i]], multicore= TRUE, 
                                                             drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.desocup.residente.nuble.sexo. = unlist(lapply(tasa.promedio.desocup.residente.nuble.sexo, '[[', 1) ) 

ee.tasa.promedio.desocup.residente.nuble.sexo = unlist(lapply(tasa.promedio.desocup.residente.nuble.sexo, SE))

tasa.promedio.desocup.residente.nuble.sexo.. = data.frame(tasa.promedio.desocup.residente.nuble.sexo., ee.tasa.promedio.desocup.residente.nuble.sexo)

write.csv(tasa.promedio.desocup.residente.nuble.sexo.., "tasa_promedio_desocup_residente_nuble_sexo.csv")

### FIN CUADRO 1
