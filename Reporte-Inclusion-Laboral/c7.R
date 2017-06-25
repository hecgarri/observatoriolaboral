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
### CUADRO 7: Tasa  de participación, ocupación y desempleo de los jovenes de Nuble segun genero, 2016.
#-------------------------------------------------------------------------------
#################################################################
# tasa de participacion by sexo a nivel nacional: jovenes
#################################################################
#
tasa.part.jovenes.residente.nacional.sexo = svyby(~I((cae_general=="Ocupado" | 
                                                        cae_general=="Desocupado" | 
                                                        cae_general=="Busca Trabajo Primera Vez") & 
                                                       edad>= 15 & edad<= 29), 
                                                  by=~sexo, denominator=~I(pet==1 & edad>= 15 & edad<= 29),
                                                  info2, svyratio, multicore= TRUE, 
                                                  drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I((cae_general=="Ocupado" | 
                  cae_general=="Desocupado" | 
                  cae_general=="Busca Trabajo Primera Vez") & 
                 edad>= 15 & edad<= 29)+sexo, data=info2$variables)[2,]

tasa.part.jovenes.residente.nacional.sexo$cv = 
  tasa.part.jovenes.residente.nacional.sexo$`se.I((cae_general == \"Ocupado\" | cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & edad >= 15 & edad <= 29)/I(pet == 1 & edad >= 15 & edad <= 29)`/tasa.part.jovenes.residente.nacional.sexo$`I((cae_general == \"Ocupado\" | cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & edad >= 15 & edad <= 29)/I(pet == 1 & edad >= 15 & edad <= 29)`

tasa.part.jovenes.residente.nacional.sexo$frecuencia = freq

names(tasa.part.jovenes.residente.nacional.sexo) = 
  c("sexo", "tasa.part.jovenes.residente.nacional.sexo", "error estandar","cv","frecuencia")

write.csv(tasa.part.jovenes.residente.nacional.sexo, "tasa_part_jovenes_residente_nacional_sexo.csv")

#
tasa.promedio.part.jovenes.residente.nacional.sexo = list()
for (i in 1:length(info)){
  tasa.promedio.part.jovenes.residente.nacional.sexo[[i]] = svyratio(~I((cae_general=="Ocupado" | cae_general=="Desocupado" | cae_general=="Busca Trabajo Primera Vez") & edad>= 15 & edad<= 29), denominator=~I(pet==1 & edad>= 15 & edad<= 29),
                                                                     info[[i]], multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.part.jovenes.residente.nacional.sexo. = unlist(lapply(tasa.promedio.part.jovenes.residente.nacional.sexo, '[[', 1) ) 

ee.tasa.promedio.part.jovenes.residente.nacional.sexo = unlist(lapply(tasa.promedio.part.jovenes.residente.nacional.sexo, SE))

tasa.promedio.part.jovenes.residente.nacional.sexo.. = data.frame(tasa.promedio.part.jovenes.residente.nacional.sexo., ee.tasa.promedio.part.jovenes.residente.nacional.sexo)

write.csv(tasa.promedio.part.jovenes.residente.nacional.sexo.., "tasa_promedio_part_jovenes_residente_nacional_sexo.csv")

#################################################################
# tasa de participacion by sexo a nivel de nuble: jovenes
#################################################################
#
tasa.part.jovenes.residente.nuble.sexo = svyby(~I((cae_general=="Ocupado" | 
                                                     cae_general=="Desocupado" | 
                                                     cae_general=="Busca Trabajo Primera Vez") & 
                                                    edad>= 15 & edad<= 29  & prov==84), 
                                               by=~sexo, denominator=~I(pet==1 & edad>= 15 & edad<= 29 & prov==84),
                                               info2, svyratio, multicore= TRUE, 
                                               drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I((cae_general=="Ocupado" | 
                  cae_general=="Desocupado" | 
                  cae_general=="Busca Trabajo Primera Vez") & 
                 edad>= 15 & edad<= 29  & prov==84)+sexo, data=info2$variables)[2,]

tasa.part.jovenes.residente.nuble.sexo$cv = 
  tasa.part.jovenes.residente.nuble.sexo$`se.I((cae_general == \"Ocupado\" | cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & edad >= 15 & edad <= 29 & prov == 84)/I(pet == 1 & edad >= 15 & edad <= 29 & prov == 84)`/tasa.part.jovenes.residente.nuble.sexo$`I((cae_general == \"Ocupado\" | cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & edad >= 15 & edad <= 29 & prov == 84)/I(pet == 1 & edad >= 15 & edad <= 29 & prov == 84)`

tasa.part.jovenes.residente.nuble.sexo$frecuencia = freq

names(tasa.part.jovenes.residente.nuble.sexo) = 
  c("sexo", "tasa.part.jovenes.residente.nuble.sexo", "error estandar","cv","frecuencia")

write.csv(tasa.part.jovenes.residente.nuble.sexo, "tasa_part_jovenes_residente_nuble_sexo.csv")

#
tasa.promedio.part.jovenes.residente.nuble.sexo = list()
for (i in 1:length(info)){
  tasa.promedio.part.jovenes.residente.nuble.sexo[[i]] = svyratio(~I((cae_general=="Ocupado" | cae_general=="Desocupado" | cae_general=="Busca Trabajo Primera Vez") & edad>= 15 & edad<= 29 & prov==84), denominator=~I(pet==1 & edad>= 15 & edad<= 29 & prov==84),
                                                                  info[[i]], multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.part.jovenes.residente.nuble.sexo. = unlist(lapply(tasa.promedio.part.jovenes.residente.nuble.sexo, '[[', 1) ) 

ee.tasa.promedio.part.jovenes.residente.nuble.sexo = unlist(lapply(tasa.promedio.part.jovenes.residente.nuble.sexo, SE))

tasa.promedio.part.jovenes.residente.nuble.sexo.. = data.frame(tasa.promedio.part.jovenes.residente.nuble.sexo., ee.tasa.promedio.part.jovenes.residente.nuble.sexo)

write.csv(tasa.promedio.part.jovenes.residente.nuble.sexo.., "tasa_promedio_part_jovenes_residente_nuble_sexo.csv")

#################################################################
# tasa de ocupacion by sexo a nivel nacional: jovenes
#################################################################
#
tasa.ocup.jovenes.residente.nacional.sexo = svyby(~I(cae_general=="Ocupado" & edad>= 15 & edad<= 29), 
                                                  by=~sexo, denominator=~I(pet==1 & edad>= 15 & edad<= 29),
                                                  info2, svyratio, multicore= TRUE, 
                                                  drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cae_general=="Ocupado" & edad>= 15 & edad<= 29)+sexo, data=info2$variables)[2,]

tasa.ocup.jovenes.residente.nacional.sexo$cv = 
  tasa.ocup.jovenes.residente.nacional.sexo$`se.I(cae_general == \"Ocupado\" & edad >= 15 & edad <= 29)/I(pet == 1 & edad >= 15 & edad <= 29)`/tasa.ocup.jovenes.residente.nacional.sexo$`I(cae_general == \"Ocupado\" & edad >= 15 & edad <= 29)/I(pet == 1 & edad >= 15 & edad <= 29)`

tasa.ocup.jovenes.residente.nacional.sexo$frecuencia = freq

names(tasa.ocup.jovenes.residente.nacional.sexo) = 
  c("sexo", "tasa.ocup.jovenes.residente.nacional.sexo", "error estandar","cv","frecuencia")

write.csv(tasa.ocup.jovenes.residente.nacional.sexo, "tasa_ocup_jovenes_residente_nacional_sexo.csv")

#
tasa.promedio.ocup.jovenes.residente.nacional.sexo = list()
for (i in 1:length(info)){
  tasa.promedio.ocup.jovenes.residente.nacional.sexo[[i]] = svyratio(~I(cae_general=="Ocupado" & edad>= 15 & edad<= 29), denominator=~I(pet==1 & edad>= 15 & edad<= 29),
                                                                     info[[i]], multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.ocup.jovenes.residente.nacional.sexo. = unlist(lapply(tasa.promedio.ocup.jovenes.residente.nacional.sexo, '[[', 1) ) 

ee.tasa.promedio.ocup.jovenes.residente.nacional.sexo = unlist(lapply(tasa.promedio.ocup.jovenes.residente.nacional.sexo, SE))

tasa.promedio.ocup.jovenes.residente.nacional.sexo.. = data.frame(tasa.promedio.ocup.jovenes.residente.nacional.sexo., ee.tasa.promedio.ocup.jovenes.residente.nacional.sexo)

write.csv(tasa.promedio.ocup.jovenes.residente.nacional.sexo.., "tasa_promedio_ocup_jovenes_residente_nacional_sexo.csv")

#################################################################
# tasa de ocupacion by sexo a nivel de nuble: jovenes
#################################################################
#
tasa.ocup.jovenes.residente.nuble.sexo = svyby(~I(cae_general=="Ocupado" & edad>= 15 & 
                                                    edad<= 29 & prov==84), 
                                               by=~sexo, denominator=~I(pet==1 & edad>= 15 & edad<= 29 & prov==84),
                                               info2, svyratio, multicore= TRUE, 
                                               drop.empty.groups = FALSE, na.rm=TRUE)


freq= xtabs(~I(cae_general=="Ocupado" & edad>= 15 & 
                 edad<= 29 & prov==84)+sexo, data=info2$variables)[2,]

tasa.ocup.jovenes.residente.nuble.sexo$cv = 
  tasa.ocup.jovenes.residente.nuble.sexo$`se.I(cae_general == \"Ocupado\" & edad >= 15 & edad <= 29 & prov == 84)/I(pet == 1 & edad >= 15 & edad <= 29 & prov == 84)`/tasa.ocup.jovenes.residente.nuble.sexo$`I(cae_general == \"Ocupado\" & edad >= 15 & edad <= 29 & prov == 84)/I(pet == 1 & edad >= 15 & edad <= 29 & prov == 84)`

tasa.ocup.jovenes.residente.nuble.sexo$frecuencia = freq

names(tasa.ocup.jovenes.residente.nuble.sexo) = 
  c("sexo", "tasa.ocup.jovenes.residente.nuble.sexo", "error estandar","cv","frecuencia")

write.csv(tasa.ocup.jovenes.residente.nuble.sexo, "tasa_ocup_jovenes_residente_nuble_sexo.csv")

#
tasa.promedio.ocup.jovenes.residente.nuble.sexo = list()
for (i in 1:length(info)){
  tasa.promedio.ocup.jovenes.residente.nuble.sexo[[i]] = svyratio(~I(cae_general=="Ocupado" & edad>= 15 & edad<= 29 & prov==84), denominator=~I(pet==1 & edad>= 15 & edad<= 29 & prov==84),
                                                                  info[[i]], multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.ocup.jovenes.residente.nuble.sexo. = unlist(lapply(tasa.promedio.ocup.jovenes.residente.nuble.sexo, '[[', 1) ) 

ee.tasa.promedio.ocup.jovenes.residente.nuble.sexo = unlist(lapply(tasa.promedio.ocup.jovenes.residente.nuble.sexo, SE))

tasa.promedio.ocup.jovenes.residente.nuble.sexo.. = data.frame(tasa.promedio.ocup.jovenes.residente.nuble.sexo., ee.tasa.promedio.ocup.jovenes.residente.nuble.sexo)

write.csv(tasa.promedio.ocup.jovenes.residente.nuble.sexo.., "tasa_promedio_ocup_jovenes_residente_nuble_sexo.csv")

#################################################################
# tasa de desocupacion by sexo a nivel nacional: jovenes
#################################################################
#
tasa.desocup.jovenes.residente.nacional.sexo = svyby(~I((cae_general=="Desocupado" | 
                                                           cae_general=="Busca Trabajo Primera Vez") & 
                                                          edad>= 15 & edad<= 29), 
                                                     by=~sexo, denominator=~I((cae_general=="Ocupado" | 
                                                                                 cae_general=="Desocupado" | 
                                                                                 cae_general=="Busca Trabajo Primera Vez") & 
                                                                                edad>= 15 & edad<= 29),
                                                     info2, svyratio, multicore= TRUE, 
                                                     drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I((cae_general=="Desocupado" | 
                  cae_general=="Busca Trabajo Primera Vez") & 
                 edad>= 15 & edad<= 29)+sexo, data=info2$variables)[2,]

tasa.desocup.jovenes.residente.nacional.sexo$cv = 
  tasa.desocup.jovenes.residente.nacional.sexo$`se.I((cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & edad >= 15 & edad <= 29)/I((cae_general == \"Ocupado\" | cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & edad >= 15 & edad <= 29)`/tasa.desocup.jovenes.residente.nacional.sexo$`I((cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & edad >= 15 & edad <= 29)/I((cae_general == \"Ocupado\" | cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & edad >= 15 & edad <= 29)`

tasa.desocup.jovenes.residente.nacional.sexo$frecuencia = freq

names(tasa.desocup.jovenes.residente.nacional.sexo) = 
  c("sexo", "tasa.desocup.jovenes.residente.nacional.sexo", "error estandar","cv","frecuencia")

write.csv(tasa.desocup.jovenes.residente.nacional.sexo, "tasa_desocup_jovenes_residente_nacional_sexo.csv")

#
tasa.promedio.desocup.jovenes.residente.nacional.sexo = list()
for (i in 1:length(info)){
  tasa.promedio.desocup.jovenes.residente.nacional.sexo[[i]] = svyratio(~I((cae_general=="Desocupado" | cae_general=="Busca Trabajo Primera Vez") & edad>= 15 & edad<= 29), denominator=~I((cae_general=="Ocupado" | cae_general=="Desocupado" | cae_general=="Busca Trabajo Primera Vez") & edad>= 15 & edad<= 29),
                                                                        info[[i]], multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.desocup.jovenes.residente.nacional.sexo. = unlist(lapply(tasa.promedio.desocup.jovenes.residente.nacional.sexo, '[[', 1) ) 

ee.tasa.promedio.desocup.jovenes.residente.nacional.sexo = unlist(lapply(tasa.promedio.desocup.jovenes.residente.nacional.sexo, SE))

tasa.promedio.desocup.jovenes.residente.nacional.sexo.. = data.frame(tasa.promedio.desocup.jovenes.residente.nacional.sexo., ee.tasa.promedio.desocup.jovenes.residente.nacional.sexo)

write.csv(tasa.promedio.desocup.jovenes.residente.nacional.sexo.., "tasa_promedio_desocup_jovenes_residente_nacional_sexo.csv")

#################################################################
# tasa de desocupacion by sexo a nivel de nuble: jovenes
#################################################################
#
tasa.desocup.jovenes.residente.nuble.sexo = svyby(~I((cae_general=="Desocupado" | 
                                                        cae_general=="Busca Trabajo Primera Vez") & 
                                                       edad>= 15 & edad<= 29 & prov==84), 
                                                  by=~sexo, denominator=~I((cae_general=="Ocupado" | 
                                                                              cae_general=="Desocupado" | 
                                                                              cae_general=="Busca Trabajo Primera Vez") & 
                                                                             edad>= 15 & edad<= 29 & prov==84),
                                                  info2, svyratio, multicore= TRUE, 
                                                  drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I((cae_general=="Desocupado" | 
                  cae_general=="Busca Trabajo Primera Vez") & 
                 edad>= 15 & edad<= 29 & prov==84)+sexo, data=info2$variables)[2,]

tasa.desocup.jovenes.residente.nuble.sexo$cv = 
  tasa.desocup.jovenes.residente.nuble.sexo$`se.I((cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & edad >= 15 & edad <= 29 & prov == 84)/I((cae_general == \"Ocupado\" | cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & edad >= 15 & edad <= 29 & prov == 84)`/tasa.desocup.jovenes.residente.nuble.sexo$`I((cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & edad >= 15 & edad <= 29 & prov == 84)/I((cae_general == \"Ocupado\" | cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & edad >= 15 & edad <= 29 & prov == 84)`

tasa.desocup.jovenes.residente.nuble.sexo$frecuencia = freq

names(tasa.desocup.jovenes.residente.nuble.sexo) = 
  c("sexo", "tasa.desocup.jovenes.residente.nuble.sexo", "error estandar","cv","frecuencia")

write.csv(tasa.desocup.jovenes.residente.nuble.sexo, "tasa_desocup_jovenes_residente_nuble_sexo.csv")

#
tasa.promedio.desocup.jovenes.residente.nuble.sexo = list()
for (i in 1:length(info)){
  tasa.promedio.desocup.jovenes.residente.nuble.sexo[[i]] = svyratio(~I((cae_general=="Desocupado" | cae_general=="Busca Trabajo Primera Vez") & edad>= 15 & edad<= 29 & prov==84), denominator=~I((cae_general=="Ocupado" | cae_general=="Desocupado" | cae_general=="Busca Trabajo Primera Vez") & edad>= 15 & edad<= 29 & prov==84),
                                                                     info[[i]], multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.desocup.jovenes.residente.nuble.sexo. = unlist(lapply(tasa.promedio.desocup.jovenes.residente.nuble.sexo, '[[', 1) ) 

ee.tasa.promedio.desocup.jovenes.residente.nuble.sexo = unlist(lapply(tasa.promedio.desocup.jovenes.residente.nuble.sexo, SE))

tasa.promedio.desocup.jovenes.residente.nuble.sexo.. = data.frame(tasa.promedio.desocup.jovenes.residente.nuble.sexo., ee.tasa.promedio.desocup.jovenes.residente.nuble.sexo)

write.csv(tasa.promedio.desocup.jovenes.residente.nuble.sexo.., "tasa_promedio_desocup_jovenes_residente_nuble_sexo.csv")

### FIN CUADRO 7
