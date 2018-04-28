#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLE: TRAMOS DE EDAD EN JOVENES
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, tramos.jovenes = 
                            ifelse(edad>=15 & edad<=19,1,
                            ifelse(edad>=20 & edad<=24,2,
                            ifelse(edad>=25 & edad<=29,3,NA))))
}

#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLE: TRAMOS DE EDAD EN JOVENES
info2$variables = mutate(info2$variables, tramos.jovenes = 
                            ifelse(edad>=15 & edad<=19,1,
                            ifelse(edad>=20 & edad<=24,2,
                            ifelse(edad>=25 & edad<=29,3,NA))))

#-------------------------------------------------------------------------------
### CUADRO 41: Tasa de participacion, ocupación y desempleo de los jovenes de Nuble, segun edad, 2016.
#-------------------------------------------------------------------------------
#################################################################
# tasa de participacion by sexo a nivel nacional: tramos jovenes
#################################################################
#
tasa.part.t.jovenes.residente.nacional.sexo = svyby(~I((cae_general=="Ocupado" | 
                                                          cae_general=="Desocupado" | 
                                                          cae_general=="Busca Trabajo Primera Vez") & 
                                                         edad>= 15 & edad<= 29), 
                                                    by=~tramos.jovenes, denominator=~I(pet==1 & edad>= 15 & edad<= 29),
                                                    info2, svyratio, multicore= TRUE, 
                                                    drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I((cae_general=="Ocupado" | 
                  cae_general=="Desocupado" | 
                  cae_general=="Busca Trabajo Primera Vez") & 
                 edad>= 15 & edad<= 29)+tramos.jovenes, data=info2$variables)[2,]

tasa.part.t.jovenes.residente.nacional.sexo$cv = 
  tasa.part.t.jovenes.residente.nacional.sexo$`se.I((cae_general == \"Ocupado\" | cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & edad >= 15 & edad <= 29)/I(pet == 1 & edad >= 15 & edad <= 29)`/tasa.part.t.jovenes.residente.nacional.sexo$`I((cae_general == \"Ocupado\" | cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & edad >= 15 & edad <= 29)/I(pet == 1 & edad >= 15 & edad <= 29)`

tasa.part.t.jovenes.residente.nacional.sexo$frecuencia = freq

names(tasa.part.t.jovenes.residente.nacional.sexo) = 
  c("tramos", "tasa.part.t.jovenes.residente.nacional.sexo", "error estandar","cv","frecuencia")

write.csv(tasa.part.t.jovenes.residente.nacional.sexo, "tasa_part_t_jovenes_residente_nacional_tramos.csv")

#
tasa.promedio.part.t.jovenes.residente.nacional.sexo = list()
for (i in 1:length(info)){
  tasa.promedio.part.t.jovenes.residente.nacional.sexo[[i]] = svyratio(~I((cae_general=="Ocupado" | 
                                                                             cae_general=="Desocupado" | 
                                                                             cae_general=="Busca Trabajo Primera Vez") & 
                                                                            edad>= 15 & edad<= 29), 
                                                                       denominator=~I(pet==1 & edad>= 15 & edad<= 29),
                                                                       info[[i]], multicore= TRUE, 
                                                                       drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.part.t.jovenes.residente.nacional.sexo. = unlist(lapply(tasa.promedio.part.t.jovenes.residente.nacional.sexo, '[[', 1) ) 

ee.tasa.promedio.part.t.jovenes.residente.nacional.sexo = unlist(lapply(tasa.promedio.part.t.jovenes.residente.nacional.sexo, SE))

tasa.promedio.part.t.jovenes.residente.nacional.sexo.. = data.frame(tasa.promedio.part.t.jovenes.residente.nacional.sexo., ee.tasa.promedio.part.t.jovenes.residente.nacional.sexo)

write.csv(tasa.promedio.part.t.jovenes.residente.nacional.sexo.., "tasa_promedio_part_t_jovenes_residente_nacional_tramos.csv")

#################################################################
# tasa de participacion by sexo a nivel de nuble: tramos jovenes
#################################################################
#
tasa.part.t.jovenes.residente.nuble.sexo = svyby(~I((cae_general=="Ocupado" | 
                                                       cae_general=="Desocupado" | 
                                                       cae_general=="Busca Trabajo Primera Vez") & 
                                                      edad>= 15 & edad<= 29  & prov==84), 
                                                 by=~tramos.jovenes, denominator=~I(pet==1 & edad>= 15 & edad<= 29 & prov==84),
                                                 info2, svyratio, multicore= TRUE, 
                                                 drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I((cae_general=="Ocupado" | 
                  cae_general=="Desocupado" | 
                  cae_general=="Busca Trabajo Primera Vez") & 
                 edad>= 15 & edad<= 29  & prov==84)+tramos.jovenes, data=info2$variables)[2,]

tasa.part.t.jovenes.residente.nuble.sexo$cv = 
  tasa.part.t.jovenes.residente.nuble.sexo$`se.I((cae_general == \"Ocupado\" | cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & edad >= 15 & edad <= 29 & prov == 84)/I(pet == 1 & edad >= 15 & edad <= 29 & prov == 84)`/tasa.part.t.jovenes.residente.nuble.sexo$`I((cae_general == \"Ocupado\" | cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & edad >= 15 & edad <= 29 & prov == 84)/I(pet == 1 & edad >= 15 & edad <= 29 & prov == 84)`

tasa.part.t.jovenes.residente.nuble.sexo$frecuencia = freq

names(tasa.part.t.jovenes.residente.nuble.sexo) = 
  c("tramos", "tasa.part.t.jovenes.residente.nuble.sexo", "error estandar","cv","frecuencia")

write.csv(tasa.part.t.jovenes.residente.nuble.sexo, "tasa_part_t_jovenes_residente_nuble_tramos.csv")

#
tasa.promedio.part.t.jovenes.residente.nuble.sexo = list()
for (i in 1:length(info)){
  tasa.promedio.part.t.jovenes.residente.nuble.sexo[[i]] = svyratio(~I((cae_general=="Ocupado" | 
                                                                          cae_general=="Desocupado" | 
                                                                          cae_general=="Busca Trabajo Primera Vez") & 
                                                                         edad>= 15 & edad<= 29 & prov==84), 
                                                                    denominator=~I(pet==1 & edad>= 15 & edad<= 29 & prov==84),
                                                                    info[[i]], multicore= TRUE, 
                                                                    drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.part.t.jovenes.residente.nuble.sexo. = unlist(lapply(tasa.promedio.part.t.jovenes.residente.nuble.sexo, '[[', 1) ) 

ee.tasa.promedio.part.t.jovenes.residente.nuble.sexo = unlist(lapply(tasa.promedio.part.t.jovenes.residente.nuble.sexo, SE))

tasa.promedio.part.t.jovenes.residente.nuble.sexo.. = data.frame(tasa.promedio.part.t.jovenes.residente.nuble.sexo., ee.tasa.promedio.part.t.jovenes.residente.nuble.sexo)

write.csv(tasa.promedio.part.t.jovenes.residente.nuble.sexo.., "tasa_promedio_part_t_jovenes_residente_nuble_tramos.csv")

#################################################################
# tasa de ocupacion by sexo a nivel nacional: tramos jovenes
#################################################################
#
tasa.ocup.t.jovenes.residente.nacional.sexo = svyby(~I(cae_general=="Ocupado" & 
                                                         edad>= 15 & edad<= 29), 
                                                    by=~tramos.jovenes, 
                                                    denominator=~I(pet==1 & edad>= 15 & edad<= 29),
                                                    info2, svyratio, multicore= TRUE, 
                                                    drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cae_general=="Ocupado" & 
                 edad>= 15 & edad<= 29)+tramos.jovenes, data=info2$variables)[2,]

tasa.ocup.t.jovenes.residente.nacional.sexo$cv = 
  tasa.ocup.t.jovenes.residente.nacional.sexo$`se.I(cae_general == \"Ocupado\" & edad >= 15 & edad <= 29)/I(pet == 1 & edad >= 15 & edad <= 29)`/tasa.ocup.t.jovenes.residente.nacional.sexo$`I(cae_general == \"Ocupado\" & edad >= 15 & edad <= 29)/I(pet == 1 & edad >= 15 & edad <= 29)`

tasa.ocup.t.jovenes.residente.nacional.sexo$frecuencia = freq

names(tasa.ocup.t.jovenes.residente.nacional.sexo) = 
  c("tramos", "tasa.ocup.t.jovenes.residente.nacional.sexo", "error estandar","cv","frecuencia")

write.csv(tasa.ocup.t.jovenes.residente.nacional.sexo, "tasa_ocup_t_jovenes_residente_nacional_tramos.csv")

#
tasa.promedio.ocup.t.jovenes.residente.nacional.sexo = list()
for (i in 1:length(info)){
  tasa.promedio.ocup.t.jovenes.residente.nacional.sexo[[i]] = svyratio(~I(cae_general=="Ocupado" & edad>= 15 & edad<= 29), 
                                                                       denominator=~I(pet==1 & edad>= 15 & edad<= 29),
                                                                       info[[i]], multicore= TRUE, 
                                                                       drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.ocup.t.jovenes.residente.nacional.sexo. = unlist(lapply(tasa.promedio.ocup.t.jovenes.residente.nacional.sexo, '[[', 1) ) 

ee.tasa.promedio.ocup.t.jovenes.residente.nacional.sexo = unlist(lapply(tasa.promedio.ocup.t.jovenes.residente.nacional.sexo, SE))

tasa.promedio.ocup.t.jovenes.residente.nacional.sexo.. = data.frame(tasa.promedio.ocup.t.jovenes.residente.nacional.sexo., ee.tasa.promedio.ocup.t.jovenes.residente.nacional.sexo)

write.csv(tasa.promedio.ocup.t.jovenes.residente.nacional.sexo.., "tasa_promedio_ocup_t_jovenes_residente_nacional_tramos.csv")

#################################################################
# tasa de ocupacion by sexo a nivel de nuble: tramos jovenes
#################################################################
#
tasa.ocup.t.jovenes.residente.nuble.sexo = svyby(~I(cae_general=="Ocupado" & 
                                                      edad>= 15 & edad<= 29 & prov==84), 
                                                 by=~tramos.jovenes, 
                                                 denominator=~I(pet==1 & edad>= 15 & edad<= 29 & prov==84),
                                                 info2, svyratio, multicore= TRUE, 
                                                 drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cae_general=="Ocupado" & 
                 edad>= 15 & edad<= 29 & prov==84)+tramos.jovenes, data=info2$variables)[2,]

tasa.ocup.t.jovenes.residente.nuble.sexo$cv = 
  tasa.ocup.t.jovenes.residente.nuble.sexo$`se.I(cae_general == \"Ocupado\" & edad >= 15 & edad <= 29 & prov == 84)/I(pet == 1 & edad >= 15 & edad <= 29 & prov == 84)`/tasa.ocup.t.jovenes.residente.nuble.sexo$`I(cae_general == \"Ocupado\" & edad >= 15 & edad <= 29 & prov == 84)/I(pet == 1 & edad >= 15 & edad <= 29 & prov == 84)`

tasa.ocup.t.jovenes.residente.nuble.sexo$frecuencia = freq

names(tasa.ocup.t.jovenes.residente.nuble.sexo) = 
  c("tramos", "tasa.ocup.t.jovenes.residente.nuble.sexo", "error estandar","cv","frecuencia")

write.csv(tasa.ocup.t.jovenes.residente.nuble.sexo, "tasa_ocup_t_jovenes_residente_nuble_tramos.csv")

#
tasa.promedio.ocup.t.jovenes.residente.nuble.sexo = list()
for (i in 1:length(info)){
  tasa.promedio.ocup.t.jovenes.residente.nuble.sexo[[i]] = svyratio(~I(cae_general=="Ocupado" & edad>= 15 & edad<= 29 & prov==84), 
                                                                    denominator=~I(pet==1 & edad>= 15 & edad<= 29 & prov==84),
                                                                    info[[i]], multicore= TRUE, 
                                                                    drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.ocup.t.jovenes.residente.nuble.sexo. = unlist(lapply(tasa.promedio.ocup.t.jovenes.residente.nuble.sexo, '[[', 1) ) 

ee.tasa.promedio.ocup.t.jovenes.residente.nuble.sexo = unlist(lapply(tasa.promedio.ocup.t.jovenes.residente.nuble.sexo, SE))

tasa.promedio.ocup.t.jovenes.residente.nuble.sexo.. = data.frame(tasa.promedio.ocup.t.jovenes.residente.nuble.sexo., ee.tasa.promedio.ocup.t.jovenes.residente.nuble.sexo)

write.csv(tasa.promedio.ocup.t.jovenes.residente.nuble.sexo.., "tasa_promedio_ocup_t_jovenes_residente_nuble_tramos.csv")

#################################################################
# tasa de desocupacion by sexo a nivel nacional: tramos jovenes
#################################################################
#
tasa.desocup.t.jovenes.residente.nacional.sexo = svyby(~I((cae_general=="Desocupado" | 
                                                             cae_general=="Busca Trabajo Primera Vez") & 
                                                            edad>= 15 & edad<= 29), 
                                                       by=~tramos.jovenes, 
                                                       denominator=~I((cae_general=="Ocupado" | 
                                                                         cae_general=="Desocupado" | 
                                                                         cae_general=="Busca Trabajo Primera Vez") & 
                                                                        edad>= 15 & edad<= 29),
                                                       info2, svyratio, multicore= TRUE, 
                                                       drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I((cae_general=="Desocupado" | 
                  cae_general=="Busca Trabajo Primera Vez") & 
                 edad>= 15 & edad<= 29)+tramos.jovenes, data=info2$variables)[2,]

tasa.desocup.t.jovenes.residente.nacional.sexo$cv = 
  tasa.desocup.t.jovenes.residente.nacional.sexo$`se.I((cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & edad >= 15 & edad <= 29)/I((cae_general == \"Ocupado\" | cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & edad >= 15 & edad <= 29)`/tasa.desocup.t.jovenes.residente.nacional.sexo$`I((cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & edad >= 15 & edad <= 29)/I((cae_general == \"Ocupado\" | cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & edad >= 15 & edad <= 29)`

tasa.desocup.t.jovenes.residente.nacional.sexo$frecuencia = freq

names(tasa.desocup.t.jovenes.residente.nacional.sexo) = 
  c("tramos", "tasa.desocup.t.jovenes.residente.nacional.sexo", "error estandar","cv","frecuencia")

write.csv(tasa.desocup.t.jovenes.residente.nacional.sexo, "tasa_desocup_t_jovenes_residente_nacional_tramos.csv")

#
tasa.promedio.desocup.t.jovenes.residente.nacional.sexo = list()
for (i in 1:length(info)){
  tasa.promedio.desocup.t.jovenes.residente.nacional.sexo[[i]] = svyratio(~I((cae_general=="Desocupado" | 
                                                                                cae_general=="Busca Trabajo Primera Vez") & 
                                                                               edad>= 15 & edad<= 29), denominator=~I((cae_general=="Ocupado" | cae_general=="Desocupado" | cae_general=="Busca Trabajo Primera Vez") & edad>= 15 & edad<= 29),
                                                                          info[[i]], multicore= TRUE, 
                                                                          drop.empty.groups = FALSE, na.rm=TRUE)
  
}

tasa.promedio.desocup.t.jovenes.residente.nacional.sexo. = unlist(lapply(tasa.promedio.desocup.t.jovenes.residente.nacional.sexo, '[[', 1) ) 

ee.tasa.promedio.desocup.t.jovenes.residente.nacional.sexo = unlist(lapply(tasa.promedio.desocup.t.jovenes.residente.nacional.sexo, SE))

tasa.promedio.desocup.t.jovenes.residente.nacional.sexo.. = data.frame(tasa.promedio.desocup.t.jovenes.residente.nacional.sexo., ee.tasa.promedio.desocup.t.jovenes.residente.nacional.sexo)

write.csv(tasa.promedio.desocup.t.jovenes.residente.nacional.sexo.., "tasa_promedio_desocup_t_jovenes_residente_nacional_tramos.csv")

#################################################################
# tasa de desocupacion by sexo a nivel de nuble: tramos jovenes
#################################################################
#
tasa.desocup.t.jovenes.residente.nuble.sexo = svyby(~I((cae_general=="Desocupado" | 
                                                          cae_general=="Busca Trabajo Primera Vez") & 
                                                         edad>= 15 & edad<= 29 & prov==84), 
                                                    by=~tramos.jovenes, 
                                                    denominator=~I((cae_general=="Ocupado" | 
                                                                      cae_general=="Desocupado" | 
                                                                      cae_general=="Busca Trabajo Primera Vez") & 
                                                                     edad>= 15 & edad<= 29 & prov==84),
                                                    info2, svyratio, multicore= TRUE, 
                                                    drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I((cae_general=="Desocupado" | 
                  cae_general=="Busca Trabajo Primera Vez") & 
                 edad>= 15 & edad<= 29 & prov==84)+tramos.jovenes, data=info2$variables)[2,]

tasa.desocup.t.jovenes.residente.nuble.sexo$cv = 
  tasa.desocup.t.jovenes.residente.nuble.sexo$`se.I((cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & edad >= 15 & edad <= 29 & prov == 84)/I((cae_general == \"Ocupado\" | cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & edad >= 15 & edad <= 29 & prov == 84)`/tasa.desocup.t.jovenes.residente.nuble.sexo$`I((cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & edad >= 15 & edad <= 29 & prov == 84)/I((cae_general == \"Ocupado\" | cae_general == \"Desocupado\" | cae_general == \"Busca Trabajo Primera Vez\") & edad >= 15 & edad <= 29 & prov == 84)`

tasa.desocup.t.jovenes.residente.nuble.sexo$frecuencia = freq

names(tasa.desocup.t.jovenes.residente.nuble.sexo) = 
  c("tramos", "tasa.desocup.t.jovenes.residente.nuble.sexo", "error estandar","cv","frecuencia")

write.csv(tasa.desocup.t.jovenes.residente.nuble.sexo, "tasa_desocup_t_jovenes_residente_nuble_tramos.csv")

#
tasa.promedio.desocup.t.jovenes.residente.nuble.sexo = list()
for (i in 1:length(info)){
  tasa.promedio.desocup.t.jovenes.residente.nuble.sexo[[i]] = svyratio(~I((cae_general=="Desocupado" | 
                                                                             cae_general=="Busca Trabajo Primera Vez") & 
                                                                            edad>= 15 & edad<= 29 & prov==84), 
                                                                       denominator=~I((cae_general=="Ocupado" | 
                                                                                         cae_general=="Desocupado" | 
                                                                                         cae_general=="Busca Trabajo Primera Vez") & 
                                                                                        edad>= 15 & edad<= 29 & prov==84),
                                                                       info[[i]], multicore= TRUE, 
                                                                       drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.desocup.t.jovenes.residente.nuble.sexo. = unlist(lapply(tasa.promedio.desocup.t.jovenes.residente.nuble.sexo, '[[', 1) ) 

ee.tasa.promedio.desocup.t.jovenes.residente.nuble.sexo = unlist(lapply(tasa.promedio.desocup.t.jovenes.residente.nuble.sexo, SE))

tasa.promedio.desocup.t.jovenes.residente.nuble.sexo.. = data.frame(tasa.promedio.desocup.t.jovenes.residente.nuble.sexo., ee.tasa.promedio.desocup.t.jovenes.residente.nuble.sexo)

write.csv(tasa.promedio.desocup.t.jovenes.residente.nuble.sexo.., "tasa_promedio_desocup_t_jovenes_residente_nuble_tramos.csv")

### FIN CUADRO 41
