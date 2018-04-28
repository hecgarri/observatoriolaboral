#-------------------------------------------------------------------------------
### CUADRO 24: Distribucion de la jornada laboral en nuble
#-------------------------------------------------------------------------------
####################################################################
# tasa de mujeres ocupadas por jornada a nivel nacional
####################################################################
#
tasa.mujeres.jornada.nacional = svyby(~I(cae_general=="Ocupado" & sexo==2),
                                      by=~c1, denominator=~I(cae_general=="Ocupado"), 
                                      info2, svyratio, multicore = TRUE, 
                                      drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cae_general=="Ocupado" & sexo==2)+c1, data=info2$variables)[2,]

tasa.mujeres.jornada.nacional$cv = 
  tasa.mujeres.jornada.nacional$`se.I(cae_general == \"Ocupado\" & sexo == 2)/I(cae_general == \"Ocupado\")`/tasa.mujeres.jornada.nacional$`I(cae_general == \"Ocupado\" & sexo == 2)/I(cae_general == \"Ocupado\")`

tasa.mujeres.jornada.nacional$frecuencia = freq

names(tasa.mujeres.jornada.nacional) = 
  c("Jornada", "tasa.mujeres.jornada.nacional", "error estandar","cv","frecuencia")

write.csv(tasa.mujeres.jornada.nacional, "tasa_mujeres_jornada_nacional.csv")

#
tasa.promedio.mujeres.jornada.nacional = list()
for (i in 1:length(info)){
  tasa.promedio.mujeres.jornada.nacional[[i]] = svyratio(~I(cae_general=="Ocupado" & sexo==2), 
                                                         denominator=~I(cae_general=="Ocupado"), 
                                                         info[[i]], multicore = TRUE, 
                                                         drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.mujeres.jornada.nacional. = 
  do.call(rbind, tasa.promedio.mujeres.jornada.nacional)

names(tasa.promedio.mujeres.jornada.nacional.) = 
  c("tasa.promedio.mujeres.jornada.nacional", "error estandar")

write.csv(tasa.promedio.mujeres.jornada.nacional., "tasa_promedio_mujeres_jornada_nacional.csv")

####################################################################
# tasa de hombres ocupados por jornada a nivel nacional
####################################################################
#
tasa.hombres.jornada.nacional = svyby(~I(cae_general=="Ocupado"  & sexo==1),
                                      by=~c1, denominator=~I(cae_general=="Ocupado"), 
                                      info2, svyratio, multicore = TRUE, 
                                      drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cae_general=="Ocupado"  & sexo==1)+c1, data=info2$variables)[2,]

tasa.hombres.jornada.nacional$cv = 
  tasa.hombres.jornada.nacional$`se.I(cae_general == \"Ocupado\" & sexo == 1)/I(cae_general == \"Ocupado\")`/tasa.hombres.jornada.nacional$`I(cae_general == \"Ocupado\" & sexo == 1)/I(cae_general == \"Ocupado\")`

tasa.hombres.jornada.nacional$frecuencia = freq

names(tasa.hombres.jornada.nacional) = 
  c("Jornada", "tasa.hombres.jornada.nacional", "error estandar","cv","frecuencia")

write.csv(tasa.hombres.jornada.nacional, "tasa_hombres_jornada_nacional.csv")

#
tasa.promedio.hombres.jornada.nacional = list()
for (i in 1:length(info)){
  tasa.promedio.hombres.jornada.nacional[[i]] = svyratio(~I(cae_general=="Ocupado" & sexo==1), 
                                                         denominator=~I(cae_general=="Ocupado"), 
                                                         info[[i]], multicore = TRUE, 
                                                         drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.hombres.jornada.nacional. = 
  do.call(rbind, tasa.promedio.hombres.jornada.nacional)

names(tasa.promedio.hombres.jornada.nacional.) = 
  c("tasa.promedio.hombres.jornada.nacional", "error estandar")

write.csv(tasa.promedio.hombres.jornada.nacional., "tasa_promedio_hombres_jornada_nacional.csv")

####################################################################
# tasa de mujeres ocupadas por jornada a nivel de nuble
####################################################################
#
tasa.mujeres.jornada.nuble = svyby(~I(prov_e==84 & sexo==2),
                                   by=~c1, denominator=~I(prov_e==84), 
                                   info2, svyratio, multicore = TRUE, 
                                   drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(prov_e==84 & sexo==2)+c1, data=info2$variables)[2,]

tasa.mujeres.jornada.nuble$cv = 
  tasa.mujeres.jornada.nuble$`se.I(prov_e == 84 & sexo == 2)/I(prov_e == 84)`/tasa.mujeres.jornada.nuble$`I(prov_e == 84 & sexo == 2)/I(prov_e == 84)`

tasa.mujeres.jornada.nuble$frecuencia = freq

names(tasa.mujeres.jornada.nuble) = 
  c("Jornada", "tasa.mujeres.jornada.nuble", "error estandar","cv","frecuencia")

write.csv(tasa.mujeres.jornada.nuble, "tasa_mujeres_jornada_nuble.csv")

#
tasa.promedio.mujeres.jornada.nuble = list()
for (i in 1:length(info)){
  tasa.promedio.mujeres.jornada.nuble[[i]] = svyratio(~I(prov_e==84 & sexo==2), 
                                                      denominator=~I(prov_e==84), 
                                                      info[[i]], multicore = TRUE, 
                                                      drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.mujeres.jornada.nuble. = 
  do.call(rbind, tasa.promedio.mujeres.jornada.nuble)

names(tasa.promedio.mujeres.jornada.nuble.) = 
  c("tasa.promedio.mujeres.jornada.nuble", "error estandar")

write.csv(tasa.promedio.mujeres.jornada.nuble., "tasa_promedio_mujeres_jornada_nuble.csv")

####################################################################
# tasa de hombres ocupados por jornada a nivel de nuble
####################################################################
#
tasa.hombres.jornada.nuble = svyby(~I(prov_e==84 & sexo==1),
                                   by=~c1, denominator=~(prov_e==84), 
                                   info2, svyratio, multicore = TRUE, 
                                   drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(prov_e==84 & sexo==1)+c1, data=info2$variables)[2,]

tasa.hombres.jornada.nuble$cv = 
  tasa.hombres.jornada.nuble$`se.I(prov_e == 84 & sexo == 1)/prov_e == 84`/tasa.hombres.jornada.nuble$`I(prov_e == 84 & sexo == 1)/prov_e == 84`

tasa.hombres.jornada.nuble$frecuencia = freq

names(tasa.hombres.jornada.nuble) = 
  c("Jornada", "tasa.hombres.jornada.nuble", "error estandar","cv","frecuencia")

write.csv(tasa.hombres.jornada.nuble, "tasa_hombres_jornada_nuble.csv")

#
tasa.promedio.hombres.jornada.nuble = list()
for (i in 1:length(info)){
  tasa.promedio.hombres.jornada.nuble[[i]] = svyratio(~I(prov_e==84 & sexo==1), 
                                                      denominator=~I(prov_e==84), 
                                                      info[[i]], multicore = TRUE, 
                                                      drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.hombres.jornada.nuble. = 
  do.call(rbind, tasa.promedio.hombres.jornada.nuble)

names(tasa.promedio.hombres.jornada.nuble.) = 
  c("tasa.promedio.hombres.jornada.nuble", "error estandar")

write.csv(tasa.promedio.hombres.jornada.nuble., "tasa_promedio_hombres_jornada_nuble.csv")

### FIN CUADRO 24
