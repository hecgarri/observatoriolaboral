# CREACION DE LA VARIABLE: CATEGORIA OCUPACIONAL 
# (1:Empleador; 2:Cuenta Propia; 3:Trabajador dependiente)
info2$variables$cat.ocup = recode(info2$variables$categoria_ocupacion, `1`= 1, 
                               `2`= 2, `3` =3, `4`=3, `5`=3, `6`=3,
                               `7`=4, .default = 0, .missing = 99)

info2$variables = mutate(info2$variables, cat.ocup = ifelse(cat.ocup==3 & b8==2,3,
                                      ifelse(cat.ocup==3 & b8==1 & b9==1,4, 
                                      ifelse(cat.ocup==3 & b8==1 & b9==2,5, 
                                      ifelse(cat.ocup==1,1, 
                                      ifelse(cat.ocup==2,2, 
                                      ifelse(cat.ocup==4,6,NA)))))))

info2$variables$cat.ocup = factor(info2$variables$cat.ocup, 
                               levels=c(1,2,3,4,5,6),
                               labels=c("empleador","cuenta propia","asalariado sin contrato", 
                                        "asalariado con contrato definido",
                                        "asalariado con contrato indefinido",
                                        "no remunerado"))

# CREACION DE LA VARIABLE: CATEGORIA OCUPACIONAL 
# (1:Empleador; 2:Cuenta Propia; 3:Trabajador dependiente)
for (i in 1:length(info)){
  info[[i]]$variables$cat.ocup = recode(info[[i]]$variables$categoria_ocupacion, `1`= 1, 
                               `2`= 2, `3` =3, `4`=3, `5`=3, `6`=3,
                               `7`=4, .default = 0, .missing = 99)
}

for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, cat.ocup = ifelse(cat.ocup==3 & b8==2,3,
                                        ifelse(cat.ocup==3 & b8==1 & b9==1,4, 
                                        ifelse(cat.ocup==3 & b8==1 & b9==2,5, 
                                        ifelse(cat.ocup==1,1, 
                                        ifelse(cat.ocup==2,2, 
                                        ifelse(cat.ocup==4,6,NA)))))))
}

for (i in 1:length(info)){
  info[[i]]$variables$cat.ocup = factor(info[[i]]$variables$cat.ocup, 
                               levels=c(1,2,3,4,5,6),
                               labels=c("empleador","cuenta propia","asalariado sin contrato", 
                                        "asalariado con contrato definido",
                                        "asalariado con contrato indefinido",
                                        "no remunerado"))
}

#-------------------------------------------------------------------------------
### CUADRO 11: Categoria ocupacional de los ocupados de nuble segun sector
#-------------------------------------------------------------------------------
################################################################
# tasa de empleador del total de ocupados en nuble
################################################################
#
tasa.empleador.nuble.sector = svyby(~I(categoria_ocupacion==1 & cae_general=="Ocupado" & prov_e==84),
                                    by=~sector2, denominator=~I(cae_general=="Ocupado" & prov_e==84), 
                                    info2, svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(categoria_ocupacion==1 & cae_general=="Ocupado" & prov_e==84)+sector2, data=info2$variables)[2,]

tasa.empleador.nuble.sector$cv = 
  tasa.empleador.nuble.sector$`se.I(categoria_ocupacion == 1 & cae_general == \"Ocupado\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`/tasa.empleador.nuble.sector$`I(categoria_ocupacion == 1 & cae_general == \"Ocupado\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`

tasa.empleador.nuble.sector$frecuencia = freq

names(tasa.empleador.nuble.sector) = 
  c("secto2", "tasa.empleador.nuble.sector", "error estandar", "cv", "frecuencia")

write.csv(tasa.empleador.nuble.sector, "tasa_empleador_nuble_sector.csv")

#
tasa.promedio.empleador.nuble = list()
for (i in 1:length(info)){
  tasa.promedio.empleador.nuble[[i]] = svyratio(~I(categoria_ocupacion==1 & cae_general=="Ocupado" & prov_e==84),
                                                denominator=~I(cae_general=="Ocupado" & prov_e==84), info[[i]],
                                                multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.empleador.nuble. = unlist(lapply(tasa.promedio.empleador.nuble, '[[', 1) ) 

ee.tasa.promedio.empleador.nuble = unlist(lapply(tasa.promedio.empleador.nuble, SE))

tasa.promedio.empleador.nuble.. = data.frame(tasa.promedio.empleador.nuble., ee.tasa.promedio.empleador.nuble)

write.csv(tasa.promedio.empleador.nuble.., "tasa_promedio_empleador_nuble.csv")

################################################################
# tasa de cuenta propia del total de ocupados en nuble
################################################################
#
tasa.cuentap.nuble.sector= svyby(~I(categoria_ocupacion==2 & cae_general=="Ocupado" & prov_e==84),
                                 by=~sector2, denominator=~I(cae_general=="Ocupado" & prov_e==84), 
                                 info2, svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(categoria_ocupacion==2 & cae_general=="Ocupado" & prov_e==84)+sector2, data=info2$variables)[2,]

tasa.cuentap.nuble.sector$cv = 
  tasa.cuentap.nuble.sector$`se.I(categoria_ocupacion == 2 & cae_general == \"Ocupado\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`/tasa.cuentap.nuble.sector$`I(categoria_ocupacion == 2 & cae_general == \"Ocupado\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`

tasa.cuentap.nuble.sector$frecuencia = freq

names(tasa.cuentap.nuble.sector) = 
  c("sector2", "tasa.cuentap.nuble.sector", "error estandar", "cv", "frecuencia")

write.csv(tasa.cuentap.nuble.sector, "tasa_cuentap_nuble_sector.csv")

#
tasa.promedio.cuentap.nuble = list()
for (i in 1:length(info)){
  tasa.promedio.cuentap.nuble[[i]] = svyratio(~I(categoria_ocupacion==2 & cae_general=="Ocupado" & prov_e==84),
                                              denominator=~I(cae_general=="Ocupado" & prov_e==84), info[[i]],
                                              multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.cuentap.nuble. = unlist(lapply(tasa.promedio.cuentap.nuble, '[[', 1) ) 

ee.tasa.promedio.cuentap.nuble = unlist(lapply(tasa.promedio.cuentap.nuble, SE))

tasa.promedio.cuentap.nuble.. = data.frame(tasa.promedio.cuentap.nuble., ee.tasa.promedio.cuentap.nuble)

write.csv(tasa.promedio.cuentap.nuble.., "tasa_promedio_cuentap_nuble.csv")

################################################################
# tasa de trabajador dependiente con contrato fijo del total de ocupados en nuble
################################################################
#
tasa.dep.fijo.nuble.sector = svyby(~I(cat.ocup=="asalariado con contrato definido"  & 
                                        cae_general=="Ocupado" & prov_e==84),
                                   by=~sector2, denominator=~I(cae_general=="Ocupado" & prov_e==84), 
                                   info2, svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cat.ocup=="asalariado con contrato definido" & cae_general=="Ocupado" & prov_e==84)+sector2, data=info2$variables)[2,]

tasa.dep.fijo.nuble.sector$cv = 
  tasa.dep.fijo.nuble.sector$`se.I(cat.ocup == \"asalariado con contrato definido\" & cae_general == \"Ocupado\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`/tasa.dep.fijo.nuble.sector$`I(cat.ocup == \"asalariado con contrato definido\" & cae_general == \"Ocupado\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`

tasa.dep.fijo.nuble.sector$frecuencia = freq

names(tasa.dep.fijo.nuble.sector) = 
  c("sector2", "tasa.dep.fijo.nuble.sector", "error estandar", "cv", "frecuencia")

write.csv(tasa.dep.fijo.nuble.sector, "tasa_dep_fijo_nuble_sector.csv")

#
tasa.promedio.dep.fijo.nuble = list()
for (i in 1:length(info)){
  tasa.promedio.dep.fijo.nuble[[i]] = svyratio(~I(cat.ocup=="asalariado con contrato definido" & 
                                                    cae_general=="Ocupado" & prov_e==84),
                                               denominator=~I(cae_general=="Ocupado" & prov_e==84), info[[i]],
                                               multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.dep.fijo.nuble. = unlist(lapply(tasa.promedio.dep.fijo.nuble, '[[', 1) ) 

ee.tasa.promedio.dep.fijo.nuble = unlist(lapply(tasa.promedio.dep.fijo.nuble, SE))

tasa.promedio.dep.fijo.nuble.. = data.frame(tasa.promedio.dep.fijo.nuble., ee.tasa.promedio.dep.fijo.nuble)

write.csv(tasa.promedio.dep.fijo.nuble.., "tasa_promedio_dep_fijo_nuble.csv")

################################################################
# tasa de trabajador dependiente con contrato indefinido del total de ocupados en nuble
################################################################
#
tasa.dep.indef.nuble.sector = svyby(~I(cat.ocup=="asalariado con contrato indefinido" & 
                                         cae_general=="Ocupado" & prov_e==84),
                                    by=~sector2, denominator=~I(cae_general=="Ocupado" & prov_e==84), info2, svyratio,
                                    multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)


freq= xtabs(~I(cat.ocup=="asalariado con contrato indefinido" & cae_general=="Ocupado" & prov_e==84)+sector2, data=info2$variables)[2,]

tasa.dep.indef.nuble.sector$cv = 
  tasa.dep.indef.nuble.sector$`se.I(cat.ocup == \"asalariado con contrato indefinido\" & cae_general == \"Ocupado\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`/tasa.dep.indef.nuble.sector$`I(cat.ocup == \"asalariado con contrato indefinido\" & cae_general == \"Ocupado\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`

tasa.dep.indef.nuble.sector$frecuencia = freq

names(tasa.dep.indef.nuble.sector) = 
  c("sector2", "tasa.dep.indef.nuble.sector", "error estandar", "cv", "frecuencia")

write.csv(tasa.dep.indef.nuble.sector, "tasa_dep_indef_nuble_sector.csv")

#
tasa.promedio.dep.indef.nuble = list()
for (i in 1:length(info)){
  tasa.promedio.dep.indef.nuble[[i]] = svyratio(~I(cat.ocup=="asalariado con contrato indefinido" & 
                                                     cae_general=="Ocupado" & prov_e==84),
                                                denominator=~I(cae_general=="Ocupado" & prov_e==84), info[[i]],
                                                multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.dep.indef.nuble. = unlist(lapply(tasa.promedio.dep.indef.nuble, '[[', 1) ) 

ee.tasa.promedio.dep.indef.nuble = unlist(lapply(tasa.promedio.dep.indef.nuble, SE))

tasa.promedio.dep.indef.nuble.. = data.frame(tasa.promedio.dep.indef.nuble., ee.tasa.promedio.dep.indef.nuble)

write.csv(tasa.promedio.dep.indef.nuble.., "tasa_promedio_dep_indef_nuble.csv")

################################################################
# tasa de dependiente sin contrato del total de ocupados en nuble
################################################################
#
tasa.dep.sin.contrato.nuble.sector = svyby(~I(cat.ocup=="asalariado sin contrato" & prov_e==84),
                                           by=~sector2, denominator=~I(cae_general=="Ocupado" & prov_e==84), 
                                           info2, svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cat.ocup=="asalariado sin contrato" & prov_e==84)+sector2, data=info2$variables)[2,]

tasa.dep.sin.contrato.nuble.sector$cv = 
  tasa.dep.sin.contrato.nuble.sector$`se.I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`/tasa.dep.sin.contrato.nuble.sector$`I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`

tasa.dep.sin.contrato.nuble.sector$frecuencia = freq

names(tasa.dep.sin.contrato.nuble.sector) = 
  c("sector2", "tasa.dep.sin.contrato.nuble.sector", "error estandar", "cv", "frecuencia")

write.csv(tasa.dep.sin.contrato.nuble.sector, "tasa_dep_sin_contrato_nuble_sector.csv")

#
tasa.promedio.dep.sin.contrato.nuble = list()
for (i in 1:length(info)){
  tasa.promedio.dep.sin.contrato.nuble[[i]] = svyratio(~I(cat.ocup=="asalariado sin contrato" & prov_e==84),
                                                       denominator=~I(cae_general=="Ocupado" & prov_e==84), info[[i]],
                                                       multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.dep.sin.contrato.nuble. = unlist(lapply(tasa.promedio.dep.sin.contrato.nuble, '[[', 1) ) 

ee.tasa.promedio.dep.sin.contrato.nuble = unlist(lapply(tasa.promedio.dep.sin.contrato.nuble, SE))

tasa.promedio.dep.sin.contrato.nuble.. = data.frame(tasa.promedio.dep.sin.contrato.nuble., ee.tasa.promedio.dep.sin.contrato.nuble)

write.csv(tasa.promedio.dep.sin.contrato.nuble.., "tasa_promedio_dep_sin_contrato_nuble.csv")

################################################################
# tasa de familiar no remunerado del total de ocupados en nuble
################################################################
#
tasa.familiar.no.remunerado.nuble.sector = svyby(~I(categoria_ocupacion==7 & cae_general=="Ocupado" & prov_e==84),
                                                 by=~sector2, denominator=~I(cae_general=="Ocupado" & prov_e==84), 
                                                 info2, svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(categoria_ocupacion==7 & cae_general=="Ocupado" & prov_e==84)+sector2, data=info2$variables)[2,]

tasa.familiar.no.remunerado.nuble.sector$cv = 
  tasa.familiar.no.remunerado.nuble.sector$`se.I(categoria_ocupacion == 7 & cae_general == \"Ocupado\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`/tasa.familiar.no.remunerado.nuble.sector$`I(categoria_ocupacion == 7 & cae_general == \"Ocupado\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`

tasa.familiar.no.remunerado.nuble.sector$frecuencia = freq

names(tasa.familiar.no.remunerado.nuble.sector) = 
  c("sector2", "tasa.familiar.no.remunerado.nuble.sector", "error estandar","cv","frecuencia")

write.csv(tasa.familiar.no.remunerado.nuble.sector, "tasa_familiar_no_remunerado_nuble_sector.csv")

#
tasa.promedio.familiar.no.remunerado.nuble = list()
for (i in 1:length(info)){
  tasa.promedio.familiar.no.remunerado.nuble[[i]] = svyratio(~I(categoria_ocupacion==7 & cae_general=="Ocupado" & prov_e==84),
                                                             denominator=~I(cae_general=="Ocupado" & prov_e==84), info[[i]],
                                                             multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.familiar.no.remunerado.nuble. = unlist(lapply(tasa.promedio.familiar.no.remunerado.nuble, '[[', 1) ) 

ee.tasa.promedio.familiar.no.remunerado.nuble = unlist(lapply(tasa.promedio.familiar.no.remunerado.nuble, SE))

tasa.promedio.familiar.no.remunerado.nuble.. = data.frame(tasa.promedio.familiar.no.remunerado.nuble., ee.tasa.promedio.familiar.no.remunerado.nuble)

write.csv(tasa.promedio.familiar.no.remunerado.nuble.., "tasa_promedio_familiar_no_remunerado_nuble.csv")

### FIN CUADRO 11
