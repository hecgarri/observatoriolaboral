#-------------------------------------------------------------------------------
### CUADRO 28: Indicadores  de seguridad social de trabajadores dependientes de Ñuble, 2016. 
#-------------------------------------------------------------------------------
#############################################################################
# tasa trabajador dependiente con contrato de trabajo. NACIONAL
#############################################################################
#
tasa.trabaja.dep.con.contrato.nacional = list()
for (i in 1:length(info)){
  tasa.trabaja.dep.con.contrato.nacional[[i]] = svyratio(~I(cat.ocup=="asalariado con contrato definido" |
                                                              cat.ocup=="asalariado con contrato indefinido"), 
                                                         denominator=~I(cat.ocup=="asalariado sin contrato" |
                                                                          cat.ocup=="asalariado con contrato definido" |
                                                                          cat.ocup=="asalariado con contrato indefinido"),
                                                         info[[i]], multicore = TRUE, 
                                                         drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.trabaja.dep.con.contrato.nacional. = unlist(lapply(tasa.trabaja.dep.con.contrato.nacional, '[[', 1) ) 

ee.tasa.trabaja.dep.con.contrato.nacional = unlist(lapply(tasa.trabaja.dep.con.contrato.nacional, SE))

tasa.trabaja.dep.con.contrato.nacional.. = data.frame(tasa.trabaja.dep.con.contrato.nacional., ee.tasa.trabaja.dep.con.contrato.nacional)

write.csv(tasa.trabaja.dep.con.contrato.nacional.., "tasa_trabaja_dep_con_contrato_nacional.csv")

#############################################################################
# tasa trabajador dependiente con contrato indefinido. NACIONAL
#############################################################################
#
tasa.trabaja.dep.con.contrato.indef.nacional = list()
for (i in 1:length(info)){
  tasa.trabaja.dep.con.contrato.indef.nacional[[i]] = svyratio(~I(cat.ocup=="asalariado con contrato indefinido"), 
                                                               denominator=~I(cat.ocup=="asalariado sin contrato" |
                                                                                cat.ocup=="asalariado con contrato definido" |
                                                                                cat.ocup=="asalariado con contrato indefinido"),
                                                               info[[i]], multicore = TRUE, 
                                                               drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.trabaja.dep.con.contrato.indef.nacional. = unlist(lapply(tasa.trabaja.dep.con.contrato.indef.nacional, '[[', 1) ) 

ee.tasa.trabaja.dep.con.contrato.indef.nacional = unlist(lapply(tasa.trabaja.dep.con.contrato.indef.nacional, SE))

tasa.trabaja.dep.con.contrato.indef.nacional.. = data.frame(tasa.trabaja.dep.con.contrato.indef.nacional., ee.tasa.trabaja.dep.con.contrato.indef.nacional)

write.csv(tasa.trabaja.dep.con.contrato.indef.nacional.., "tasa_trabaja_dep_con_contrato_indef_nacional.csv")

#############################################################################
# tasa trabajador dependiente con contrato con vacaciones: NACIONAL
#############################################################################
#
tasa.trabaja.dep.con.contrato.vac.nacional = list()
for (i in 1:length(info)){
  tasa.trabaja.dep.con.contrato.vac.nacional[[i]] = svyratio(~I((cat.ocup=="asalariado con contrato definido" |
                                                                   cat.ocup=="asalariado con contrato indefinido") & b7_1==1), 
                                                             denominator=~I(cat.ocup=="asalariado sin contrato" |
                                                                              cat.ocup=="asalariado con contrato definido" |
                                                                              cat.ocup=="asalariado con contrato indefinido"),
                                                             info[[i]], multicore = TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.trabaja.dep.con.contrato.vac.nacional. = unlist(lapply(tasa.trabaja.dep.con.contrato.vac.nacional, '[[', 1) ) 

ee.tasa.trabaja.dep.con.contrato.vac.nacional = unlist(lapply(tasa.trabaja.dep.con.contrato.vac.nacional, SE))

tasa.trabaja.dep.con.contrato.vac.nacional.. = data.frame(tasa.trabaja.dep.con.contrato.vac.nacional., ee.tasa.trabaja.dep.con.contrato.vac.nacional)

write.csv(tasa.trabaja.dep.con.contrato.vac.nacional.., "tasa_trabaja_dep_con_contrato_vac_nacional.csv")

#############################################################################
# tasa trabajador dependiente con contrato con dias pagados por enfermedad: NACIONAL
#############################################################################
#
tasa.trabaja.dep.con.contrato.enf.nacional = list()
for (i in 1:length(info)){
  tasa.trabaja.dep.con.contrato.enf.nacional[[i]] = svyratio(~I((cat.ocup=="asalariado con contrato definido" |
                                                                   cat.ocup=="asalariado con contrato indefinido") & b7_2==1), 
                                                             denominator=~I(cat.ocup=="asalariado sin contrato" |
                                                                              cat.ocup=="asalariado con contrato definido" |
                                                                              cat.ocup=="asalariado con contrato indefinido"),
                                                             info[[i]], multicore = TRUE, 
                                                             drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.trabaja.dep.con.contrato.enf.nacional. = unlist(lapply(tasa.trabaja.dep.con.contrato.enf.nacional, '[[', 1) ) 

ee.tasa.trabaja.dep.con.contrato.enf.nacional = unlist(lapply(tasa.trabaja.dep.con.contrato.enf.nacional, SE))

tasa.trabaja.dep.con.contrato.enf.nacional.. = data.frame(tasa.trabaja.dep.con.contrato.enf.nacional., ee.tasa.trabaja.dep.con.contrato.enf.nacional)

write.csv(tasa.trabaja.dep.con.contrato.enf.nacional.., "tasa_trabaja_dep_con_contrato_enf_nacional.csv")

#############################################################################
# tasa trabajador dependiente con contrato con cotizaciones previsional: NACIONAL
#############################################################################
#
tasa.trabaja.dep.con.cot.afp.nacional = list()
for (i in 1:length(info)){
  tasa.trabaja.dep.con.cot.afp.nacional[[i]] = svyratio(~I((cat.ocup=="asalariado con contrato definido" |
                                                              cat.ocup=="asalariado con contrato indefinido") & b7_3==1), 
                                                        denominator=~I(cat.ocup=="asalariado sin contrato" |
                                                                         cat.ocup=="asalariado con contrato definido" |
                                                                         cat.ocup=="asalariado con contrato indefinido"),
                                                        info[[i]], multicore = TRUE, 
                                                        drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.trabaja.dep.con.cot.afp.nacional. = unlist(lapply(tasa.trabaja.dep.con.cot.afp.nacional, '[[', 1) ) 

ee.tasa.trabaja.dep.con.cot.afp.nacional= unlist(lapply(tasa.trabaja.dep.con.cot.afp.nacional, SE))

tasa.trabaja.dep.con.cot.afp.nacional.. = data.frame(tasa.trabaja.dep.con.cot.afp.nacional., ee.tasa.trabaja.dep.con.cot.afp.nacional)

write.csv(tasa.trabaja.dep.con.cot.afp.nacional.., "tasa_trabaja_dep_con_cont_afp_nacional.csv")

#############################################################################
# tasa trabajador dependiente con contrato con cotizaciones en salud: NACIONAL
#############################################################################
#
tasa.trabaja.dep.con.cot.salud.nacional = list()
for (i in 1:length(info)){
  tasa.trabaja.dep.con.cot.salud.nacional[[i]] = svyratio(~I((cat.ocup=="asalariado con contrato definido" |
                                                                cat.ocup=="asalariado con contrato indefinido") & b7_4==1), 
                                                          denominator=~I(cat.ocup=="asalariado sin contrato" |
                                                                           cat.ocup=="asalariado con contrato definido" |
                                                                           cat.ocup=="asalariado con contrato indefinido"),
                                                          info[[i]], multicore = TRUE, 
                                                          drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.trabaja.dep.con.cot.salud.nacional. = unlist(lapply(tasa.trabaja.dep.con.cot.salud.nacional, '[[', 1) ) 

ee.tasa.trabaja.dep.con.cot.salud.nacional= unlist(lapply(tasa.trabaja.dep.con.cot.salud.nacional, SE))

tasa.trabaja.dep.con.cot.salud.nacional.. = data.frame(tasa.trabaja.dep.con.cot.salud.nacional., ee.tasa.trabaja.dep.con.cot.salud.nacional)

write.csv(tasa.trabaja.dep.con.cot.salud.nacional.., "tasa_trabaja_dep_con_cont_salud_nacional.csv")

#############################################################################
# tasa trabajador dependiente con contrato de trabajo. NUBLE
#############################################################################
#
tasa.trabaja.dep.con.contrato.nuble = list()
for (i in 1:length(info)){
  tasa.trabaja.dep.con.contrato.nuble[[i]] = svyratio(~I((cat.ocup=="asalariado con contrato definido" |
                                                            cat.ocup=="asalariado con contrato indefinido") & b8==1 & prov_e==84), 
                                                      denominator=~I(prov_e==84 & (cat.ocup=="asalariado sin contrato" |
                                                                                     cat.ocup=="asalariado con contrato definido" |
                                                                                     cat.ocup=="asalariado con contrato indefinido")),
                                                      info[[i]], multicore = TRUE, 
                                                      drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.trabaja.dep.con.contrato.nuble. = unlist(lapply(tasa.trabaja.dep.con.contrato.nuble, '[[', 1) ) 

ee.tasa.trabaja.dep.con.contrato.nuble = unlist(lapply(tasa.trabaja.dep.con.contrato.nuble, SE))

tasa.trabaja.dep.con.contrato.nuble.. = data.frame(tasa.trabaja.dep.con.contrato.nuble., ee.tasa.trabaja.dep.con.contrato.nuble)

write.csv(tasa.trabaja.dep.con.contrato.nuble.., "tasa_trabaja_dep_con_contrato_nuble.csv")

#############################################################################
# tasa trabajador dependiente con contrato indefinido. NUBLE
#############################################################################
#
tasa.trabaja.dep.con.contrato.indef.nuble = list()
for (i in 1:length(info)){
  tasa.trabaja.dep.con.contrato.indef.nuble[[i]] = svyratio(~I(cat.ocup=="asalariado con contrato indefinido" & prov_e==84), 
                                                            denominator=~I(prov_e==84 & (cat.ocup=="asalariado sin contrato" |
                                                                                           cat.ocup=="asalariado con contrato definido" |
                                                                                           cat.ocup=="asalariado con contrato indefinido")),
                                                            info[[i]], multicore = TRUE, 
                                                            drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.trabaja.dep.con.contrato.indef.nuble. = unlist(lapply(tasa.trabaja.dep.con.contrato.indef.nuble, '[[', 1) ) 

ee.tasa.trabaja.dep.con.contrato.indef.nuble = unlist(lapply(tasa.trabaja.dep.con.contrato.indef.nuble, SE))

tasa.trabaja.dep.con.contrato.indef.nuble.. = data.frame(tasa.trabaja.dep.con.contrato.indef.nuble., ee.tasa.trabaja.dep.con.contrato.indef.nuble)

write.csv(tasa.trabaja.dep.con.contrato.indef.nuble.., "tasa_trabaja_dep_con_contrato_indef_nuble.csv")

#############################################################################
# tasa trabajador dependiente con contrato con vacaciones: NUBLE
#############################################################################
#
tasa.trabaja.dep.con.contrato.vac.nuble = list()
for (i in 1:length(info)){
  tasa.trabaja.dep.con.contrato.vac.nuble[[i]] = svyratio(~I((cat.ocup=="asalariado con contrato definido" |
                                                                cat.ocup=="asalariado con contrato indefinido") & b7_1==1 & prov_e==84), 
                                                          denominator=~I(prov_e==84 & (cat.ocup=="asalariado sin contrato" |
                                                                                         cat.ocup=="asalariado con contrato definido" |
                                                                                         cat.ocup=="asalariado con contrato indefinido")),
                                                          info[[i]], multicore = TRUE, 
                                                          drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.trabaja.dep.con.contrato.vac.nuble. = unlist(lapply(tasa.trabaja.dep.con.contrato.vac.nuble, '[[', 1) ) 

ee.tasa.trabaja.dep.con.contrato.vac.nuble = unlist(lapply(tasa.trabaja.dep.con.contrato.vac.nuble, SE))

tasa.trabaja.dep.con.contrato.vac.nuble.. = data.frame(tasa.trabaja.dep.con.contrato.vac.nuble., ee.tasa.trabaja.dep.con.contrato.vac.nuble)

write.csv(tasa.trabaja.dep.con.contrato.vac.nuble.., "tasa_trabaja_dep_con_contrato_vac_nuble.csv")

#############################################################################
# tasa trabajador dependiente con contrato con dias pagados por enfermedad: NUBLE
#############################################################################
#
tasa.trabaja.dep.con.contrato.enf.nuble = list()
for (i in 1:length(info)){
  tasa.trabaja.dep.con.contrato.enf.nuble[[i]] = svyratio(~I((cat.ocup=="asalariado con contrato definido" |
                                                                cat.ocup=="asalariado con contrato indefinido") & b7_2==1 & prov_e==84), 
                                                          denominator=~I(prov_e==84 & (cat.ocup=="asalariado sin contrato" |
                                                                                         cat.ocup=="asalariado con contrato definido" |
                                                                                         cat.ocup=="asalariado con contrato indefinido")),
                                                          info[[i]], multicore = TRUE, 
                                                          drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.trabaja.dep.con.contrato.enf.nuble. = unlist(lapply(tasa.trabaja.dep.con.contrato.enf.nuble, '[[', 1) ) 

ee.tasa.trabaja.dep.con.contrato.enf.nuble = unlist(lapply(tasa.trabaja.dep.con.contrato.enf.nuble, SE))

tasa.trabaja.dep.con.contrato.enf.nuble.. = data.frame(tasa.trabaja.dep.con.contrato.enf.nuble., ee.tasa.trabaja.dep.con.contrato.enf.nuble)

write.csv(tasa.trabaja.dep.con.contrato.enf.nuble.., "tasa_trabaja_dep_con_contrato_enf_nuble.csv")

#############################################################################
# tasa trabajador dependiente con contrato con cotizaciones previsional: NUBLE
#############################################################################
#
tasa.trabaja.dep.con.cot.afp.nuble = list()
for (i in 1:length(info)){
  tasa.trabaja.dep.con.cot.afp.nuble[[i]] = svyratio(~I((cat.ocup=="asalariado con contrato definido" |
                                                           cat.ocup=="asalariado con contrato indefinido") & b7_3==1 & prov_e==84), 
                                                     denominator=~I(prov_e==84 & (cat.ocup=="asalariado sin contrato" |
                                                                                    cat.ocup=="asalariado con contrato definido" |
                                                                                    cat.ocup=="asalariado con contrato indefinido")),
                                                     info[[i]], multicore = TRUE, 
                                                     drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.trabaja.dep.con.cot.afp.nuble. = unlist(lapply(tasa.trabaja.dep.con.cot.afp.nuble, '[[', 1) ) 

ee.tasa.trabaja.dep.con.cot.afp.nuble = unlist(lapply(tasa.trabaja.dep.con.cot.afp.nuble, SE))

tasa.trabaja.dep.con.cot.afp.nuble.. = data.frame(tasa.trabaja.dep.con.cot.afp.nuble., ee.tasa.trabaja.dep.con.cot.afp.nuble)

write.csv(tasa.trabaja.dep.con.cot.afp.nuble.., "tasa_trabaja_dep_con_cont_afp_nuble.csv")

#############################################################################
# tasa trabajador dependiente con contrato con cotizaciones en salud: NUBLE
#############################################################################
#
tasa.trabaja.dep.con.cot.salud.nuble = list()
for (i in 1:length(info)){
  tasa.trabaja.dep.con.cot.salud.nuble[[i]] = svyratio(~I((cat.ocup=="asalariado con contrato definido" |
                                                             cat.ocup=="asalariado con contrato indefinido") & b7_4==1 & prov_e==84), 
                                                       denominator=~I(prov_e==84 & (cat.ocup=="asalariado sin contrato" |
                                                                                      cat.ocup=="asalariado con contrato definido" |
                                                                                      cat.ocup=="asalariado con contrato indefinido")),
                                                       info[[i]], multicore = TRUE, 
                                                       drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.trabaja.dep.con.cot.salud.nuble. = unlist(lapply(tasa.trabaja.dep.con.cot.salud.nuble, '[[', 1) ) 

ee.tasa.trabaja.dep.con.cot.salud.nuble = unlist(lapply(tasa.trabaja.dep.con.cot.salud.nuble, SE))

tasa.trabaja.dep.con.cot.salud.nuble.. = data.frame(tasa.trabaja.dep.con.cot.salud.nuble., ee.tasa.trabaja.dep.con.cot.salud.nuble)

write.csv(tasa.trabaja.dep.con.cot.salud.nuble.., "tasa_trabaja_dep_con_cont_salud_nuble.csv")

### FIN CUADRO 28
