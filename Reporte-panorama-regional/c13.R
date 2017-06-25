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
### CUADRO 13: 
#-------------------------------------------------------------------------------

###################################################################
# Indicadores de seguridad social de salud : NUBLE
###################################################################
#
tasa.cot.salud.nuble.sector = svyby(~I((cat.ocup=="asalariado sin contrato" | 
                                          cat.ocup=="asalariado con contrato definido" | 
                                          cat.ocup=="asalariado con contrato indefinido") & 
                                         b7_4==1 & prov_e==84),
                                    by=~sector2, denominator=~I(prov_e==84 & (cat.ocup=="asalariado sin contrato" | 
                                                                                cat.ocup=="asalariado con contrato definido" | 
                                                                                cat.ocup=="asalariado con contrato indefinido")), 
                                    info2, svyratio, multicore = TRUE, 
                                    drop.empty.groups = FALSE, na.rm=TRUE)


freq= xtabs(~I((cat.ocup=="asalariado sin contrato" | 
                  cat.ocup=="asalariado con contrato definido" | 
                  cat.ocup=="asalariado con contrato indefinido") & b7_4==1 & prov_e==84)+sector2, data=info2$variables)[2,]

tasa.cot.salud.nuble.sector$cv = 
  tasa.cot.salud.nuble.sector$`se.I((cat.ocup == \"asalariado sin contrato\" | cat.ocup == \"asalariado con contrato definido\" | cat.ocup == \"asalariado con contrato indefinido\") & b7_4 == 1 & prov_e == 84)/I(prov_e == 84 & (cat.ocup == \"asalariado sin contrato\" | cat.ocup == \"asalariado con contrato definido\" | cat.ocup == \"asalariado con contrato indefinido\"))`/tasa.cot.salud.nuble.sector$`I((cat.ocup == \"asalariado sin contrato\" | cat.ocup == \"asalariado con contrato definido\" | cat.ocup == \"asalariado con contrato indefinido\") & b7_4 == 1 & prov_e == 84)/I(prov_e == 84 & (cat.ocup == \"asalariado sin contrato\" | cat.ocup == \"asalariado con contrato definido\" | cat.ocup == \"asalariado con contrato indefinido\"))`

tasa.cot.salud.nuble.sector$frecuencia = freq

names(tasa.cot.salud.nuble.sector) = 
  c("sector", "tasa.cot.salud.nuble.sector", "error estandar","cv","frecuencia")

write.csv(tasa.cot.salud.nuble.sector, "tasa_cot_salud_nuble_sector.csv")

#
tasa.promedio.cot.salud.nuble = list()
for (i in 1:length(info)){
  tasa.promedio.cot.salud.nuble[[i]] = svyratio(~I((cat.ocup=="asalariado sin contrato" | 
                                                      cat.ocup=="asalariado con contrato definido" | 
                                                      cat.ocup=="asalariado con contrato indefinido") & b7_4==1 & prov_e==84), 
                                                denominator=~I(prov_e==84 & (cat.ocup=="asalariado sin contrato" | 
                                                                               cat.ocup=="asalariado con contrato definido" | 
                                                                               cat.ocup=="asalariado con contrato indefinido")),
                                                info[[i]], multicore = TRUE, 
                                                drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.cot.salud.nuble. = unlist(lapply(tasa.promedio.cot.salud.nuble, '[[', 1) ) 

ee.tasa.promedio.cot.salud.nuble = unlist(lapply(tasa.promedio.cot.salud.nuble, SE))

tasa.promedio.cot.salud.nuble.. = data.frame(tasa.promedio.cot.salud.nuble., ee.tasa.promedio.cot.salud.nuble)

write.csv(tasa.promedio.cot.salud.nuble.., "tasa_promedio_cot_salud_nuble.csv")

#############################################################
# Indicadores de seguridad social de  afp: NUBLE
#############################################################
#
tasa.cot.afp.nuble.sector = svyby(~I((cat.ocup=="asalariado sin contrato" | 
                                        cat.ocup=="asalariado con contrato definido" | 
                                        cat.ocup=="asalariado con contrato indefinido") & b7_3==1 & prov_e==84),
                                  by=~sector2, denominator=~I(prov_e==84 & (cat.ocup=="asalariado sin contrato" | 
                                                                              cat.ocup=="asalariado con contrato definido" | 
                                                                              cat.ocup=="asalariado con contrato indefinido")), 
                                  info2, svyratio, multicore = TRUE, 
                                  drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I((cat.ocup=="asalariado sin contrato" | 
                  cat.ocup=="asalariado con contrato definido" | 
                  cat.ocup=="asalariado con contrato indefinido") & b7_3==1 & prov_e==84)+sector2, data=info2$variables)[2,]

tasa.cot.afp.nuble.sector$cv = 
  tasa.cot.afp.nuble.sector$`se.I((cat.ocup == \"asalariado sin contrato\" | cat.ocup == \"asalariado con contrato definido\" | cat.ocup == \"asalariado con contrato indefinido\") & b7_3 == 1 & prov_e == 84)/I(prov_e == 84 & (cat.ocup == \"asalariado sin contrato\" | cat.ocup == \"asalariado con contrato definido\" | cat.ocup == \"asalariado con contrato indefinido\"))`/tasa.cot.afp.nuble.sector$`I((cat.ocup == \"asalariado sin contrato\" | cat.ocup == \"asalariado con contrato definido\" | cat.ocup == \"asalariado con contrato indefinido\") & b7_3 == 1 & prov_e == 84)/I(prov_e == 84 & (cat.ocup == \"asalariado sin contrato\" | cat.ocup == \"asalariado con contrato definido\" | cat.ocup == \"asalariado con contrato indefinido\"))`

tasa.cot.afp.nuble.sector$frecuencia = freq

names(tasa.cot.afp.nuble.sector) = 
  c("sector", "tasa.cot.afp.nuble.sector", "error estandar","cv","frecuencia")

write.csv(tasa.cot.afp.nuble.sector, "tasa_cot_afp_nuble_sector.csv")

#
tasa.promedio.cot.afp.nuble = list()
for (i in 1:length(info)){
  tasa.promedio.cot.afp.nuble[[i]] = svyratio(~I((cat.ocup=="asalariado sin contrato" | 
                                                    cat.ocup=="asalariado con contrato definido" | 
                                                    cat.ocup=="asalariado con contrato indefinido") & b7_3==1 & prov_e==84), 
                                              denominator=~I(prov_e==84 & (cat.ocup=="asalariado sin contrato" | 
                                                                             cat.ocup=="asalariado con contrato definido" | 
                                                                             cat.ocup=="asalariado con contrato indefinido")),
                                              info[[i]], multicore = TRUE, 
                                              drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.cot.afp.nuble. = unlist(lapply(tasa.promedio.cot.afp.nuble, '[[', 1) ) 

ee.tasa.promedio.cot.afp.nuble = unlist(lapply(tasa.promedio.cot.afp.nuble, SE))

tasa.promedio.cot.afp.nuble.. = data.frame(tasa.promedio.cot.afp.nuble., ee.tasa.promedio.cot.afp.nuble)

write.csv(tasa.promedio.cot.afp.nuble.., "tasa_promedio_cot_afp_nuble.csv")

### FIN CUADRO 13
