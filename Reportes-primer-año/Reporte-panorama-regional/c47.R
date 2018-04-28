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
### CUADRO 47: Distribucion sectorial Nuble, segun edad, 2016. 
#-------------------------------------------------------------------------------
###################################################################
# Total de ocupados jovenes y no jovenes de nuble por sector categoria ocupacional
###################################################################
#
total.ocupados.nuble.sector.jovenes = list()
for (i in 1:length(info)){
  total.ocupados.nuble.sector.jovenes[[i]] = svyby(~I(prov_e==84),
                                                   by=~sector2+I(edad >= 15 & edad <= 29), info[[i]], svytotal,
                                                   multicore = TRUE, 
                                                   drop.empty.groups = FALSE, na.rm=TRUE)
}

total.ocupados.nuble.sector.jovenes. = 
  do.call(rbind, total.ocupados.nuble.sector.jovenes)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(prov_e==84)+sector2+I(edad >= 15 & edad <= 29), data=info[[i]]$variables) %>% data.frame() %>%
    filter(`I.prov_e....84.`== TRUE)
}

freq = do.call(rbind, freq)

total.ocupados.nuble.sector.jovenes.$cv = 
  total.ocupados.nuble.sector.jovenes.$`se.I(prov_e == 84)TRUE`/total.ocupados.nuble.sector.jovenes.$`I(prov_e == 84)TRUE`

total.ocupados.nuble.sector.jovenes.$frecuencia = freq$Freq

total.ocupados.nuble.sector.jovenes. = 
  total.ocupados.nuble.sector.jovenes.[,c("sector2", "I(edad >= 15 & edad <= 29)", "I(prov_e == 84)TRUE", "se.I(prov_e == 84)TRUE","cv","frecuencia")]

write.csv(total.ocupados.nuble.sector.jovenes., "total_ocupados_nuble_sector_jovenes.csv")

################################################################
# tasa de dependiente sin contrato del total de ocupados en nuble by sector y  sexo
################################################################
#
tasa.dep.sin.contrato.nuble.sector.jovenes = svyby(~I(cat.ocup=="asalariado sin contrato" & prov_e==84),
                                                   by=~sector2+I(edad >= 15 & edad <= 29), denominator=~I(cae_general=="Ocupado" & prov_e==84), 
                                                   info2, svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cat.ocup=="asalariado sin contrato" & prov_e==84)+sector2+I(edad >= 15 & edad <= 29), data=info2$variables) %>% data.frame() %>% filter(`I.cat.ocup.....asalariado.sin.contrato....prov_e....84.`==TRUE)

tasa.dep.sin.contrato.nuble.sector.jovenes$cv = 
  tasa.dep.sin.contrato.nuble.sector.jovenes$`se.I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`/tasa.dep.sin.contrato.nuble.sector.jovenes$`I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`

tasa.dep.sin.contrato.nuble.sector.jovenes$frecuencia = freq$Freq

names(tasa.dep.sin.contrato.nuble.sector.jovenes) = 
  c("sector2", "I(edad >= 15 & edad <= 29)", "I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)", "se.I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)", "cv", "frecuencia")

write.csv(tasa.dep.sin.contrato.nuble.sector.jovenes, "tasa_dep_sin_contrato_nuble_sector_jovenes.csv")

#
tasa.dep.sin.contrato.nuble.jovenes.promedio = svyby(~I(cat.ocup=="asalariado sin contrato" & prov_e==84),
                                                     by=~I(edad >= 15 & edad <= 29), denominator=~I(cae_general=="Ocupado" & prov_e==84), 
                                                     info2, svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cat.ocup=="asalariado sin contrato" & prov_e==84)+I(edad >= 15 & edad <= 29), data=info2$variables) %>% data.frame() %>% filter(`I.cat.ocup.....asalariado.sin.contrato....prov_e....84.`==TRUE)

tasa.dep.sin.contrato.nuble.jovenes.promedio$cv = 
  tasa.dep.sin.contrato.nuble.jovenes.promedio$`se.I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`/tasa.dep.sin.contrato.nuble.jovenes.promedio$`I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`

tasa.dep.sin.contrato.nuble.jovenes.promedio$frecuencia = freq$Freq

names(tasa.dep.sin.contrato.nuble.jovenes.promedio) = 
  c("I(edad >= 15 & edad <= 29)", "I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)", "se.I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)", "cv", "frecuencia")

write.csv(tasa.dep.sin.contrato.nuble.jovenes.promedio, "tasa_dep_sin_contrato_nuble_jovenes_promedio.csv")

################################################################
# tasa de cuenta propia del total de ocupados en nuble por sector y sexo
################################################################
#
tasa.cuentap.nuble.sector.jovenes= svyby(~I(categoria_ocupacion==2 & cae_general=="Ocupado" & prov_e==84),
                                         by=~sector2+I(edad >= 15 & edad <= 29), denominator=~I(cae_general=="Ocupado" & prov_e==84), 
                                         info2, svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(categoria_ocupacion==2 & cae_general=="Ocupado" & prov_e==84)+sector2+I(edad >= 15 & edad <= 29), data=info2$variables) %>% data.frame() %>% filter(`I.categoria_ocupacion....2...cae_general.....Ocupado....prov_e....84.`==TRUE)

tasa.cuentap.nuble.sector.jovenes$cv = 
  tasa.cuentap.nuble.sector.jovenes$`se.I(categoria_ocupacion == 2 & cae_general == \"Ocupado\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`/tasa.cuentap.nuble.sector.jovenes$`I(categoria_ocupacion == 2 & cae_general == \"Ocupado\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`

tasa.cuentap.nuble.sector.jovenes$frecuencia = freq$Freq

names(tasa.cuentap.nuble.sector.jovenes) = 
  c("sector2", "I(edad >= 15 & edad <= 29)", "tasa.cuentap.nuble.sector2.sexo", "error estandar", "cv", "frecuencia")

write.csv(tasa.cuentap.nuble.sector.jovenes, "tasa_cuentap_nuble_sector_jovenes.csv")

#
tasa.cuentap.nuble.jovenes.promedio= svyby(~I(categoria_ocupacion==2 & cae_general=="Ocupado" & prov_e==84),
                                           by=~I(edad >= 15 & edad <= 29), denominator=~I(cae_general=="Ocupado" & prov_e==84), 
                                           info2, svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(categoria_ocupacion==2 & cae_general=="Ocupado" & prov_e==84)+I(edad >= 15 & edad <= 29), data=info2$variables) %>% data.frame() %>% filter(`I.categoria_ocupacion....2...cae_general.....Ocupado....prov_e....84.`==TRUE)

tasa.cuentap.nuble.jovenes.promedio$cv = 
  tasa.cuentap.nuble.jovenes.promedio$`se.I(categoria_ocupacion == 2 & cae_general == \"Ocupado\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`/tasa.cuentap.nuble.jovenes.promedio$`I(categoria_ocupacion == 2 & cae_general == \"Ocupado\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`

tasa.cuentap.nuble.jovenes.promedio$frecuencia = freq$Freq

names(tasa.cuentap.nuble.jovenes.promedio) = 
  c("I(edad >= 15 & edad <= 29)", "tasa.cuentap.nuble.sexo.promedio", "error estandar", "cv", "frecuencia")

write.csv(tasa.cuentap.nuble.jovenes.promedio, "tasa_cuentap_nuble_jovenes_promedio.csv")

### FIN CUADRO 47
