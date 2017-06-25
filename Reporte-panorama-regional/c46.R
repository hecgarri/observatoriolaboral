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
### CUADRO 46: 
#-------------------------------------------------------------------------------
###################################################################
# Total de ocupados jovenes y no jovenes de nuble por sector categoria ocupacional
###################################################################
#
total.ocupados.nuble.1digito.jovenes = list()
for (i in 1:length(info)){
  total.ocupados.nuble.1digito.jovenes[[i]] = svyby(~I(prov_e==84),
                                                    by=~b1+I(edad >= 15 & edad <= 29), info[[i]], svytotal,
                                                    multicore = TRUE, 
                                                    drop.empty.groups = FALSE, na.rm=TRUE)
}

total.ocupados.nuble.1digito.jovenes. = 
  do.call(rbind, total.ocupados.nuble.1digito.jovenes)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(prov_e==84)+b1+I(edad >= 15 & edad <= 29), data=info[[i]]$variables) %>% data.frame() %>%
    filter(`I.prov_e....84.`== TRUE)
}

freq = do.call(rbind, freq)

total.ocupados.nuble.1digito.jovenes.$cv = 
  total.ocupados.nuble.1digito.jovenes.$`se.I(prov_e == 84)TRUE`/total.ocupados.nuble.1digito.jovenes.$`I(prov_e == 84)TRUE`

total.ocupados.nuble.1digito.jovenes.$frecuencia = freq$Freq

total.ocupados.nuble.1digito.jovenes. = 
  total.ocupados.nuble.1digito.jovenes.[,c("b1", "I(edad >= 15 & edad <= 29)", "I(prov_e == 84)TRUE", "se.I(prov_e == 84)TRUE","cv","frecuencia")]

write.csv(total.ocupados.nuble.1digito.jovenes., "total_ocupados_nuble_idigito_jovenes.csv")

################################################################
# tasa de dependiente sin contrato del total de ocupados en nuble by sector y  sexo
################################################################
#
tasa.dep.sin.contrato.nuble.1digito.jovenes = svyby(~I(cat.ocup=="asalariado sin contrato" & prov_e==84),
                                                    by=~b1+I(edad >= 15 & edad <= 29), denominator=~I(cae_general=="Ocupado" & prov_e==84), 
                                                    info2, svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cat.ocup=="asalariado sin contrato" & prov_e==84)+b1+I(edad >= 15 & edad <= 29), data=info2$variables) %>% data.frame() %>% filter(`I.cat.ocup.....asalariado.sin.contrato....prov_e....84.`==TRUE)

tasa.dep.sin.contrato.nuble.1digito.jovenes$cv = 
  tasa.dep.sin.contrato.nuble.1digito.jovenes$`se.I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`/tasa.dep.sin.contrato.nuble.1digito.jovenes$`I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`

tasa.dep.sin.contrato.nuble.1digito.jovenes$frecuencia = freq$Freq

tasa.dep.sin.contrato.nuble.1digito.jovenes = 
  tasa.dep.sin.contrato.nuble.1digito.jovenes[,c("b1", "I(edad >= 15 & edad <= 29)", "I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)", "se.I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)", "cv", "frecuencia")]

write.csv(tasa.dep.sin.contrato.nuble.1digito.jovenes, "tasa_dep_sin_contrato_nuble_1digito_jovenes.csv")

################################################################
# tasa de cuenta propia del total de ocupados en nuble por sector y sexo
################################################################
#
tasa.cuentap.nuble.1digito.jovenes= svyby(~I(categoria_ocupacion==2 & cae_general=="Ocupado" & prov_e==84),
                                          by=~b1+I(edad >= 15 & edad <= 29), denominator=~I(cae_general=="Ocupado" & prov_e==84), 
                                          info2, svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(categoria_ocupacion==2 & cae_general=="Ocupado" & prov_e==84)+b1+I(edad >= 15 & edad <= 29), data=info2$variables) %>% data.frame() %>% filter(`I.categoria_ocupacion....2...cae_general.....Ocupado....prov_e....84.`==TRUE)

tasa.cuentap.nuble.1digito.jovenes$cv = 
  tasa.cuentap.nuble.1digito.jovenes$`se.I(categoria_ocupacion == 2 & cae_general == \"Ocupado\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`/tasa.cuentap.nuble.1digito.jovenes$`I(categoria_ocupacion == 2 & cae_general == \"Ocupado\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`

tasa.cuentap.nuble.1digito.jovenes$frecuencia = freq$Freq

names(tasa.cuentap.nuble.1digito.jovenes) = 
  c("b1", "I(edad >= 15 & edad <= 29)", "tasa.cuentap.nuble.sector2.sexo", "error estandar", "cv", "frecuencia")

write.csv(tasa.cuentap.nuble.1digito.jovenes, "tasa_cuentap_nuble_1digito_jovenes.csv")

### FIN CUADRO 46
