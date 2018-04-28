#-------------------------------------------------------------------------------
### CUADRO 30: 
#-------------------------------------------------------------------------------
###################################################################
# Total de ocupados nuble por ocupaciones a 1 digito
###################################################################
#
total.ocupados.nuble.1digito = list()
for (i in 1:length(info)){
  total.ocupados.nuble.1digito[[i]] = svyby(~I(prov_e==84),
                                            by=~b1, info[[i]], svytotal,
                                            multicore = TRUE, 
                                            drop.empty.groups = FALSE, na.rm=TRUE)
}

total.ocupados.nuble.1digito. = 
  do.call(rbind, total.ocupados.nuble.1digito)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(prov_e==84)+b1, data=info[[i]]$variables) %>% data.frame() %>%
    filter(`I.prov_e....84.`== TRUE)
}

freq = do.call(rbind, freq)

total.ocupados.nuble.1digito.$cv = 
  total.ocupados.nuble.1digito.$`se.I(prov_e == 84)TRUE`/total.ocupados.nuble.1digito.$`I(prov_e == 84)TRUE`

total.ocupados.nuble.1digito.$frecuencia = freq$Freq

total.ocupados.nuble.1digito. = 
  total.ocupados.nuble.1digito.[,c("b1", "I(prov_e == 84)TRUE", "se.I(prov_e == 84)TRUE","cv","frecuencia")]

write.csv(total.ocupados.nuble.1digito., "total_ocupados_nuble_1digito.csv")

################################################################
# tasa de dependiente sin contrato del total de ocupados en nuble by clasificación a 1 digito
################################################################
#
tasa.dep.sin.contrato.nuble.1digito = svyby(~I(cat.ocup=="asalariado sin contrato" & prov_e==84),
                                            by=~b1, denominator=~I(cae_general=="Ocupado" & prov_e==84), 
                                            info2, svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cat.ocup=="asalariado sin contrato" & prov_e==84)+b1, data=info2$variables)[2,]

tasa.dep.sin.contrato.nuble.1digito$cv = 
  tasa.dep.sin.contrato.nuble.1digito$`se.I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`/tasa.dep.sin.contrato.nuble.1digito$`I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`

tasa.dep.sin.contrato.nuble.1digito$frecuencia = freq

names(tasa.dep.sin.contrato.nuble.1digito) = 
  c("b1", "tasa.dep.sin.contrato.nuble.1digito", "error estandar", "cv", "frecuencia")

write.csv(tasa.dep.sin.contrato.nuble.1digito, "tasa_dep_sin_contrato_nuble_1digito.csv")

################################################################
# tasa de cuenta propia del total de ocupados en nuble por categoria a 1 digito
################################################################
#
tasa.cuentap.nuble.1digito= svyby(~I(cat.ocup=="cuenta propia" & prov_e==84),
                                  by=~b1, denominator=~I(cae_general=="Ocupado" & prov_e==84), 
                                  info2, svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cat.ocup=="cuenta propia" & prov_e==84)+b1, data=info2$variables)[2,]

tasa.cuentap.nuble.1digito$cv = 
  tasa.cuentap.nuble.1digito$`se.I(cat.ocup == \"cuenta propia\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`/tasa.cuentap.nuble.1digito$`I(cat.ocup == \"cuenta propia\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`

tasa.cuentap.nuble.1digito$frecuencia = freq

names(tasa.cuentap.nuble.1digito) = 
  c("b1", "tasa.cuentap.nuble.1digito", "error estandar", "cv", "frecuencia")

write.csv(tasa.cuentap.nuble.1digito, "tasa_cuentap_nuble_1digito.csv")


###################################################################
# Total de ocupados nuble por ocupaciones a 1 digito por categoria ocupacional y sexo
###################################################################
#
total.ocupados.nuble.1digito.sexo = list()
for (i in 1:length(info)){
  total.ocupados.nuble.1digito.sexo[[i]] = svyby(~I(prov_e==84),
                                                 by=~b1+sexo, info[[i]], svytotal,
                                                 multicore = TRUE, 
                                                 drop.empty.groups = FALSE, na.rm=TRUE)
}

total.ocupados.nuble.1digito.sexo. = 
  do.call(rbind, total.ocupados.nuble.1digito.sexo)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(prov_e==84)+b1+sexo, data=info[[i]]$variables) %>% data.frame() %>%
    filter(`I.prov_e....84.`== TRUE)
}

freq = do.call(rbind, freq)

total.ocupados.nuble.1digito.sexo.$cv = 
  total.ocupados.nuble.1digito.sexo.$`se.I(prov_e == 84)TRUE`/total.ocupados.nuble.1digito.sexo.$`I(prov_e == 84)TRUE`

total.ocupados.nuble.1digito.sexo.$frecuencia = freq$Freq

total.ocupados.nuble.1digito.sexo. = 
  total.ocupados.nuble.1digito.sexo.[,c("b1", "sexo", "I(prov_e == 84)TRUE", "se.I(prov_e == 84)TRUE","cv","frecuencia")]

write.csv(total.ocupados.nuble.1digito.sexo., "total_ocupados_nuble_1digito_sexo.csv")

################################################################
# tasa de dependiente sin contrato del total de ocupados en nuble by clasificación a 1 digito y  sexo
################################################################
#
tasa.dep.sin.contrato.nuble.1digito.sexo = svyby(~I(cat.ocup=="asalariado sin contrato" & prov_e==84),
                                                 by=~b1+sexo, denominator=~I(cae_general=="Ocupado" & prov_e==84), 
                                                 info2, svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cat.ocup=="asalariado sin contrato" & prov_e==84)+b1+sexo, data=info2$variables) %>% data.frame() %>% 
  filter(`I.cat.ocup.....asalariado.sin.contrato....prov_e....84.`==TRUE)

tasa.dep.sin.contrato.nuble.1digito.sexo$cv = 
  tasa.dep.sin.contrato.nuble.1digito.sexo$`se.I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`/tasa.dep.sin.contrato.nuble.1digito.sexo$`I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`

tasa.dep.sin.contrato.nuble.1digito.sexo$frecuencia = freq$Freq

names(tasa.dep.sin.contrato.nuble.1digito.sexo) = 
  c("b1", "sexo", "I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)", "se.I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)", "cv", "frecuencia")

write.csv(tasa.dep.sin.contrato.nuble.1digito.sexo, "tasa_dep_sin_contrato_nuble_1digito_sexo.csv")

################################################################
# tasa de cuenta propia del total de ocupados en nuble por categoria a 1 digito y sexo
################################################################
#
tasa.cuentap.nuble.1digito.sexo= svyby(~I(categoria_ocupacion==2 & prov_e==84),
                                       by=~b1+sexo, denominator=~I(prov_e==84), 
                                       info2, svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(categoria_ocupacion==2 & prov_e==84)+b1+sexo, data=info2$variables) %>% data.frame() %>% 
  filter(`I.categoria_ocupacion....2...prov_e....84.`==TRUE)

tasa.cuentap.nuble.1digito.sexo$cv = 
  tasa.cuentap.nuble.1digito.sexo$`se.I(categoria_ocupacion == 2 & prov_e == 84)/I(prov_e == 84)`/tasa.cuentap.nuble.1digito.sexo$`I(categoria_ocupacion == 2 & prov_e == 84)/I(prov_e == 84)`

tasa.cuentap.nuble.1digito.sexo$frecuencia = freq$Freq

names(tasa.cuentap.nuble.1digito.sexo) = 
  c("b1", "sexo", "tasa.cuentap.nuble.1digito", "error estandar", "cv", "frecuencia")

write.csv(tasa.cuentap.nuble.1digito.sexo, "tasa_cuentap_nuble_1digito.csv")

### FIN CUADRO 30
