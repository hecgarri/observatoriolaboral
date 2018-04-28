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
### CUADRO 39: Distribucion sectorial de Nuble, segun genero, 2015. 
#-------------------------------------------------------------------------------
###################################################################
# Total de ocupados nuble por sector economico y sexo
###################################################################
#
total.ocupados.nuble.sector.sexo = list()
for (i in 1:length(info)){
  total.ocupados.nuble.sector.sexo[[i]] = svyby(~I(prov_e==84),
                                                by=~sector2+sexo, info[[i]], svytotal,
                                                multicore = TRUE, 
                                                drop.empty.groups = FALSE, na.rm=TRUE)
}

total.ocupados.nuble.sector.sexo. = 
  do.call(rbind, total.ocupados.nuble.sector.sexo)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(prov_e==84)+sector2+sexo, data=info[[i]]$variables) %>% data.frame() %>%
    filter(`I.prov_e....84.`== TRUE)
}

freq = do.call(rbind, freq)

total.ocupados.nuble.sector.sexo.$cv = 
  total.ocupados.nuble.sector.sexo.$`se.I(prov_e == 84)TRUE`/total.ocupados.nuble.sector.sexo.$`I(prov_e == 84)TRUE`

total.ocupados.nuble.sector.sexo.$frecuencia = freq$Freq

total.ocupados.nuble.sector.sexo. = 
  total.ocupados.nuble.sector.sexo.[,c("sector2", "sexo", "I(prov_e == 84)TRUE", "se.I(prov_e == 84)TRUE","cv","frecuencia")]

write.csv(total.ocupados.nuble.sector.sexo., "total_ocupados_nuble_sector_sexo.csv")

################################################################
# tasa de dependiente sin contrato del total de ocupados en nuble by sector y  sexo
################################################################
#
tasa.dep.sin.contrato.nuble.sector.sexo = svyby(~I(cat.ocup=="asalariado sin contrato" & prov_e==84),
                                                by=~sector2+sexo, denominator=~I(cae_general=="Ocupado" & prov_e==84), 
                                                info2, svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cat.ocup=="asalariado sin contrato" & prov_e==84)+sector2+sexo, data=info2$variables) %>% data.frame() %>% filter(`I.cat.ocup.....asalariado.sin.contrato....prov_e....84.`==TRUE)

tasa.dep.sin.contrato.nuble.sector.sexo$cv = 
  tasa.dep.sin.contrato.nuble.sector.sexo$`se.I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`/tasa.dep.sin.contrato.nuble.sector.sexo$`I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`

tasa.dep.sin.contrato.nuble.sector.sexo$frecuencia = freq$Freq

names(tasa.dep.sin.contrato.nuble.sector.sexo) = 
  c("sector2", "sexo", "I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)", "se.I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)", "cv", "frecuencia")

write.csv(tasa.dep.sin.contrato.nuble.sector.sexo, "tasa_dep_sin_contrato_nuble_sector_sexo.csv")

#
tasa.dep.sin.contrato.nuble.sexo.promedio = svyby(~I(cat.ocup=="asalariado sin contrato" & prov_e==84),
                                                  by=~sexo, denominator=~I(cae_general=="Ocupado" & prov_e==84), 
                                                  info2, svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cat.ocup=="asalariado sin contrato" & prov_e==84)+sexo, data=info2$variables) %>% data.frame() %>% filter(`I.cat.ocup.....asalariado.sin.contrato....prov_e....84.`==TRUE)

tasa.dep.sin.contrato.nuble.sexo.promedio$cv = 
  tasa.dep.sin.contrato.nuble.sexo.promedio$`se.I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`/tasa.dep.sin.contrato.nuble.sexo.promedio$`I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`

tasa.dep.sin.contrato.nuble.sexo.promedio$frecuencia = freq$Freq

names(tasa.dep.sin.contrato.nuble.sexo.promedio) = 
  c("sexo", "I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)", "se.I(cat.ocup == \"asalariado sin contrato\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)", "cv", "frecuencia")

write.csv(tasa.dep.sin.contrato.nuble.sexo.promedio, "tasa_dep_sin_contrato_nuble_sexo_promedio.csv")

################################################################
# tasa de cuenta propia del total de ocupados en nuble por sector y sexo
################################################################
#
tasa.cuentap.nuble.sector.sexo= svyby(~I(categoria_ocupacion==2 & cae_general=="Ocupado" & prov_e==84),
                                      by=~sector2+sexo, denominator=~I(cae_general=="Ocupado" & prov_e==84), 
                                      info2, svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(categoria_ocupacion==2 & cae_general=="Ocupado" & prov_e==84)+sector2+sexo, data=info2$variables) %>% data.frame() %>% filter(`I.categoria_ocupacion....2...cae_general.....Ocupado....prov_e....84.`==TRUE)

tasa.cuentap.nuble.sector.sexo$cv = 
  tasa.cuentap.nuble.sector.sexo$`se.I(categoria_ocupacion == 2 & cae_general == \"Ocupado\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`/tasa.cuentap.nuble.sector.sexo$`I(categoria_ocupacion == 2 & cae_general == \"Ocupado\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`

tasa.cuentap.nuble.sector.sexo$frecuencia = freq$Freq

names(tasa.cuentap.nuble.sector.sexo) = 
  c("sector2", "sexo", "tasa.cuentap.nuble.sector2.sexo", "error estandar", "cv", "frecuencia")

write.csv(tasa.cuentap.nuble.sector.sexo, "tasa_cuentap_nuble_sector.sexo.csv")

#
tasa.cuentap.nuble.sexo.promedio = svyby(~I(categoria_ocupacion==2 & cae_general=="Ocupado" & prov_e==84),
                                         by=~sexo, denominator=~I(cae_general=="Ocupado" & prov_e==84), 
                                         info2, svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(categoria_ocupacion==2 & cae_general=="Ocupado" & prov_e==84)+sexo, data=info2$variables) %>% data.frame() %>% filter(`I.categoria_ocupacion....2...cae_general.....Ocupado....prov_e....84.`==TRUE)

tasa.cuentap.nuble.sexo.promedio$cv = 
  tasa.cuentap.nuble.sexo.promedio$`se.I(categoria_ocupacion == 2 & cae_general == \"Ocupado\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`/tasa.cuentap.nuble.sexo.promedio$`I(categoria_ocupacion == 2 & cae_general == \"Ocupado\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`

tasa.cuentap.nuble.sexo.promedio$frecuencia = freq$Freq

names(tasa.cuentap.nuble.sexo.promedio) = 
  c("sexo", "tasa.cuentap.nuble.sector2.sexo", "error estandar", "cv", "frecuencia")

write.csv(tasa.cuentap.nuble.sexo.promedio, "tasa_cuentap_nuble_sexo_promedio.csv")

### FIN CUADRO 39
