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

###################################################################
# CUADRO 6: Distribucion sectorial de Nuble, segun genero, 2015
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
# tasa de dependiente con contrato indefinido del total de ocupados en nuble by sector y  sexo
################################################################
#
tasa.dep.con.contratoindef.nuble.sector.sexo = svyby(~I(cat.ocup=="asalariado con contrato indefinido" & prov_e==84),
                                                 by=~sector2+sexo, denominator=~I(cae_general=="Ocupado" & prov_e==84), 
                                                 info2, svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cat.ocup=="asalariado con contrato indefinido" & prov_e==84)+sector2+sexo, data=info2$variables) %>% data.frame() %>% 
  filter(`I.cat.ocup.....asalariado.con.contrato.indefinido....prov_e....84.`==TRUE)

tasa.dep.con.contratoindef.nuble.sector.sexo$cv = 
  tasa.dep.con.contratoindef.nuble.sector.sexo$`se.I(cat.ocup == \"asalariado con contrato indefinido\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`/tasa.dep.con.contratoindef.nuble.sector.sexo$`I(cat.ocup == \"asalariado con contrato indefinido\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`

tasa.dep.con.contratoindef.nuble.sector.sexo$frecuencia = freq$Freq

names(tasa.dep.con.contratoindef.nuble.sector.sexo) = 
  c("sector2", "sexo", "I(cat.ocup == \"asalariado con contrato indefinido\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)", "se.I(cat.ocup == \"asalariado con contrato indefinido\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)", "cv", "frecuencia")

write.csv(tasa.dep.con.contratoindef.nuble.sector.sexo, "tasa_dep_con_contratoindef_nuble_sector_sexo.csv")

#
tasa.dep.con.contratoindef.nuble.sector.sexo.promedio = svyby(~I(cat.ocup=="asalariado con contrato indefinido" & prov_e==84),
                                                      by=~sexo, denominator=~I(cae_general=="Ocupado" & prov_e==84), 
                                                      info2, svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cat.ocup=="asalariado con contrato indefinido" & prov_e==84)+sexo, data=info2$variables) %>% data.frame() %>% 
  filter(`I.cat.ocup.....asalariado.con.contrato.indefinido....prov_e....84.`==TRUE)

tasa.dep.con.contratoindef.nuble.sector.sexo.promedio$cv = 
  tasa.dep.con.contratoindef.nuble.sector.sexo.promedio$`se.I(cat.ocup == \"asalariado con contrato indefinido\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`/tasa.dep.con.contratoindef.nuble.sector.sexo.promedio$`I(cat.ocup == \"asalariado con contrato indefinido\" & prov_e == 84)/I(cae_general == \"Ocupado\" & prov_e == 84)`

tasa.dep.con.contratoindef.nuble.sector.sexo.promedio$frecuencia = freq$Freq

names(tasa.dep.con.contratoindef.nuble.sector.sexo.promedio) = 
  c("sexo", "tasa.dep.con.contratoindef.nuble.1digito.sexo.promedio", "se", "cv", "frecuencia")

write.csv(tasa.dep.con.contratoindef.nuble.sector.sexo.promedio, "tasa_dep_con_contratoindef_nuble_sector_sexo_promedio.csv")

#
################################################################
# Escolaridad promedio de las ocupaciones a 1 digito por sexo
################################################################
#
escolaridad.promedio.ocupaciones.nuble.sector.sexo = svyby(~esc,
                                                      by=~sector2+sexo, subset(info2,prov_e==84), svymean, 
                                                      multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~sector2+sexo, data=subset(info2$variables,prov_e==84)) %>% data.frame()

escolaridad.promedio.ocupaciones.nuble.sector.sexo$cv = 
  escolaridad.promedio.ocupaciones.nuble.sector.sexo$`se`/escolaridad.promedio.ocupaciones.nuble.sector.sexo$`esc`

escolaridad.promedio.ocupaciones.nuble.sector.sexo$frecuencia = freq$Freq

names(escolaridad.promedio.ocupaciones.nuble.sector.sexo) = 
  c("sector2", "sexo", "esc", "se", "cv", "frecuencia")

write.csv(escolaridad.promedio.ocupaciones.nuble.sector.sexo, "escolaridad_promedio_ocupaciones_nuble_sector2_sexo.csv")

#
escolaridad.promedio.ocupaciones.nuble.sexo = svyby(~esc,
                                         by=~sexo, subset(info2,prov_e==84), svymean, 
                                         multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~sexo, data=subset(info2$variables,prov_e==84)) %>% data.frame()

escolaridad.promedio.ocupaciones.nuble.sexo$cv = 
  escolaridad.promedio.ocupaciones.nuble.sexo$`se`/escolaridad.promedio.ocupaciones.nuble.sexo$`esc`

escolaridad.promedio.ocupaciones.nuble.sexo$frecuencia = freq$Freq

names(escolaridad.promedio.ocupaciones.nuble.sexo) = 
  c("sexo", "esc", "se", "cv", "frecuencia")

write.csv(escolaridad.promedio.ocupaciones.nuble.sexo, "escolaridad_promedio_ocupaciones_nuble_sexo.csv")

### FIN CUADRO 6
