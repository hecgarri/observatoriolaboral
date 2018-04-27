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
### CUADRO 14: Distribucion sectorial Nuble, segun edad, 2016. 
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
# tasa de dependiente con contrato indefinido del total de ocupados en nuble by sector y  sexo
################################################################
#
tasa.dep.con.contratoindef.nuble.sector.jovenes = svyby(~I(cat.ocup=="asalariado con contrato indefinido"),
                                                         by=~sector2+I(edad >= 15 & edad <= 29), denominator=~I(cae_general=="Ocupado"), 
                                                         subset(info2,prov_e==84), svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cat.ocup=="asalariado con contrato indefinido")+sector2+I(edad >= 15 & edad <= 29), data=subset(info2$variables,prov_e==84)) %>% data.frame() %>% 
  filter(`I.cat.ocup.....asalariado.con.contrato.indefinido..`==TRUE)

tasa.dep.con.contratoindef.nuble.sector.jovenes$cv = 
  tasa.dep.con.contratoindef.nuble.sector.jovenes$`se.I(cat.ocup == \"asalariado con contrato indefinido\")/I(cae_general == \"Ocupado\")`/tasa.dep.con.contratoindef.nuble.sector.jovenes$`I(cat.ocup == \"asalariado con contrato indefinido\")/I(cae_general == \"Ocupado\")`

tasa.dep.con.contratoindef.nuble.sector.jovenes$frecuencia = freq$Freq

names(tasa.dep.con.contratoindef.nuble.sector.jovenes) = 
  c("sector", "jovenes", "tasa", "se", "cv", "frecuencia")

write.csv(tasa.dep.con.contratoindef.nuble.sector.jovenes, "tasa_dep_con_contratoindef_nuble_sector_jovenes.csv")

#
tasa.dep.con.contratoindef.nuble.jovenes.promedio = svyby(~I(cat.ocup=="asalariado con contrato indefinido"),
                                                                  by=~I(edad >= 15 & edad <= 29), denominator=~I(cae_general=="Ocupado"), 
                                                                  subset(info2,prov_e==84), svyratio, multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cat.ocup=="asalariado con contrato indefinido")+I(edad >= 15 & edad <= 29), data=subset(info2$variables,prov_e==84)) %>% data.frame() %>% 
  filter(`I.cat.ocup.....asalariado.con.contrato.indefinido..`==TRUE)

tasa.dep.con.contratoindef.nuble.jovenes.promedio$cv = 
  tasa.dep.con.contratoindef.nuble.jovenes.promedio$`se.I(cat.ocup == \"asalariado con contrato indefinido\")/I(cae_general == \"Ocupado\")`/tasa.dep.con.contratoindef.nuble.jovenes.promedio$`I(cat.ocup == \"asalariado con contrato indefinido\")/I(cae_general == \"Ocupado\")`

tasa.dep.con.contratoindef.nuble.jovenes.promedio$frecuencia = freq$Freq

names(tasa.dep.con.contratoindef.nuble.jovenes.promedio) = 
  c("jovenes", "tasa", "se", "cv", "frecuencia")

write.csv(tasa.dep.con.contratoindef.nuble.jovenes.promedio, "tasa_dep_con_contratoindef_nuble_jovenes_promedio.csv")

#
################################################################
# Escolaridad promedio de las ocupaciones a 1 digito por sexo
################################################################
#
escolaridad.promedio.ocupaciones.nuble.sector.jovenes = svyby(~esc,
                                                               by=~sector2+I(edad >= 15 & edad <= 29), subset(info2,prov_e==84), svymean, 
                                                               multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~sector2+I(edad >= 15 & edad <= 29), data=subset(info2$variables,prov_e==84)) %>% data.frame()

escolaridad.promedio.ocupaciones.nuble.sector.jovenes$cv = 
  escolaridad.promedio.ocupaciones.nuble.sector.jovenes$`se`/escolaridad.promedio.ocupaciones.nuble.sector.jovenes$`esc`

escolaridad.promedio.ocupaciones.nuble.sector.jovenes$frecuencia = freq$Freq

names(escolaridad.promedio.ocupaciones.nuble.sector.jovenes) = 
  c("sector2", "jovenes", "esc", "se", "cv", "frecuencia")

write.csv(escolaridad.promedio.ocupaciones.nuble.sector.jovenes, "escolaridad_promedio_ocupaciones_nuble_sector_jovenes.csv")

#
escolaridad.promedio.ocupaciones.nuble.jovenes = svyby(~esc,
                                                       by=~I(edad >= 15 & edad <= 29), subset(info2,prov_e==84), svymean, 
                                                       multicore=TRUE, drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(edad >= 15 & edad <= 29), data=subset(info2$variables,prov_e==84)) %>% data.frame()

escolaridad.promedio.ocupaciones.nuble.jovenes$cv = 
  escolaridad.promedio.ocupaciones.nuble.jovenes$`se`/escolaridad.promedio.ocupaciones.nuble.jovenes$`esc`

escolaridad.promedio.ocupaciones.nuble.jovenes$frecuencia = freq$Freq

names(escolaridad.promedio.ocupaciones.nuble.jovenes) = 
  c("jovenes", "esc", "se", "cv", "frecuencia")

write.csv(escolaridad.promedio.ocupaciones.nuble.jovenes, "escolaridad_promedio_ocupaciones_nuble_jovenes.csv")

### FIN CUADRO 11
