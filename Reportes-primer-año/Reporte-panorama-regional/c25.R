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
### CUADRO 25: Cantidad de trabajadores de nuble según categoría ocupacional y tipo de jornada de trabajo
#-------------------------------------------------------------------------------
###################################################################
# Total de ocupados nacional por jornada laboral
###################################################################
#
total.ocupados.nacional.jornada = list()
for (i in 1:length(info)){
  total.ocupados.nacional.jornada[[i]] = svyby(~I(cae_general=="Ocupado"),
                                               by=~c1, info[[i]], svytotal,
                                               multicore = TRUE, 
                                               drop.empty.groups = FALSE, na.rm=TRUE)
}

total.ocupados.nacional.jornada. = 
  do.call(rbind, total.ocupados.nacional.jornada)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(cae_general=="Ocupado")+c1, data=info[[i]]$variables)
}
freq. = do.call(cbind, freq)[1,]

total.ocupados.nacional.jornada.$cv = 
  total.ocupados.nacional.jornada.$`se.I(cae_general == \"Ocupado\")TRUE`/total.ocupados.nacional.jornada.$`I(cae_general == \"Ocupado\")TRUE`

total.ocupados.nacional.jornada.$frecuencia = freq.

total.ocupados.nacional.jornada. = 
  total.ocupados.nacional.jornada.[,c("c1", "I(cae_general == \"Ocupado\")TRUE", "se.I(cae_general == \"Ocupado\")TRUE", "cv", "frecuencia")]

write.csv(total.ocupados.nacional.jornada., "total_ocupados_nacional_jornada.csv")

###################################################################
# Total de ocupados nuble por jornada laboral
###################################################################
#
total.ocupados.nuble.jornada = list()
for (i in 1:length(info)){
  total.ocupados.nuble.jornada[[i]] = svyby(~I(prov_e==84),
                                            by=~c1, info[[i]], svytotal,
                                            multicore = TRUE, 
                                            drop.empty.groups = FALSE, na.rm=TRUE)
}

total.ocupados.nuble.jornada. = 
  do.call(rbind, total.ocupados.nuble.jornada)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(prov_e==84)+c1, data=info[[i]]$variables)
}
freq. = do.call(cbind, freq)[2,]

total.ocupados.nuble.jornada.$cv = 
  total.ocupados.nuble.jornada.$`se.I(prov_e == 84)TRUE`/total.ocupados.nuble.jornada.$`I(prov_e == 84)TRUE`

total.ocupados.nuble.jornada.$frecuencia = freq.

total.ocupados.nuble.jornada. = 
  total.ocupados.nuble.jornada.[,c("c1", "I(prov_e == 84)TRUE", "se.I(prov_e == 84)TRUE","cv","frecuencia")]

write.csv(total.ocupados.nuble.jornada., "total_ocupados_nuble_jornada.csv")

###################################################################
# Total de ocupados nacional por categoria ocupacional
###################################################################
#
total.ocupados.nacional.categoria = list()
for (i in 1:length(info)){
  total.ocupados.nacional.categoria[[i]] = svyby(~I(cae_general=="Ocupado"),
                                                 by=~cat.ocup, info[[i]], svytotal,
                                                 multicore = TRUE, 
                                                 drop.empty.groups = FALSE, na.rm=TRUE)
}

total.ocupados.nacional.categoria. = 
  do.call(rbind, total.ocupados.nacional.categoria)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(cae_general=="Ocupado")+cat.ocup, data=info[[i]]$variables) %>% data.frame()
}

freq. = do.call(rbind, freq)

total.ocupados.nacional.categoria.$cv = 
  total.ocupados.nacional.categoria.$`se.I(cae_general == \"Ocupado\")TRUE`/total.ocupados.nacional.categoria.$`I(cae_general == \"Ocupado\")TRUE`

total.ocupados.nacional.categoria.$frecuencia = freq.$Freq

total.ocupados.nacional.categoria. = 
  total.ocupados.nacional.categoria.[,c("cat.ocup", "I(cae_general == \"Ocupado\")TRUE", "se.I(cae_general == \"Ocupado\")TRUE","cv","frecuencia")]

write.csv(total.ocupados.nacional.categoria., "total_ocupados_nacional_categoria.csv")

###################################################################
# Total de ocupados nuble por categoria ocupacional
###################################################################
#
total.ocupados.nuble.categoria = list()
for (i in 1:length(info)){
  total.ocupados.nuble.categoria[[i]] = svyby(~I(prov_e==84),
                                              by=~cat.ocup, info[[i]], svytotal,
                                              multicore = TRUE, 
                                              drop.empty.groups = FALSE, na.rm=TRUE)
}

total.ocupados.nuble.categoria. = 
  do.call(rbind, total.ocupados.nuble.categoria)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(prov_e==84)+cat.ocup, data=info[[i]]$variables) %>% data.frame() %>% 
    filter(`I.prov_e....84.`==TRUE)
}

freq. = do.call(rbind, freq)

total.ocupados.nuble.categoria.$cv = 
  total.ocupados.nuble.categoria.$`se.I(prov_e == 84)TRUE`/total.ocupados.nuble.categoria.$`I(prov_e == 84)TRUE`

total.ocupados.nuble.categoria.$frecuencia = freq.$Freq

total.ocupados.nuble.categoria. = 
  total.ocupados.nuble.categoria.[,c("cat.ocup", "I(prov_e == 84)TRUE", "se.I(prov_e == 84)TRUE","cv","frecuencia")]

write.csv(total.ocupados.nuble.categoria., "total_ocupados_nuble_categoria.csv")

###################################################################
# Total de ocupados nuble por jornada laboral y categoria ocupacional
###################################################################
#
total.ocupados.nuble.jornada.categoria = list()
for (i in 1:length(info)){
  total.ocupados.nuble.jornada.categoria[[i]] = svyby(~I(prov_e==84),
                                                      by=~cat.ocup+c1, info[[i]], 
                                                      svytotal, multicore = TRUE, 
                                                      drop.empty.groups = FALSE, na.rm=TRUE)
}

total.ocupados.nuble.jornada.categoria. = 
  do.call(rbind, total.ocupados.nuble.jornada.categoria)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(prov_e==84)+cat.ocup+c1, data=info[[i]]$variables) %>% data.frame() %>% 
    filter(`I.prov_e....84.`==TRUE)
}

freq. = do.call(rbind, freq)

total.ocupados.nuble.jornada.categoria.$cv = 
  total.ocupados.nuble.jornada.categoria.$`se.I(prov_e == 84)TRUE`/total.ocupados.nuble.jornada.categoria.$`I(prov_e == 84)TRUE`


total.ocupados.nuble.jornada.categoria.$frecuencia = freq.$Freq

total.ocupados.nuble.jornada.categoria. = 
  total.ocupados.nuble.jornada.categoria.[,c("cat.ocup", "c1", "I(prov_e == 84)TRUE", "se.I(prov_e == 84)TRUE","cv","frecuencia")]

write.csv(total.ocupados.nuble.jornada.categoria., "total_ocupados_nuble_jornada_categoria.csv")

### FIN CUADRO 25
