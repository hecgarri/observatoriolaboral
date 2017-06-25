#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLE: TAMANO DE EMPRESA Y UNIPERSONALES
#
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, tipo.empresa = 
                              ifelse(b15_1==3,4, 
                              ifelse(b15_1==4,5,
                              ifelse(b15_1==5,6,
                              ifelse(b15_1==1 & b15_2==1 & categoria_ocupacion==2,1,
                              ifelse((b15_1==1 & categoria_ocupacion!=2 & b15_2==1) | 
                                      (b15_1==1 & b15_2>1 & b15_2!=10),2,
                              ifelse(b15_1==2 | b15_2==10,3,NA)))))))
}

# (1:Unipersonal; 2:Micro; 3:Pequena; 4:Mediana; 5:Grande)
for (i in 1:length(info)){
  info[[i]]$variables$tipo.empresa = recode(info[[i]]$variables$tipo.empresa, `1`= 1, 
                                   `2`= 2, `3` =2, `4`=3, `5`=4, `6`=5, 
                                   .default = 0, .missing = 99)
}

for (i in 1:length(info)){
  info[[i]]$variables$tipo.empresa = factor(info[[i]]$variables$tipo.empresa, 
                                   levels=c(1,2,3,4,5),
                                   labels=c("Unipersonal", "Micro", "Pequena", 
                                            "Mediana", "Grande"))
}

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
### CUADRO 58: ocupados en nuble por tamano empresa y nivel educacional (con efecto conmutacion)
#-------------------------------------------------------------------------------
#
total.ocupados.nuble.empresa.educ = list()
for (i in 1:length(info)){
  total.ocupados.nuble.empresa.educ[[i]] = svyby(~I(prov_e==84),
                                                 by=~tipo.empresa+educ2, info[[i]], svytotal,
                                                 multicore = TRUE, 
                                                 drop.empty.groups = FALSE, na.rm=TRUE)
}

total.ocupados.nuble.empresa.educ. = 
  do.call(rbind, total.ocupados.nuble.empresa.educ)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(prov_e==84)+tipo.empresa+educ2, data=info[[i]]$variables)  %>% data.frame() %>% 
    filter(`I.prov_e....84.`==TRUE) 
}

freq. = do.call(rbind, freq)

total.ocupados.nuble.empresa.educ.$cv = 
  total.ocupados.nuble.empresa.educ.$`se.I(prov_e == 84)TRUE`/total.ocupados.nuble.empresa.educ.$`I(prov_e == 84)TRUE`

total.ocupados.nuble.empresa.educ.$frecuencia = freq.$Freq

total.ocupados.nuble.empresa.educ. = 
  total.ocupados.nuble.empresa.educ.[,c("tipo.empresa","educ2", "I(prov_e == 84)TRUE", "se.I(prov_e == 84)TRUE", "cv", "frecuencia")]

write.csv(total.ocupados.nuble.empresa.educ., "total_ocupados_nuble_tempresa_educ.csv")

# a nivel nacional
total.ocupados.nacional.empresa = list()
for (i in 1:length(info)){
  total.ocupados.nacional.empresa[[i]]=svyby(~I(cae_general=="Ocupado"),
                                             by=~tipo.empresa, info[[i]], 
                                             svytotal, multicore = TRUE, 
                                             drop.empty.groups = FALSE, na.rm=TRUE)
}

total.ocupados.nacional.empresa. = 
  do.call(rbind, total.ocupados.nacional.empresa)

total.ocupados.nacional.empresa. = 
  total.ocupados.nacional.empresa.[, c("tipo.empresa", "I(cae_general == \"Ocupado\")TRUE", "se.I(cae_general == \"Ocupado\")TRUE")]

write.csv(total.ocupados.nacional.empresa., "total_ocupados_nacional_tempresa.csv")

#-------------------------------------------------------------------------------
### : ocupados en nuble por tamano empresa y categoria ocupacional (con efecto conmutacion)
#-------------------------------------------------------------------------------
#
total.ocupados.nuble.empresa.cat.ocup = list()
for (i in 1:length(info)){
  total.ocupados.nuble.empresa.cat.ocup[[i]] = svyby(~I(prov_e==84),
                                                     by=~cat.ocup+tipo.empresa, info[[i]], svytotal,
                                                     multicore = TRUE, 
                                                     drop.empty.groups = FALSE, na.rm=TRUE)
}

total.ocupados.nuble.empresa.cat.ocup. = 
  do.call(rbind, total.ocupados.nuble.empresa.cat.ocup)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(prov_e==84)+cat.ocup+tipo.empresa, data=info[[i]]$variables)  %>% data.frame() %>% 
    filter(`I.prov_e....84.`==TRUE) 
}

freq. = do.call(rbind, freq)

total.ocupados.nuble.empresa.cat.ocup.$cv = 
  total.ocupados.nuble.empresa.cat.ocup.$`se.I(prov_e == 84)TRUE`/total.ocupados.nuble.empresa.cat.ocup.$`I(prov_e == 84)TRUE`

total.ocupados.nuble.empresa.cat.ocup.$frecuencia = freq.$Freq

total.ocupados.nuble.empresa.cat.ocup. = 
  total.ocupados.nuble.empresa.cat.ocup.[,c("cat.ocup", "tipo.empresa", "I(prov_e == 84)TRUE", "se.I(prov_e == 84)TRUE", "cv", "frecuencia")]

write.csv(total.ocupados.nuble.empresa.cat.ocup., "total_ocupados_nuble_tempresa_cat_ocup.csv")

# a nivel nacional
total.ocupados.nacional.cat.ocup = list()
for (i in 1:length(info)){
  total.ocupados.nacional.cat.ocup[[i]]=svyby(~I(cae_general=="Ocupado"),
                                              by=~cat.ocup, info[[i]], 
                                              svytotal, multicore = TRUE, 
                                              drop.empty.groups = FALSE, na.rm=TRUE)
}

total.ocupados.nacional.cat.ocup. = 
  do.call(rbind, total.ocupados.nacional.cat.ocup)

total.ocupados.nacional.cat.ocup. = 
  total.ocupados.nacional.cat.ocup.[, c("cat.ocup", "I(cae_general == \"Ocupado\")TRUE", "se.I(cae_general == \"Ocupado\")TRUE")]

write.csv(total.ocupados.nacional.cat.ocup., "total_ocupados_nacional_tempresa.csv")

### FIN CUADRO 56
