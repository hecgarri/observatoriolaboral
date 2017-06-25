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

#-------------------------------------------------------------------------------
### CUADRO 56: NUmero de empresas y ocupados de la Provincia de Ñuble según tamaño de empresas, 2016
#-------------------------------------------------------------------------------
###################################################################
# Total de ocupados nuble por tamano de empresa
###################################################################
#
total.ocupados.nuble.empresa = list()
for (i in 1:length(info)){
  total.ocupados.nuble.empresa[[i]] = svyby(~I(prov_e==84),
                                            by=~tipo.empresa, info[[i]], svytotal,
                                            multicore = TRUE, 
                                            drop.empty.groups = FALSE, na.rm=TRUE)
}

total.ocupados.nuble.empresa. = 
  do.call(rbind, total.ocupados.nuble.empresa)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(prov_e==84)+tipo.empresa, data=info[[i]]$variables)
}

freq. = do.call(cbind, freq)[1,]

total.ocupados.nuble.empresa.$cv = 
  total.ocupados.nuble.empresa.$`se.I(prov_e == 84)TRUE`/total.ocupados.nuble.empresa.$`I(prov_e == 84)TRUE`

total.ocupados.nuble.empresa.$frecuencia = freq.

total.ocupados.nuble.empresa. = 
  total.ocupados.nuble.empresa.[,c("tipo.empresa", "I(prov_e == 84)TRUE", "se.I(prov_e == 84)TRUE","cv","frecuencia")]

write.csv(total.ocupados.nuble.empresa., "total_ocupados_nuble_empresa.csv")

###################################################################
# Total de ocupados nacional por tamano de empresa
###################################################################
#
total.ocupados.nacional.empresa = list()
for (i in 1:length(info)){
  total.ocupados.nacional.empresa[[i]] = svyby(~I(cae_general=="Ocupado"),
                                               by=~tipo.empresa, info[[i]], svytotal,
                                               multicore = TRUE, 
                                               drop.empty.groups = FALSE, na.rm=TRUE)
}

total.ocupados.nacional.empresa. = 
  do.call(rbind, total.ocupados.nacional.empresa)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(cae_general=="Ocupado")+tipo.empresa, data=info[[i]]$variables)
}

freq. = do.call(cbind, freq)[1,]

total.ocupados.nacional.empresa.$cv = 
  total.ocupados.nacional.empresa.$`se.I(cae_general == \"Ocupado\")TRUE`/total.ocupados.nacional.empresa.$`I(cae_general == \"Ocupado\")TRUE`

total.ocupados.nacional.empresa.$frecuencia = freq.

total.ocupados.nacional.empresa. = 
  total.ocupados.nacional.empresa.[,c("tipo.empresa", "I(cae_general == \"Ocupado\")TRUE", "se.I(cae_general == \"Ocupado\")TRUE","cv","frecuencia")]

write.csv(total.ocupados.nacional.empresa., "total_ocupados_nacional_empresa.csv")

### FIN CUADRO 56