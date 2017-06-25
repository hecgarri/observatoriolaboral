#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLE: RAZONES DE INACTIVIDAD (e9 ENE)
# 1: Cuidado de hijos o terceros               : 11,12,14
# 2: Quehaceres del hogar                      : 3
# 3: Estudiante                                : 4
# 4: Enfermedad o discapacidad                 : 8,10
# 5: Percepcion negativa del mercado laboral   : 9,15,16,17,18,19
# 6: No tiene interes                          : 21
# 7: Tiene otra fuente de ingreso              : 5,6,7
# 8: Iniciadores                               : 1,2
# 9: Otra razon                                : 13,20

for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, razon.inactivos =
                                  ifelse(e9==11 | e9==12 | e9==14,1,
                                  ifelse(e9==3,2,
                                  ifelse(e9==4,3,
                                  ifelse(e9==8 | e9==10,4,
                                  ifelse(e9==9 | e9==15 | e9==16 | e9==17 | e9==18 | e9==19,5,
                                  ifelse(e9==21,6,
                                  ifelse(e9==5 | e9==6 | e9==7,7,
                                  ifelse(e9==1 | e9==2,8,
                                  ifelse(e9==13 | e9==20,9,0))))))))))
}

#-------------------------------------------------------------------------------
### CUADRO 36: Inactivos entre 15 y 60 años, segun razones de inactividad y género de Nuble, 2016. 
#-------------------------------------------------------------------------------
################################################################################
# Inactivos por sexo y escolaridad a nivel nacional y nuble
################################################################################

# inactivos a nivel nacional, by sexo, by educ
inactivos.nacional.sexo = list()
for (i in 1:length(info)){
  inactivos.nacional.sexo[[i]] = svyby(~I(cae_general=="Inactivo"),
                                       by=~razon.inactivos+sexo, info[[i]], 
                                       svytotal, multicore== TRUE, 
                                       drop.empty.groups = FALSE, na.rm=TRUE)
}

inactivos.nacional.sexo. = 
  do.call(rbind, inactivos.nacional.sexo)

inactivos.nacional.sexo. = 
  inactivos.nacional.sexo.[,c("razon.inactivos", "sexo","I(cae_general == \"Inactivo\")TRUE","se.I(cae_general == \"Inactivo\")TRUE")]

write.csv(inactivos.nacional.sexo., "inactivos_nacional_sexo.csv")

# inactivos a nivel nuble, by sexo, by educ
inactivos.nuble.sexo = list()
for (i in 1:length(info)){
  inactivos.nuble.sexo[[i]] = svyby(~I(cae_general=="Inactivo" & prov==84),
                                    by=~razon.inactivos+sexo, info[[i]], svytotal,
                                    multicore== TRUE, drop.empty.groups = FALSE, 
                                    na.rm=TRUE)
}

inactivos.nuble.sexo. = 
  do.call(rbind, inactivos.nuble.sexo)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(cae_general=="Inactivo" & prov==84)+razon.inactivos+sexo, data=info[[i]]$variables) %>% data.frame() %>% 
    filter(`I.cae_general.....Inactivo....prov....84.`==TRUE)
}

freq = do.call(rbind, freq)

inactivos.nuble.sexo.$cv = 
  inactivos.nuble.sexo.$`se.I(cae_general == \"Inactivo\" & prov == 84)TRUE`/inactivos.nuble.sexo.$`I(cae_general == \"Inactivo\" & prov == 84)TRUE`

inactivos.nuble.sexo.$frecuencia = freq$Freq

inactivos.nuble.sexo. = 
  inactivos.nuble.sexo.[, c("razon.inactivos", "sexo","I(cae_general == \"Inactivo\" & prov == 84)TRUE","se.I(cae_general == \"Inactivo\" & prov == 84)TRUE", "cv", "frecuencia")]

write.csv(inactivos.nuble.sexo., "inactivos_nuble_sexo.csv")

### FIN CUADRO 36
