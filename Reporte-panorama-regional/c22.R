# CREACION DE LA VARIABLE: CONMUTANTE A NIVEL REGIONAL
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, conmutante1 =
                                 ifelse(region!=region_e,1,0))
}

#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLE: CONMUTANTE A NIVEL PROVINCIAL
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, conmutante2 =
                                 ifelse(prov!=prov_e,1,0))
}


# CREACION DE LA VARIABLE: CONMUTANTE A NIVEL NACIONAL
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, conmuta.nacional1 =
                                    ifelse(region!=1 & region_e==1,1,
                                    ifelse(region!=2 & region_e==2,1,
                                    ifelse(region!=3 & region_e==3,1,
                                    ifelse(region!=4 & region_e==4,1,                
                                    ifelse(region!=5 & region_e==5,1,
                                    ifelse(region!=6 & region_e==6,1,
                                    ifelse(region!=7 & region_e==7,1,
                                    ifelse(region!=8 & region_e==8,1,
                                    ifelse(region!=9 & region_e==9,1,
                                    ifelse(region!=10 & region_e==10,1,
                                    ifelse(region!=11 & region_e==11,1,
                                    ifelse(region!=12 & region_e==12,1,
                                    ifelse(region!=13 & region_e==13,1,
                                    ifelse(region!=14 & region_e==14,1,
                                    ifelse(region!=15 & region_e==15,1,0))))))))))))))))
}

#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLE: CONMUTANTE A NIVEL NACIONAL
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, conmuta.nacional2 =
                                    ifelse(region==1 & region_e!=1,1,
                                    ifelse(region==2 & region_e!=2,1,
                                    ifelse(region==3 & region_e!=3,1,
                                    ifelse(region==4 & region_e!=4,1,                
                                    ifelse(region==5 & region_e!=5,1,
                                    ifelse(region==6 & region_e!=6,1,
                                    ifelse(region==7 & region_e!=7,1,
                                    ifelse(region==8 & region_e!=8,1,
                                    ifelse(region==9 & region_e!=9,1,
                                    ifelse(region==10 & region_e!=10,1,
                                    ifelse(region==11 & region_e!=11,1,
                                    ifelse(region==12 & region_e!=12,1,
                                    ifelse(region==13 & region_e!=13,1,
                                    ifelse(region==14 & region_e!=14,1,
                                    ifelse(region==15 & region_e!=15,1,0))))))))))))))))
}

#-------------------------------------------------------------------------------
### CUADRO 22: Caracteristicas generales de los trabajadores residentes de nuble que trabajan en otra region 2016
#-------------------------------------------------------------------------------
###################################################################
# Edad de los trabajadores residente que trabajan fuera de Nuble
###################################################################
#
edad.promedio.residentes.trabaja.otra.nuble = list()
for (i in 1:length(info)){
  edad.promedio.residentes.trabaja.otra.nuble[[i]]=svyby(~edad,
                                                         by=~I(conmutante2==1 & prov==84), info[[i]], 
                                                         svymean, multicore = TRUE, 
                                                         drop.empty.groups = FALSE, na.rm=TRUE) %>% 
    filter( `I(conmutante2 == 1 & prov == 84)`==TRUE)
}

edad.promedio.residentes.trabaja.otra.nuble. = 
  do.call(rbind, edad.promedio.residentes.trabaja.otra.nuble)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(conmutante2==1 & prov==84), data=info[[i]]$variables) %>% data.frame() %>% 
    filter(I.conmutante2....1...prov....84.==TRUE)
}

freq = do.call(rbind, freq)

edad.promedio.residentes.trabaja.otra.nuble.$cv = 
  edad.promedio.residentes.trabaja.otra.nuble.$`se`/edad.promedio.residentes.trabaja.otra.nuble.$`edad`

edad.promedio.residentes.trabaja.otra.nuble.$frecuencia = freq$Freq

edad.promedio.residentes.trabaja.otra.nuble. = 
  edad.promedio.residentes.trabaja.otra.nuble.[, c("I(conmutante2 == 1 & prov == 84)", "edad", "se", "cv", "frecuencia")]

write.csv(edad.promedio.residentes.trabaja.otra.nuble., "edad_promedio_residentes_trabaja_otra_nuble.csv")

###################################################################
# Edad de los trabajadores residente que trabajan en otra region a nivel nacional
###################################################################
#
edad.promedio.residentes.trabaja.otra.nacional = list()
for (i in 1:length(info)){
  edad.promedio.residentes.trabaja.otra.nacional[[i]]=svyby(~edad,
                                                            by=~I(conmuta.nacional2==1), info[[i]], svymean,
                                                            multicore = TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

edad.promedio.residentes.trabaja.otra.nacional. = 
  do.call(rbind, edad.promedio.residentes.trabaja.otra.nacional)

edad.promedio.residentes.trabaja.otra.nacional. = 
  edad.promedio.residentes.trabaja.otra.nacional.[, c("I(conmuta.nacional2 == 1)", "edad", "se")]

write.csv(edad.promedio.residentes.trabaja.otra.nacional., "edad_promedio_residentes_trabaja_otra_nacional.csv")

###################################################################
# Escolaridad de los trabajadores no residentes de Nuble
###################################################################
#
esc.promedio.residentes.trabaja.otra.nuble = list()
for (i in 1:length(info)){
  esc.promedio.residentes.trabaja.otra.nuble[[i]]=svyby(~esc,
                                                        by=~I(conmutante2==1 & prov==84), info[[i]], 
                                                        svymean, multicore = TRUE, 
                                                        drop.empty.groups = FALSE, na.rm=TRUE) %>% 
    filter( `I(conmutante2 == 1 & prov == 84)`==TRUE)
}

esc.promedio.residentes.trabaja.otra.nuble. = 
  do.call(rbind, esc.promedio.residentes.trabaja.otra.nuble)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(conmutante2==1 & prov==84), data=info[[i]]$variables) %>% data.frame() %>% 
    filter(I.conmutante2....1...prov....84.==TRUE)
}

freq = do.call(rbind, freq)

esc.promedio.residentes.trabaja.otra.nuble.$cv = 
  esc.promedio.residentes.trabaja.otra.nuble.$`se`/esc.promedio.residentes.trabaja.otra.nuble.$`esc`

esc.promedio.residentes.trabaja.otra.nuble.$frecuencia = freq$Freq

esc.promedio.residentes.trabaja.otra.nuble. = 
  esc.promedio.residentes.trabaja.otra.nuble.[, c("I(conmutante2 == 1 & prov == 84)", "esc", "se", "cv", "frecuencia")]

write.csv(esc.promedio.residentes.trabaja.otra.nuble., "esc_promedio_residentes_trabaja_otra_nuble.csv")

###################################################################
# Escolaridad de los trabajadores residentes trabaja otra a nivel nacional
###################################################################
#
esc.promedio.residentes.trabaja.otra.nacional = list()
for (i in 1:length(info)){
  esc.promedio.residentes.trabaja.otra.nacional[[i]]=svyby(~esc,
                                                           by=~I(conmuta.nacional2==1), info[[i]], 
                                                           svymean, multicore = TRUE, 
                                                           drop.empty.groups = FALSE, na.rm=TRUE)
}

esc.promedio.residentes.trabaja.otra.nacional. = 
  do.call(rbind, esc.promedio.residentes.trabaja.otra.nacional)

esc.promedio.residentes.trabaja.otra.nacional. = 
  esc.promedio.residentes.trabaja.otra.nacional.[, c("I(conmuta.nacional2 == 1)", "esc", "se")]

write.csv(esc.promedio.residentes.trabaja.otra.nacional., "esc_promedio_residentes_trabaja_otra_nacional.csv")

####################################################################################
# Porcentaje de mujeres promedio residentes que trabaja en otra a nivel nacional. Se calcula sobre el total de residentes en otra
####################################################################################
#
tasa.mujeres.residente.otra.nacional = list()
for (i in 1:length(info)){
  tasa.mujeres.residente.otra.nacional[[i]] = svyratio(~I(conmuta.nacional2==1 & sexo==2), 
                                                       denominator=~conmuta.nacional2,
                                                       info[[i]],multicore = TRUE, 
                                                       drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.mujeres.residente.otra.nacional. = unlist(lapply(tasa.mujeres.residente.otra.nacional, '[[', 1) ) 

ee.tasa.mujeres.residente.otra.nacional = unlist(lapply(tasa.mujeres.residente.otra.nacional, SE))

tasa.mujeres.residente.otra.nacional.. = data.frame(tasa.mujeres.residente.otra.nacional., ee.tasa.mujeres.residente.otra.nacional)

write.csv(tasa.mujeres.residente.otra.nacional.., "tasa_promedio_mujeres_residente_trabaja_otra_nacional.csv")

####################################################################################
# Porcentaje de mujeres promedio residentes que trabaja en otra a nivel de nuble. Se calcula sobre el total de residentes en otra
####################################################################################
#
tasa.mujeres.residente.trabaja.otra.nuble = list()
for (i in 1:length(info)){
  tasa.mujeres.residente.trabaja.otra.nuble[[i]] = svyratio(~I(conmutante2==1 & prov==84 & sexo==2), 
                                                            denominator=~I(conmutante2==1 & prov==84),
                                                            info[[i]],multicore = TRUE, 
                                                            drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.mujeres.residente.trabaja.otra.nuble. = unlist(lapply(tasa.mujeres.residente.trabaja.otra.nuble, '[[', 1) ) 

ee.tasa.mujeres.residente.trabaja.otra.nuble = unlist(lapply(tasa.mujeres.residente.trabaja.otra.nuble, SE))

tasa.mujeres.residente.trabaja.otra.nuble.. = data.frame(tasa.mujeres.residente.trabaja.otra.nuble., ee.tasa.mujeres.residente.trabaja.otra.nuble)

write.csv(tasa.mujeres.residente.trabaja.otra.nuble.., "tasa_promedio_mujeres_residente_trabaja_otra_nuble.csv")

### FIN CUADRO 22
