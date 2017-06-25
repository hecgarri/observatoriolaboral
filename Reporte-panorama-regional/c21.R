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
### CUADRO 21: Caracterisiticas generales de los trabajadores no residentes de nuble 2016
#-------------------------------------------------------------------------------
###################################################################
# Edad de los trabajadores no residentes de Nuble
###################################################################
#
edad.promedio.no.residentes.nuble = list()
for (i in 1:length(info)){
  edad.promedio.no.residentes.nuble[[i]]=svyby(~edad,
                                               by=~I(conmutante2==1 & prov_e==84), info[[i]], 
                                               svymean, multicore = TRUE, 
                                               drop.empty.groups = FALSE, na.rm=TRUE) %>% 
    filter( `I(conmutante2 == 1 & prov_e == 84)`==TRUE)
}

edad.promedio.no.residentes.nuble. = 
  do.call(rbind, edad.promedio.no.residentes.nuble)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(conmutante2==1 & prov_e==84), data=info[[i]]$variables) %>% data.frame() %>% 
    filter(I.conmutante2....1...prov_e....84.==TRUE)
}

freq = do.call(rbind, freq)

edad.promedio.no.residentes.nuble.$cv = 
  edad.promedio.no.residentes.nuble.$`se`/edad.promedio.no.residentes.nuble.$`edad`

edad.promedio.no.residentes.nuble.$frecuencia = freq$Freq

edad.promedio.no.residentes.nuble. = 
  edad.promedio.no.residentes.nuble.[, c("I(conmutante2 == 1 & prov_e == 84)", "edad", "se", "cv", "frecuencia")]

write.csv(edad.promedio.no.residentes.nuble., "edad_promedio_no_residentes_nuble.csv")

###################################################################
# Edad de los trabajadores no residentes a nivel nacional
###################################################################
#
edad.promedio.no.residentes.nacional = list()
for (i in 1:length(info)){
  edad.promedio.no.residentes.nacional[[i]]=svyby(~edad,
                                                  by=~I(conmuta.nacional1==1), info[[i]], svymean,
                                                  multicore = TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

edad.promedio.no.residentes.nacional. = 
  do.call(rbind, edad.promedio.no.residentes.nacional)

edad.promedio.no.residentes.nacional. = 
  edad.promedio.no.residentes.nacional.[, c("I(conmuta.nacional1 == 1)", "edad", "se")]

write.csv(edad.promedio.no.residentes.nacional., "edad_promedio_no_residentes_nacional.csv")

###################################################################
# Escolaridad de los trabajadores no residentes de Nuble
###################################################################
#
esc.promedio.no.residentes.nuble = list()
for (i in 1:length(info)){
  esc.promedio.no.residentes.nuble[[i]]=svyby(~esc,
                                              by=~I(conmutante2==1 & prov_e==84), info[[i]], svymean,
                                              multicore = TRUE, drop.empty.groups = FALSE, na.rm=TRUE) %>% 
    filter( `I(conmutante2 == 1 & prov_e == 84)`==TRUE)
}

esc.promedio.no.residentes.nuble. = 
  do.call(rbind, esc.promedio.no.residentes.nuble)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(conmutante2==1 & prov_e==84), data=info[[i]]$variables) %>% data.frame() %>% 
    filter(I.conmutante2....1...prov_e....84.==TRUE)
}

freq = do.call(rbind, freq)

esc.promedio.no.residentes.nuble.$cv = 
  esc.promedio.no.residentes.nuble.$`se`/esc.promedio.no.residentes.nuble.$`esc`

esc.promedio.no.residentes.nuble.$frecuencia = freq$Freq

esc.promedio.no.residentes.nuble. = 
  esc.promedio.no.residentes.nuble.[, c("I(conmutante2 == 1 & prov_e == 84)", "esc", "se", "cv", "frecuencia")]

write.csv(esc.promedio.no.residentes.nuble., "esc_promedio_no_residentes_nuble.csv")

###################################################################
# Escolaridad de los trabajadores no residentes a nivel nacional
###################################################################
#
esc.promedio.no.residentes.nacional = list()
for (i in 1:length(info)){
  esc.promedio.no.residentes.nacional[[i]]=svyby(~esc,
                                                 by=~I(conmuta.nacional1==1), info[[i]], svymean,
                                                 multicore = TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

esc.promedio.no.residentes.nacional. = 
  do.call(rbind, esc.promedio.no.residentes.nacional)

esc.promedio.no.residentes.nacional. = 
  esc.promedio.no.residentes.nacional.[, c("I(conmuta.nacional1 == 1)", "esc", "se")]

write.csv(esc.promedio.no.residentes.nacional., "esc_promedio_no_residentes_nacional.csv")

####################################################################################
# Porcentaje de mujeres promedio no residentes a nivel nacional. Se calcula sobre el total de no residentes
####################################################################################
#
tasa.mujeres.no.residente.nacional = list()
for (i in 1:length(info)){
  tasa.mujeres.no.residente.nacional[[i]] = svyratio(~I(conmuta.nacional1==1 & sexo==2), 
                                                     denominator=~conmuta.nacional1, info[[i]],
                                                     multicore = TRUE, drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.mujeres.no.residente.nacional. = unlist(lapply(tasa.mujeres.no.residente.nacional, '[[', 1) ) 

ee.tasa.mujeres.no.residente.nacional = unlist(lapply(tasa.mujeres.no.residente.nacional, SE))

tasa.mujeres.no.residente.nacional.. = data.frame(tasa.mujeres.no.residente.nacional., ee.tasa.mujeres.no.residente.nacional)

write.csv(tasa.mujeres.no.residente.nacional.., "tasa_promedio_mujeres_no_residente_nacional.csv")

####################################################################################
# Porcentaje de mujeres promedio de no residente a nivel nuble. Se calcula sobre el de no residentes
####################################################################################

#
tasa.mujeres.no.residente.nuble = list()
for (i in 1:length(info)){
  tasa.mujeres.no.residente.nuble[[i]] = svyratio(~I(conmutante2==1 & prov_e==84 & sexo==2), 
                                                  denominator=~I(conmutante2==1 & prov_e==84),
                                                  info[[i]],multicore = TRUE, 
                                                  drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.mujeres.no.residente.nuble. = unlist(lapply(tasa.mujeres.no.residente.nuble, '[[', 1) ) 

ee.tasa.mujeres.no.residente.nuble = unlist(lapply(tasa.mujeres.no.residente.nuble, SE))

tasa.mujeres.no.residente.nuble.. = data.frame(tasa.mujeres.no.residente.nuble., ee.tasa.mujeres.no.residente.nuble)

write.csv(tasa.mujeres.no.residente.nuble.., "tasa_promedio_mujeres_no_residente_nuble.csv")

### FIN CUADRO 21
