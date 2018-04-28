#-------------------------------------------------------------------------------
### CUADRO 18: Caracterisiticas generales de los ocupados en nuble 2016
#-------------------------------------------------------------------------------
####################################################################################
# edad promedio de los ocupados a nivel nacional
####################################################################################
#
edad.promedio.ocupados.nacional = list()
for (i in 1:length(info)){
  edad.promedio.ocupados.nacional[[i]]=svyby(~edad,
                                             by=~I(cae_general=="Ocupado"), info[[i]], 
                                             svymean, multicore = TRUE, 
                                             drop.empty.groups = FALSE, na.rm=TRUE)
}

edad.promedio.ocupados.nacional. = 
  do.call(rbind, edad.promedio.ocupados.nacional)

edad.promedio.ocupados.nacional. = 
  edad.promedio.ocupados.nacional.[, c("I(cae_general == \"Ocupado\")", "edad", "se")]

write.csv(edad.promedio.ocupados.nacional., "edad_promedio_ocupados_nacional.csv")

#
edad.promedio.ocupados.nuble = list()
for (i in 1:length(info)){
  edad.promedio.ocupados.nuble[[i]]=svyby(~edad,
                                          by=~I(prov_e==84), info[[i]], 
                                          svymean, multicore = TRUE, 
                                          drop.empty.groups = FALSE, na.rm=TRUE)
}

edad.promedio.ocupados.nuble. = 
  do.call(rbind, edad.promedio.ocupados.nuble)

edad.promedio.ocupados.nuble. = 
  edad.promedio.ocupados.nuble.[, c("I(prov_e == 84)", "edad", "se")]

write.csv(edad.promedio.ocupados.nuble., "edad_promedio_ocupados_nuble.csv")

####################################################################################
# escolaridad promedio de los ocupados a nivel nacional
####################################################################################
#
esc.promedio.ocupados.nacional = list()
for (i in 1:length(info)){
  esc.promedio.ocupados.nacional[[i]]=svyby(~esc,
                                            by=~I(cae_general=="Ocupado"), info[[i]], 
                                            svymean, multicore = TRUE, 
                                            drop.empty.groups = FALSE, na.rm=TRUE)
}

esc.promedio.ocupados.nacional. = 
  do.call(rbind, esc.promedio.ocupados.nacional)

esc.promedio.ocupados.nacional. = 
  esc.promedio.ocupados.nacional.[, c("I(cae_general == \"Ocupado\")", "esc", "se")]

write.csv(esc.promedio.ocupados.nacional., "esc_promedio_ocupados_nacional.csv")

#
esc.promedio.ocupados.nuble = list()
for (i in 1:length(info)){
  esc.promedio.ocupados.nuble[[i]]=svyby(~esc,
                                         by=~I(prov_e==84), info[[i]], 
                                         svymean, multicore = TRUE, 
                                         drop.empty.groups = FALSE, na.rm=TRUE)
}

esc.promedio.ocupados.nuble. = 
  do.call(rbind, esc.promedio.ocupados.nuble)

esc.promedio.ocupados.nuble. = 
  esc.promedio.ocupados.nuble.[, c("I(prov_e == 84)", "esc", "se")]

write.csv(esc.promedio.ocupados.nuble., "esc_promedio_ocupados_nuble.csv")

####################################################################################
# Porcentaje de mujeres promedio de los ocupados a nivel nacional. Se calcula sobre el total de ocupados
####################################################################################
#
tasa.mujeres.ocupados.nacional = list()
for (i in 1:length(info)){
  tasa.mujeres.ocupados.nacional[[i]] = svyratio(~I(cae_general=="Ocupado" & sexo==2), 
                                                 denominator=~I(cae_general=="Ocupado"),
                                                 info[[i]],multicore = TRUE, 
                                                 drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.mujeres.ocupados.nacional. = unlist(lapply(tasa.mujeres.ocupados.nacional, '[[', 1) ) 

ee.tasa.mujeres.ocupados.nacional = unlist(lapply(tasa.mujeres.ocupados.nacional, SE))

tasa.mujeres.ocupados.nacional.. = data.frame(tasa.mujeres.ocupados.nacional., ee.tasa.mujeres.ocupados.nacional)

write.csv(tasa.mujeres.ocupados.nacional.., "tasa_promedio_mujeres_nacional.csv")

#
tasa.mujeres.ocupados.nuble = list()
for (i in 1:length(info)){
  tasa.mujeres.ocupados.nuble[[i]] = svyratio(~I(prov_e==84 & sexo==2), 
                                              denominator=~I(prov_e==84),
                                              info[[i]],multicore = TRUE, 
                                              drop.empty.groups = FALSE, 
                                              na.rm=TRUE)
}

tasa.mujeres.ocupados.nuble. = unlist(lapply(tasa.mujeres.ocupados.nuble, '[[', 1) ) 

ee.tasa.mujeres.ocupados.nuble = unlist(lapply(tasa.mujeres.ocupados.nuble, SE))

tasa.mujeres.ocupados.nuble.. = data.frame(tasa.mujeres.ocupados.nuble., ee.tasa.mujeres.ocupados.nuble)

write.csv(tasa.mujeres.ocupados.nuble.., "tasa_promedio_mujeres_nuble.csv")

### FIN CUADRO 18

