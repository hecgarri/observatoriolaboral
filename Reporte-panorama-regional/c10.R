#-------------------------------------------------------------------------------
### CUADRO 10: Caracteristicas generales de los ocupados en nuble segun sector
#-------------------------------------------------------------------------------
###################################################################
## Numero de ocupados por sector en nuble
###################################################################
# 
# Residentes por sector en nuble
trabajan.nuble.sector = list()
for (i in 1:length(info)){
  trabajan.nuble.sector[[i]] = svyby(~I(prov_e==84),
                                     by=~sector2, info[[i]], svytotal,
                                     multicore = TRUE, 
                                     drop.empty.groups = FALSE, na.rm=TRUE)
}

trabajan.nuble.sector. = 
  do.call(rbind, trabajan.nuble.sector)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(prov_e==84)+sector2, data=info[[i]]$variables)
}
freq. = do.call(cbind, freq)[2,]

trabajan.nuble.sector.$cv = 
  trabajan.nuble.sector.$`se.I(prov_e == 84)TRUE`/trabajan.nuble.sector.$`I(prov_e == 84)TRUE`

trabajan.nuble.sector.$frecuencia = freq.

trabajan.nuble.sector. = 
  trabajan.nuble.sector.[,c("sector2", "I(prov_e == 84)TRUE", "se.I(prov_e == 84)TRUE","cv","frecuencia")]

write.csv(trabajan.nuble.sector., "ocupados_nuble_sector_conmutacion.csv")

###################################################################
# Edad promedio de los ocupados en Nuble
###################################################################
#
edad.ocupados.nuble.sector=list()
for (i in 1:length(info)){
  edad.ocupados.nuble.sector[[i]]=svyby(~edad,
                                        by=~sector2+I(prov_e==84), info[[i]], 
                                        svymean, multicore = TRUE, 
                                        drop.empty.groups = FALSE, na.rm=TRUE) %>% 
    filter( `I(prov_e == 84)`==TRUE)
}

edad.ocupados.nuble.sector. = 
  do.call(rbind, edad.ocupados.nuble.sector)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~sector2+I(prov_e==84), data=info[[i]]$variables) %>% data.frame() %>% 
    filter(I.prov_e....84.==TRUE)
}

freq = do.call(rbind, freq)

edad.ocupados.nuble.sector.$cv = 
  edad.ocupados.nuble.sector.$`se`/edad.ocupados.nuble.sector.$`edad`

edad.ocupados.nuble.sector.$frecuencia = freq$Freq

edad.ocupados.nuble.sector. = 
  edad.ocupados.nuble.sector.[, c("sector2", "I(prov_e == 84)", "edad", "se", "cv", "frecuencia")]

write.csv(edad.ocupados.nuble.sector., "edad_ocupados_nuble_sector.csv")

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

###################################################################
# Escolaridad promedio de los ocupados en Nuble
###################################################################
#
esc.ocupados.nuble.sector=list()
for (i in 1:length(info)){
  esc.ocupados.nuble.sector[[i]]=svyby(~esc,
                                       by=~sector2+I(prov_e==84), info[[i]], 
                                       svymean, multicore = TRUE, 
                                       drop.empty.groups = FALSE, na.rm=TRUE) %>% 
    filter( `I(prov_e == 84)`==TRUE)
}

esc.ocupados.nuble.sector. = 
  do.call(rbind, esc.ocupados.nuble.sector)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~sector2+I(prov_e==84), data=info[[i]]$variables) %>% data.frame() %>% 
    filter(I.prov_e....84.==TRUE)
}

freq = do.call(rbind, freq)

esc.ocupados.nuble.sector.$cv = 
  esc.ocupados.nuble.sector.$`se`/esc.ocupados.nuble.sector.$`esc`

esc.ocupados.nuble.sector.$frecuencia = freq$Freq

esc.ocupados.nuble.sector. = 
  esc.ocupados.nuble.sector.[, c("sector2", "I(prov_e == 84)", "esc", "se", "cv", "frecuencia")]

write.csv(esc.ocupados.nuble.sector., "esc_ocupados_nuble_sector.csv")

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

###################################################################
# Porcentaje de mujeres ocupadas en nuble
###################################################################
#
tasa.mujeres.ocupados.nuble.sector = svyby(~I(prov_e==84 & sexo==2), by=~sector2,
                                           denominator=~I(prov_e==84),info2, svyratio,
                                           multicore = TRUE, drop.empty.groups = FALSE, 
                                           na.rm=TRUE)

freq= xtabs(~I(prov_e==84 & sexo==2)+sector2, data=info2$variables)[2,]

tasa.mujeres.ocupados.nuble.sector$cv = 
  tasa.mujeres.ocupados.nuble.sector$`se.I(prov_e == 84 & sexo == 2)/I(prov_e == 84)`/tasa.mujeres.ocupados.nuble.sector$`I(prov_e == 84 & sexo == 2)/I(prov_e == 84)`

tasa.mujeres.ocupados.nuble.sector$frecuencia = freq

names(tasa.mujeres.ocupados.nuble.sector) = 
  c("sector", "tasa.mujeres.ocupados.nuble.sector", "error estandar","cv","frecuencia")

write.csv(tasa.mujeres.ocupados.nuble.sector, "tasa_mujeres_ocupados_nuble_sector.csv")

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

### FIN CUADRO 10
