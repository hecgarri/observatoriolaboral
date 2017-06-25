#-------------------------------------------------------------------------------
### CUADRO 2: Escolaridad, edad e ingreso promedio de la población ocupada de Nuble segun genero, 2016.  
#-------------------------------------------------------------------------------
# edad promedio total de los ocupados a nivel nacional
edad.promedio.ocupados.nacional = list()
for (i in 1:length(info)){
  edad.promedio.ocupados.nacional[[i]] = svyby(~edad, info[[i]],
                                               by=~I(cae_general=="Ocupado"), 
                                               svymean, multicore = TRUE, 
                                               drop.empty.groups = FALSE, na.rm=TRUE)
}

edad.promedio.ocupados.nacional. = 
  do.call(rbind, edad.promedio.ocupados.nacional)

names(edad.promedio.ocupados.nacional.) = 
  c("ocup", "edad.promedio.ocupados.nacional", "error estandar")

write.csv(edad.promedio.ocupados.nacional., "edad_promedio_ocupados_nacional.csv")

# Edad promedio de los ocupados a nivel nacional by sexo
edad.promedio.ocupados.nacional.sexo =list()
for (i in 1:length(info)){
  edad.promedio.ocupados.nacional.sexo[[i]]=svyby(~edad,
                                                  by=~sexo+I(cae_general=="Ocupado"), 
                                                  info[[i]], svymean, multicore = TRUE, 
                                                  drop.empty.groups = FALSE, na.rm=TRUE)
}

edad.promedio.ocupados.nacional.sexo. = 
  do.call(rbind, edad.promedio.ocupados.nacional.sexo)

names(edad.promedio.ocupados.nacional.sexo.) = 
  c("sexo","ocup", "edad.promedio.ocupados.nuble.sexo", "error estandar")

write.csv(edad.promedio.ocupados.nacional.sexo., "edad_promedio_ocupados_nacional_sexo.csv")

# edad promedio total de los ocupados en nuble
edad.promedio.ocupados.nuble =list()
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

# Edad promedio de los ocupados de nuble by sexo
edad.promedio.ocupados.nuble.sexo =list()
for (i in 1:length(info)){
  edad.promedio.ocupados.nuble.sexo[[i]]=svyby(~edad,
                                               by=~I(prov_e==84)+sexo, info[[i]], 
                                               svymean, multicore = TRUE, 
                                               drop.empty.groups = FALSE, na.rm=TRUE)
}

edad.promedio.ocupados.nuble.sexo. = 
  do.call(rbind, edad.promedio.ocupados.nuble.sexo)

edad.promedio.ocupados.nuble.sexo. = 
  edad.promedio.ocupados.nuble.sexo.[, c("I(prov_e == 84)", "sexo", "edad", "se")]

write.csv(edad.promedio.ocupados.nuble.sexo., "edad_promedio_ocupados_nuble_sexo.csv")

# Escolaridad promedio total de los ocupados a nivel nacional
esc.promedio.ocupados.nacional = list()
for (i in 1:length(info)){
  esc.promedio.ocupados.nacional[[i]] = svyby(~esc, info[[i]],
                                              by=~I(cae_general=="Ocupado"), 
                                              svymean, multicore = TRUE, 
                                              drop.empty.groups = FALSE, na.rm=TRUE)
}

esc.promedio.ocupados.nacional. = 
  do.call(rbind, esc.promedio.ocupados.nacional)

names(esc.promedio.ocupados.nacional.) = 
  c("ocup", "esc.promedio.ocupados.nacional", "error estandar")

write.csv(esc.promedio.ocupados.nacional., "esc_promedio_ocupados_nacional.csv")

# Escolaridad promedio de los ocupados a nivel nacional
esc.promedio.ocupados.nacional.sexo =list()
for (i in 1:length(info)){
  esc.promedio.ocupados.nacional.sexo[[i]]=svyby(~esc,
                                                 by=~sexo+I(cae_general=="Ocupado"), 
                                                 info[[i]], svymean, multicore = TRUE, 
                                                 drop.empty.groups = FALSE, na.rm=TRUE)
}

esc.promedio.ocupados.nacional.sexo. = 
  do.call(rbind, esc.promedio.ocupados.nacional.sexo)

names(esc.promedio.ocupados.nacional.sexo.) = 
  c("sexo", "ocup","esc.promedio.ocupados.nacional.sexo", "error estandar")

write.csv(esc.promedio.ocupados.nacional.sexo., "esc_promedio_ocupados_nacional_sexo.csv")

# Escolaridad promedio total de los ocupados en nuble
esc.promedio.ocupados.nuble =list()
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

# Escolaridad promedio de los ocupados de nuble by sexo
esc.promedio.ocupados.nuble.sexo =list()
for (i in 1:length(info)){
  esc.promedio.ocupados.nuble.sexo[[i]]=svyby(~esc,
                                              by=~sexo+I(prov_e==84), info[[i]], 
                                              svymean, multicore = TRUE, 
                                              drop.empty.groups = FALSE, na.rm=TRUE)
}

esc.promedio.ocupados.nuble.sexo. = 
  do.call(rbind, esc.promedio.ocupados.nuble.sexo)

esc.promedio.ocupados.nuble.sexo. = 
  esc.promedio.ocupados.nuble.sexo.[, c("sexo", "I(prov_e == 84)", "esc", "se")]

write.csv(esc.promedio.ocupados.nuble.sexo., "esc_promedio_ocupados_nuble_sexo.csv")

# Porcentaje de los ocupados con educación superior completa NACIONAL
tasa.ocupados.profesional.nacional = svyby(~I(cae_general=="Ocupado" & educ>=7),
                                      by=~sexo, denominator=~I(cae_general=="Ocupado"), 
                                      info2, svyratio, multicore = TRUE, 
                                      drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(cae_general=="Ocupado" & educ>=7)+sexo, data=info2$variables)[2,]

tasa.ocupados.profesional.nacional$cv = 
  tasa.ocupados.profesional.nacional$`se.I(cae_general == \"Ocupado\" & educ >= 7)/I(cae_general == \"Ocupado\")`/tasa.ocupados.profesional.nacional$`I(cae_general == \"Ocupado\" & educ >= 7)/I(cae_general == \"Ocupado\")`

tasa.ocupados.profesional.nacional$frecuencia = freq

names(tasa.ocupados.profesional.nacional) = 
  c("Jornada", "tasa.mujeres.jornada.nacional", "error estandar","cv","frecuencia")

write.csv(tasa.ocupados.profesional.nacional, "tasa_ocupados_profesional_nacional.csv")

#
tasa.promedio.ocupados.profesional.nacional = list()
for (i in 1:length(info)){
  tasa.promedio.ocupados.profesional.nacional[[i]] = svyratio(~I(cae_general=="Ocupado" & educ>=7), 
                                                         denominator=~I(cae_general=="Ocupado"), 
                                                         info[[i]], multicore = TRUE, 
                                                         drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.ocupados.profesional.nacional. = 
  do.call(rbind, tasa.promedio.ocupados.profesional.nacional)

names(tasa.promedio.ocupados.profesional.nacional.) = 
  c("tasa.promedio.ocupados.profesional.nacional", "error estandar")

write.csv(tasa.promedio.ocupados.profesional.nacional., "tasa_promedio_ocupados_profesional_nacional.csv")


# Porcentaje de los ocupados con educación superior completa NUBLE
tasa.ocupados.profesional.nuble = svyby(~I(prov_e==84 & educ>=7),
                                           by=~sexo, denominator=~I(prov_e==84), 
                                           info2, svyratio, multicore = TRUE, 
                                           drop.empty.groups = FALSE, na.rm=TRUE)

freq= xtabs(~I(prov_e==84 & educ>=7)+sexo, data=info2$variables)[2,]

tasa.ocupados.profesional.nuble$cv = 
  tasa.ocupados.profesional.nuble$`se.I(prov_e == 84 & educ >= 7)/I(prov_e == 84)`/tasa.ocupados.profesional.nuble$`I(prov_e == 84 & educ >= 7)/I(prov_e == 84)`

tasa.ocupados.profesional.nuble$frecuencia = freq

names(tasa.ocupados.profesional.nuble) = 
  c("sexo", "tasa.ocupados.profesional.nuble", "error estandar","cv","frecuencia")

write.csv(tasa.ocupados.profesional.nuble, "tasa_ocupados_profesional_nuble.csv")

#
tasa.promedio.ocupados.profesional.nuble = list()
for (i in 1:length(info)){
  tasa.promedio.ocupados.profesional.nuble[[i]] = svyratio(~I(prov_e==84 & educ>=7), 
                                                              denominator=~I(prov_e==84), 
                                                              info[[i]], multicore = TRUE, 
                                                              drop.empty.groups = FALSE, na.rm=TRUE)
}

tasa.promedio.ocupados.profesional.nuble. = 
  do.call(rbind, tasa.promedio.ocupados.profesional.nuble)

names(tasa.promedio.ocupados.profesional.nuble.) = 
  c("tasa.promedio.ocupados.profesional.nacional", "error estandar")

write.csv(tasa.promedio.ocupados.profesional.nuble., "tasa_promedio_ocupados_profesional_nuble.csv")

### FIN CUADRO 2
