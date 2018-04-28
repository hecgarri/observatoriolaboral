# CREACION DE LA VARIABLE: TRAMOS ETARIOS
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, tramos_etarios = 
                          ifelse(edad>=15 & edad<=25,"15-25", 
                          ifelse(edad>=26 & edad<=35,"25-35",
                          ifelse(edad>=36 & edad<=45,"36-45",
                          ifelse(edad>=46 & edad<=55,"46-55",
                          ifelse(edad>=56 & edad<=65,"56-65",
                          ifelse(edad>65,"65 o mas",NA)))))))
}

#-------------------------------------------------------------------------------
# CUADRO 2: Tramos de edad de la poblacion en edad de trabajar en nuble para hombres y mujeres, 2016
#-------------------------------------------------------------------------------
#
pet_nuble_tramos = list()
for (i in 1:length(info)){
  pet_nuble_tramos[[i]] = svyby(~I(prov==84),by=~tramos_etarios,
                                info[[i]], svytotal)
}

pet_nuble_tramos. = do.call(rbind, pet_nuble_tramos)

pet_nuble_tramos. = pet_nuble_tramos.[,c("tramos_etarios","I(prov == 84)TRUE",
                                         "se.I(prov == 84)TRUE")] 

write.csv(pet_nuble_tramos, "pet_nuble_tramos.csv")

#
pet_nuble_tramos_hombres = list()
for (i in 1:length(info)){
  pet_nuble_tramos_hombres[[i]] = svyby(~I(prov==84 & sexo==1),by=~tramos_etarios,
                                        info[[i]], svytotal)
}

pet_nuble_tramos_hombres. = do.call(rbind, pet_nuble_tramos_hombres)

pet_nuble_tramos_hombres. = pet_nuble_tramos_hombres.[,c("tramos_etarios","I(prov == 84 & sexo == 1)TRUE",
                                                         "se.I(prov == 84 & sexo == 1)TRUE")] 
#Coeficiente de variacion 
pet_nuble_tramos_hombres.$`cv.I(prov == 84 & sexo == 1)TRUE`= pet_nuble_tramos_hombres.$`se.I(prov == 84 & sexo == 1)TRUE`/pet_nuble_tramos_hombres.$`I(prov == 84 & sexo == 1)TRUE`
# Tamano de la muestra 
pet_nuble_tramos_hombres.$`freq.I(prov == 84 & sexo == 1)TRUE` = xtabs(~tramos_etarios+I(prov==84), data=info[[1]]$variables)[,2]

# Recomendable 
pet_nuble_tramos_hombres.$`recomendable.I(prov == 84 & sexo == 1)TRUE` = ifelse(pet_nuble_tramos_hombres.$`cv.I(prov == 84 & sexo == 1)TRUE`<=0.25 & 
                                                                                  pet_nuble_tramos_hombres.$`freq.I(prov == 84 & sexo == 1)TRUE`>=50,
                                                                                "Recomendable", "No Recomendable")

write.csv(pet_nuble_tramos_hombres., "pet_nuble_tramos_hombres.csv")

#
pet_nuble_tramos_mujeres = list()
for (i in 1:length(info)){
  pet_nuble_tramos_mujeres[[i]] = svyby(~I(prov==84 & sexo==2),by=~tramos_etarios,
                                        info[[i]], svytotal)
}

pet_nuble_tramos_mujeres. = do.call(rbind, pet_nuble_tramos_mujeres)

pet_nuble_tramos_mujeres. = pet_nuble_tramos_mujeres.[,c("tramos_etarios","I(prov == 84 & sexo == 2)TRUE",
                                                         "se.I(prov == 84 & sexo == 2)TRUE")] 

#Coeficiente de variacion 
pet_nuble_tramos_mujeres.$`cv.I(prov == 84 & sexo == 2)TRUE`= pet_nuble_tramos_mujeres.$`se.I(prov == 84 & sexo == 2)TRUE`/pet_nuble_tramos_mujeres.$`I(prov == 84 & sexo == 2)TRUE`
# Tamano de la muestra 
pet_nuble_tramos_mujeres.$`freq.I(prov == 84 & sexo == 2)TRUE` = xtabs(~tramos_etarios+I(prov==84), data=info[[1]]$variables)[,2]

# Recomendable 
pet_nuble_tramos_mujeres.$`recomendable.I(prov == 84 & sexo == 2)TRUE` = ifelse(pet_nuble_tramos_mujeres.$`cv.I(prov == 84 & sexo == 2)TRUE`<=0.25 & 
                                                                                  pet_nuble_tramos_mujeres.$`freq.I(prov == 84 & sexo == 2)TRUE`>=50,
                                                                                "Recomendable", "No Recomendable")

write.csv(pet_nuble_tramos_mujeres., "pet_nuble_tramos_mujeres.csv")

#
pet_tramos = cbind(pet_nuble_tramos.[,2:3], pet_nuble_tramos_hombres.[,2:6], pet_nuble_tramos_mujeres.[,2:6])
#names(pet_tramos) = c("Tramos etarios", "Biobio", "Error Biobio", "Hombres Biobio", "Error Hombres Biobio", 
#                      "Mujeres Biobio", "Error Mujeres Biobio", "nuble", "Error nuble", "Hombres nuble", 
#                      "Error Hombres nuble", "Mujeres nuble", "Error Mujeres nuble")
write.csv(pet_tramos, "pet_tramos.csv")

# FIN CUADRO 2

