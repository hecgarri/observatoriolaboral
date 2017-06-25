#-------------------------------------------------------------------------------
### CUADRO 3: Ocupados de Nuble por nivel educacional y sexo, 2016.  
#-------------------------------------------------------------------------------
################################################################################
# Ocupados por sexo y escolaridad a nivel nacional y nuble
################################################################################

# ocupados a nivel nacional, by sexo, by educ
ocupados.nacional.educ.sexo = list()
for (i in 1:length(info)){
  ocupados.nacional.educ.sexo[[i]] = svyby(~I(cae_general=="Ocupado"),
                                           by=~educ3+sexo, info[[i]], svytotal,
                                           multicore = TRUE, 
                                           drop.empty.groups = FALSE, na.rm=TRUE)
}

ocupados.nacional.educ.sexo. = 
  do.call(rbind, ocupados.nacional.educ.sexo)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(cae_general=="Ocupado")+educ3+sexo, data=info[[i]]$variables)  %>% data.frame() %>% 
    filter(`I.cae_general.....Ocupado..`==TRUE)
  
}
freq. = do.call(rbind, freq)

ocupados.nacional.educ.sexo.$cv = 
  ocupados.nacional.educ.sexo.$`se.I(cae_general == \"Ocupado\")TRUE`/ocupados.nacional.educ.sexo.$`I(cae_general == \"Ocupado\")TRUE`

ocupados.nacional.educ.sexo.$frecuencia = freq.$Freq

ocupados.nacional.educ.sexo. = 
  ocupados.nacional.educ.sexo.[,c("educ3","sexo", "I(cae_general == \"Ocupado\")TRUE", "I(cae_general == \"Ocupado\")TRUE", "cv", "frecuencia")]

write.csv(ocupados.nacional.educ.sexo., "ocupados_nacional_educ_sexo.csv")

# ocupados a nivel de nuble, by sexo, by educ
ocupados.nuble.educ.sexo = list()
for (i in 1:length(info)){
  ocupados.nuble.educ.sexo[[i]] = svyby(~I(prov_e==84),
                                        by=~educ3+sexo, info[[i]], svytotal,
                                        multicore = TRUE, 
                                        drop.empty.groups = FALSE, na.rm=TRUE)
}

ocupados.nuble.educ.sexo. = 
  do.call(rbind, ocupados.nuble.educ.sexo)

freq = list()
for (i in 1:length(info)){
  freq[[i]]= xtabs(~I(prov_e==84)+educ3+sexo, data=info[[i]]$variables) %>% data.frame() %>% 
    filter(`I.prov_e....84.`==TRUE) 
  
}
freq. = do.call(rbind, freq)

ocupados.nuble.educ.sexo.$cv = 
  ocupados.nuble.educ.sexo.$`se.I(prov_e == 84)TRUE`/ocupados.nuble.educ.sexo.$`I(prov_e == 84)TRUE`

ocupados.nuble.educ.sexo.$frecuencia = freq.$Freq

ocupados.nuble.educ.sexo. = 
  ocupados.nuble.educ.sexo.[,c("educ3", "sexo","I(prov_e == 84)TRUE","se.I(prov_e == 84)TRUE", "cv", "frecuencia")]

write.csv(ocupados.nuble.educ.sexo., "ocupados_nuble_educ_sexo.csv")

### FIN CUADRO 3
