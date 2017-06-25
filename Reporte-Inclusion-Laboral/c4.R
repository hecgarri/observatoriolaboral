#-------------------------------------------------------------------------------
### CUADRO 4: Brecha de genero en ingresos para trabajadores dependientes, ingresos de los dependientes segun sexo y nivel educacional de Nuble, 2016. 
#-------------------------------------------------------------------------------
################################################################################
# ocupados a nivel de nuble, by sexo, by educ
################################################################################
#
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

write.csv(ocupados.nuble.educ.sexo., "ocupados_nuble_educ3_sexo.csv")

### FIN CUADRO 4
