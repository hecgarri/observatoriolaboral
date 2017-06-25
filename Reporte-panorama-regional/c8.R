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

#-------------------------------------------------------------------------------
# CUADRO 8: Composicion de los trabajadores de nuble segun residencia.
#-------------------------------------------------------------------------------
# Vive en Nuble,pero trabajan en otra region 
conmutantes.nuble = list()
for (i in 1:length(info)){
  conmutantes.nuble[[i]] = svytotal(~I(conmutante2==1 & prov==84),info[[i]], na.rm=TRUE)
}

conmutantes.nuble. = unlist(lapply(conmutantes.nuble, '[[', 2) ) 

ee.conmutantes.nuble = unlist(lapply(conmutantes.nuble, SE))[c(2,4,6,8)]

conmutantes.nuble.. = data.frame(conmutantes.nuble., ee.conmutantes.nuble)

write.csv(conmutantes.nuble.., "VivenPeroNoTrabajanEnNuble.csv")

# Trabaja en nuble, pero vive en otra region 
conmutantes.otra = list()
for (i in 1:length(info)){
  conmutantes.otra[[i]] = svytotal(~I(conmutante2==1 & prov_e==84),info[[i]], na.rm=TRUE)
}

conmutantes.otra. = unlist(lapply(conmutantes.otra, '[[', 2) ) 

ee.conmutantes.otra = unlist(lapply(conmutantes.otra, SE))[c(2,4,6,8)]

conmutantes.otra.. = data.frame(conmutantes.otra., ee.conmutantes.otra)

write.csv(conmutantes.otra.., "TrabajanEnNublePeroVivenEnOtraRegion.csv")

# Trabajan en nuble y viven en nuble
residentes.nuble. = list()
for (i in 1:length(info)){
  residentes.nuble.[[i]] = svytotal(~I(prov==84 & prov_e==84),info[[i]], na.rm=TRUE)
}

residentes.nuble.. = unlist(lapply(residentes.nuble., '[[', 2) ) 

ee.residentes.nuble = unlist(lapply(residentes.nuble., SE))[c(2,4,6,8)]

residentes.nuble... = data.frame(residentes.nuble.., ee.residentes.nuble)

write.csv(residentes.nuble..., "VivenEnNubleyTrabajanEnNuble.csv")

# Trabajan en nuble
trabajan.nuble = list()
for (i in 1:length(info)){
  trabajan.nuble[[i]] = svytotal(~I(prov_e==84),info[[i]], na.rm=TRUE)
}

trabajan.nuble. = unlist(lapply(trabajan.nuble, '[[', 2) ) 

ee.trabajan.nuble = unlist(lapply(trabajan.nuble, SE))[c(2,4,6,8)]

trabajan.nuble.. = data.frame(trabajan.nuble., ee.trabajan.nuble)

write.csv(trabajan.nuble.., "TrabajanEnNuble.csv")

# FIN CUADRO 8:

