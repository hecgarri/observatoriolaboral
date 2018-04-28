#-------------------------------------------------------------------------------
# Cuadro 19: ocupados de nuble por tramo de edad y sexo
#-------------------------------------------------------------------------------
## hombres nacional
#-------------------------------------------------------------------------------
# Creacion de la variable: tramos
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, tramos = 
                            ifelse(edad>=15 & edad<=19,1,
                            ifelse(edad>=20 & edad<=24,2,
                            ifelse(edad>=25 & edad<=34,3,
                            ifelse(edad>=35 & edad<=44,4,
                            ifelse(edad>=45 & edad<=54,5,
                            ifelse(edad>=55 & edad<=64,6,
                            ifelse(edad>=65,7,NA)))))))) 
}
#
for (i in 1:length(info)){
  info[[i]]$variables$tramos = factor(info[[i]]$variables$tramos, 
                       levels=c(1,2,3,4,5,6,7),
                       labels=c("15-19", "20-24", "25-34", 
                                "35-44", "45-54", "55-64", "65 o mas"))
}


tramos = levels(info[[1]]$variables$tramos)

tramos_etarios_nacional2 = list()
for (i in 1:length(info)){
  tramos_etarios_nacional2[[i]] = lapply(tramos, function(x) 
    svyratio(~I(cae_general=="Ocupado" & tramos==x & sexo==1),
                                  denominator=~I(cae_general=="Ocupado" & sexo==1), 
                                  na.rm=TRUE, info[[i]]))
}

#
tramos_etarios_nacional2_=list()
for (i in 1:length(info)){
  tramos_etarios_nacional2_[[i]] = unlist(lapply(tramos_etarios_nacional2[[i]], '[[', 1)) 
}

tramos_etarios_nacional. = do.call(rbind, tramos_etarios_nacional2_)
rownames(tramos_etarios_nacional.) = c("EFM","AMJ","JAS","OND")
colnames(tramos_etarios_nacional.) = c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65 y mas")

var.tramos_etarios_nacional2_=list()
for (i in 1:length(info)){
  var.tramos_etarios_nacional2_[[i]] = unlist(lapply(tramos_etarios_nacional2[[i]], '[[', 2)) 
}

var.tramos_etarios_nacional2_ = do.call(rbind, var.tramos_etarios_nacional2_)
rownames(var.tramos_etarios_nacional2_) = c("EFM","AMJ","JAS","OND")
colnames(var.tramos_etarios_nacional2_) = c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65 y mas")

write.csv(tramos_etarios_nacional., "tramos_etarios_nacional_sexo_hombre.csv")

# mujeres nacional
tramos = levels(info[[1]]$variables$tramos)
tramos.etarios.mujer.nacional2 = list()
for (i in 1:length(info)){
  tramos.etarios.mujer.nacional2[[i]] = 
    lapply(tramos, function(x) svyratio(~I(cae_general=="Ocupado" & tramos==x & sexo==2),
                                        denominator=~I(cae_general=="Ocupado" & sexo==2), 
                                        na.rm=TRUE, info[[i]]))
}

tramos.etarios.mujer.nacional2.=list()
for (i in 1:length(info)){
  tramos.etarios.mujer.nacional2.[[i]] = unlist(lapply(tramos.etarios.mujer.nacional2[[i]], '[[', 1)) 
}

tramos.etarios.mujer.nacional. = do.call(rbind, tramos.etarios.mujer.nacional2.)
rownames(tramos.etarios.mujer.nacional.) = c("EFM","AMJ","JAS","OND")
colnames(tramos.etarios.mujer.nacional.) = c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65 y mas")

var.tramos.etarios.mujer.nacional2.=list()
for (i in 1:length(info)){
  var.tramos.etarios.mujer.nacional2.[[i]] = unlist(lapply(tramos.etarios.mujer.nacional2[[i]], '[[', 2)) 
}

var.tramos.etarios.mujer.nacional. = do.call(rbind, var.tramos.etarios.mujer.nacional2.)
rownames(var.tramos.etarios.mujer.nacional.) = c("EFM","AMJ","JAS","OND")
colnames(var.tramos.etarios.mujer.nacional.) = c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65 y mas")

write.csv(tramos.etarios.mujer.nacional., "tramos_etarios_nacional_sexo_mujer.csv")

## Occupados Nacional por tramos
tramos = levels(info[[1]]$variables$tramos)
tramos_etarios_nacional = list()
for (i in 1:length(info)){
  tramos_etarios_nacional[[i]] = 
    lapply(tramos, function(x) svyratio(~I(cae_general=="Ocupado" & tramos==x),
                                        denominator=~I(cae_general=="Ocupado"), 
                                        na.rm=TRUE, info[[i]]))
}

#
tramos_etarios_nacional.=list()
for (i in 1:length(info)){
  tramos_etarios_nacional.[[i]] = unlist(lapply(tramos_etarios_nacional[[i]], '[[', 1)) 
}

tramos_etarios_nacional2 = do.call(rbind, tramos_etarios_nacional.)
rownames(tramos_etarios_nacional2) = c("EFM","AMJ","JAS","OND")
colnames(tramos_etarios_nacional2) = c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65 y mas")

var.tramos_etarios_nacional2=list()
for (i in 1:length(info)){
  var.tramos_etarios_nacional2[[i]] = unlist(lapply(tramos_etarios_nacional[[i]], '[[', 2)) 
}

var.tramos_etarios_nacional2_ = do.call(rbind, var.tramos_etarios_nacional2)
rownames(var.tramos_etarios_nacional2_) = c("EFM","AMJ","JAS","OND")
colnames(var.tramos_etarios_nacional2_) = c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65 y mas")

write.csv(tramos_etarios_nacional2, "tramos_etarios_nacional.csv")

# Hombre en nuble
tramos = levels(info[[1]]$variables$tramos)
tramos.etarios_nuble2 = list()
for (i in 1:length(info)){
  tramos.etarios_nuble2[[i]] = 
    lapply(tramos, function(x) svyratio(~I(cae_general=="Ocupado" & tramos==x & prov_e==84 & sexo==1),
                                        denominator=~I(cae_general=="Ocupado" & prov_e==84 & sexo==1), 
                                        na.rm=TRUE, info[[i]]))
}

tramos.etarios_nuble2.=list()
for (i in 1:length(info)){
  tramos.etarios_nuble2.[[i]] = unlist(lapply(tramos.etarios_nuble2[[i]], '[[', 1)) 
}

tramos.etarios_nuble. = do.call(rbind, tramos.etarios_nuble2.)
rownames(tramos.etarios_nuble.) = c("EFM","AMJ","JAS","OND")
colnames(tramos.etarios_nuble.) = c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65 y mas")

var.tramos.etarios_nuble2.=list()
for (i in 1:length(info)){
  var.tramos.etarios_nuble2.[[i]] = unlist(lapply(tramos.etarios_nuble2[[i]], '[[', 2)) 
}

var.tramos.etarios_nuble. = do.call(rbind, var.tramos.etarios_nuble2.)
rownames(var.tramos.etarios_nuble.) = c("EFM","AMJ","JAS","OND")
colnames(var.tramos.etarios_nuble.) = c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65 y mas")

write.csv(tramos.etarios_nuble., "tramos_etarios_nuble_sexo_hombre.csv")

# mujeres nuble
tramos = levels(info[[1]]$variables$tramos)
tramos.etarios_nuble_mujeres_nuble2 = list()
for (i in 1:length(info)){
  tramos.etarios_nuble_mujeres_nuble2[[i]] = 
    lapply(tramos, function(x) svyratio(~I(cae_general=="Ocupado" & tramos==x & prov_e==84 & sexo==2),
                                        denominator=~I(cae_general=="Ocupado" & prov_e==84 & sexo==2), 
                                        na.rm=TRUE, info[[i]]))
}

tramos.etarios_nuble_mujeres_nuble2.=list()
for (i in 1:length(info)){
  tramos.etarios_nuble_mujeres_nuble2.[[i]] = unlist(lapply(tramos.etarios_nuble_mujeres_nuble2[[i]], '[[', 1)) 
}

tramos.etarios_nuble_mujeres_nuble. = do.call(rbind, tramos.etarios_nuble_mujeres_nuble2.)
rownames(tramos.etarios_nuble_mujeres_nuble.) = c("EFM","AMJ","JAS","OND")
colnames(tramos.etarios_nuble_mujeres_nuble.) = c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65 y mas")

var.tramos.etarios_nuble_mujeres_nuble2.=list()
for (i in 1:length(info)){
  var.tramos.etarios_nuble_mujeres_nuble2.[[i]] = unlist(lapply(tramos.etarios_nuble_mujeres_nuble2[[i]], '[[', 2)) 
}

var.tramos.etarios_nuble_mujeres_nuble. = do.call(rbind, var.tramos.etarios_nuble_mujeres_nuble2.)
rownames(var.tramos.etarios_nuble_mujeres_nuble.) = c("EFM","AMJ","JAS","OND")
colnames(var.tramos.etarios_nuble_mujeres_nuble.) = c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65 y mas")

write.csv(tramos.etarios_nuble_mujeres_nuble., "tramos_etarios_nuble_sexo_mujer.csv")

## Ocupados Nuble por tramos
tramos = levels(info[[1]]$variables$tramos)
tramos.etarios.nuble = list()
for (i in 1:length(info)){
  tramos.etarios.nuble[[i]] = 
    lapply(tramos, function(x) svyratio(~I(cae_general=="Ocupado" & tramos==x & prov_e==84),
                                        denominator=~I(cae_general=="Ocupado" & prov_e==84), 
                                        na.rm=TRUE, info[[i]]))
}

tramos.etarios.nuble.=list()
for (i in 1:length(info)){
  tramos.etarios.nuble.[[i]] = unlist(lapply(tramos.etarios.nuble[[i]], '[[', 1)) 
}

tramos.etarios.nuble2 = do.call(rbind, tramos.etarios.nuble.)
rownames(tramos.etarios.nuble2) = c("EFM","AMJ","JAS","OND")
colnames(tramos.etarios.nuble2) = c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65 y mas")

var.tramos.etarios.nuble.=list()
for (i in 1:length(info)){
  var.tramos.etarios.nuble.[[i]] = unlist(lapply(tramos.etarios.nuble[[i]], '[[', 2)) 
}

var.tramos.etarios.nuble2. = do.call(rbind, var.tramos.etarios.nuble.)
rownames(var.tramos.etarios.nuble2.) = c("EFM","AMJ","JAS","OND")
colnames(var.tramos.etarios.nuble2.) = c("15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65 y mas")

write.csv(tramos.etarios.nuble2, "tramos_etarios_nuble.csv")

# FIN CUADRO 19
