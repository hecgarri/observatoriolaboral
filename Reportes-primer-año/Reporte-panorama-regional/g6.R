#-------------------------------------------------------------------------------
# GRAFICO 6:
#-------------------------------------------------------------------------------

#Tasa de desempleo nacional
tasa.desempleo = c()
for (i in 1:length(info)){
  tasa.desempleo[[i]] = svyratio(~I(cae_general=="Desocupado" | cae_general=="Busca Trabajo Primera Vez"),
                                 denominator=~I(cae_general=="Ocupado" | cae_general=="Desocupado" |
                                                  cae_general=="Busca Trabajo Primera Vez"),
                                 info[[i]])
}

tasa.desempleo.n = unlist(lapply(tasa.desempleo, '[[', 1) ) 

ee.tasa.desempleo = unlist(lapply(tasa.desempleo, '[[',2))

cv.tasa.desempleo = (ee.tasa.desempleo/tasa.desempleo.n)

freq= list()
for (i in 1:length(info)){
  freq[[i]]=xtabs(~I(cae_general=="Desocupado" | cae_general=="Busca Trabajo Primera Vez"), data=info[[i]]$variables)[2]
}
freq1 = do.call(rbind, freq)

desempleo.nacional = data.frame(tasa.desempleo.n, ee.tasa.desempleo, cv.tasa.desempleo, freq1)

write.csv(desempleo.nacional, "desempleo_nacional.csv")

#Tasa de desempleo provincial
tasa.desempleo.r = list()
for (i in 1:length(info)){
  tasa.desempleo.r[[i]] = svyratio(~I((cae_general=="Desocupado" | cae_general=="Busca Trabajo Primera Vez") & prov==84),
                                   denominator=~I((cae_general=="Ocupado" |
                                                     cae_general=="Desocupado" |
                                                     cae_general=="Busca Trabajo Primera Vez") & 
                                                    prov==84),info[[i]])
  
}

tasa.desempleo.r. = unlist(lapply(tasa.desempleo.r, '[[', 1) ) 

ee.tasa.desempleo = unlist(lapply(tasa.desempleo.r, SE))

cv.tasa.desempleo.r. = ee.tasa.desempleo/tasa.desempleo.r. 

for (i in 1:length(info)){
  freq[[i]] = xtabs(~I((cae_general=="Desocupado" | cae_general=="Busca Trabajo Primera Vez") & prov==84), 
                    data = info[[i]]$variables)[2]
}

freq1 = do.call(rbind, freq)

tasa.desempleo.r.. = data.frame(tasa.desempleo.r., ee.tasa.desempleo, cv.tasa.desempleo.r., freq1)

write.csv(tasa.desempleo.r.., "tasa_desempleo_provincial.csv")

# FIN GRAFICO 6:
