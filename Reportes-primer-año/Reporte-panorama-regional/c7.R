#-------------------------------------------------------------------------------
# CUADRO 7: tasa de ocupacion para nuble y nacional 
#-------------------------------------------------------------------------------

# Tasa de ocupacion NACIONAL
tasa.ocup = list()
for (i in 1:length(info)){
  tasa.ocup[[i]] = svyratio(~I(cae_general=="Ocupado"),~I(edad>=15), design = info[[i]])
}

tasa.ocup.n = unlist(lapply(tasa.ocup, '[[', 1) ) 

ee.tasa.ocup = unlist(lapply(tasa.ocup, SE))

cv.tasa.ocup = (ee.tasa.ocup/tasa.ocup.n)

freq = list()
for (i in 1:length(info)){
  freq[[i]] = xtabs(~I(cae_general=="Ocupado"), data = info[[i]]$variables)[2]
}
freq1 = do.call(rbind, freq)

tasa.ocupacion = data.frame(tasa.ocup.n, ee.tasa.ocup, cv.tasa.ocup, freq1)

write.csv(tasa.ocupacion, "tasa_ocupacion_nacional.csv")

# Tasa de ocupacion PROVINCIAL
tasa.ocup.r = list()
for (i in 1:length(info)){
  tasa.ocup.r[[i]] = svyratio(~I(cae_general=="Ocupado" & prov==84),
                              denominator=~I(edad>=15 & prov==84),
                              info[[i]])
}

tasa.ocup.r. = unlist(lapply(tasa.ocup.r,'[[',1))

ee.tasa.ocup.r = unlist(lapply(tasa.ocup.r,SE)) 

cv.tasa.ocup.r = ee.tasa.ocup.r/tasa.ocup.r.

freq = list()
for (i in 1:length(info)){
  freq[[i]] = xtabs(~I(cae_general=="Ocupado" & prov==84), data=info[[i]]$variables)[2]
}

freq1 = do.call(rbind, freq)

tasa.ocup.r.. = data.frame(tasa.ocup.r., ee.tasa.ocup.r, cv.tasa.ocup.r, freq1)

write.csv(tasa.ocup.r.., "tasa_ocupacion_provincial.csv")

# FIN CUADRO 7
