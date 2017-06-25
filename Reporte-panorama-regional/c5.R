#-------------------------------------------------------------------------------
# CUADRO 5: fuerza de trabajo en nuble y naciona, participacion sobre el tota nacional y tasa de crecimiento.
#-------------------------------------------------------------------------------

# Fuerza de trabajo: nacional
fuerza.trabajo = list()
for (i in 1:length(info)){
  fuerza.trabajo[[i]] = svytotal(~I(cae_general=="Ocupado" | cae_general=="Desocupado" |
                                      cae_general=="Busca Trabajo Primera Vez"),info[[i]])
}

fuerza.trabajo.n = unlist(lapply(fuerza.trabajo, '[[', 2) ) 

ee.fuerza.trabajo = unlist(lapply(fuerza.trabajo, SE))[seq(2,length(info)*2,2)]

cv.fuerza.trabajo = (ee.fuerza.trabajo/fuerza.trabajo.n)

freq = list()
for (i in 1:length(info)){
  freq[[i]] = xtabs(~I(cae_general=="Ocupado" | cae_general=="Desocupado" |
                         cae_general=="Busca Trabajo Primera Vez"), data=info[[i]]$variables)[2]
}

freq = do.call(rbind, freq)

fuerza.trabajo.. = data.frame(fuerza.trabajo.n, ee.fuerza.trabajo, cv.fuerza.trabajo, freq)

write.csv(fuerza.trabajo.., "fuerza_trabajo_nacional.csv")

#fuerza.trabajo provincial 

fuerza.trabajo.p = list()
for (i in 1:length(info)){
  fuerza.trabajo.p[[i]] = svytotal(~I((cae_general=="Ocupado" |
                                         cae_general=="Desocupado" | 
                                         cae_general=="Busca Trabajo Primera Vez") & prov==84),
                                   info[[i]])
}

fuerza.trabajo.p. = unlist(lapply(fuerza.trabajo.p, '[[', 2)) 

ee.fuerza.trabajo = unlist(lapply(fuerza.trabajo.p, SE))[seq(2,length(info)*2,2)]

cv.fuerza.trabajo.p = (ee.fuerza.trabajo/fuerza.trabajo.p.)

freq = list()
for (i in 1:length(info)){
  freq[[i]] = xtabs(~I((cae_general=="Ocupado" |
                          cae_general=="Desocupado" | 
                          cae_general=="Busca Trabajo Primera Vez") & prov==84), data=info[[i]]$variables)[2]
}
freq = do.call(rbind, freq)

fuerza.trabajo.p.. = data.frame(fuerza.trabajo.p., ee.fuerza.trabajo, cv.fuerza.trabajo.p, freq)

write.csv(fuerza.trabajo.p.., "fuerza_trabajo_provincial.csv")

# Tasa de participacion NACIONAL
tasa.part = list()
for (i in 1:length(info)){
  tasa.part[[i]] = svyratio(~I(cae_general=="Ocupado" | 
                                 cae_general=="Desocupado" | 
                                 cae_general=="Busca Trabajo Primera Vez"), 
                            denominator=~I(edad>=15),
                            info[[i]],multicore = TRUE, 
                            drop.empty.groups = FALSE, 
                            na.rm=TRUE)
}

tasa.part.n = unlist(lapply(tasa.part, '[[', 1) ) 

ee.tasa.part = unlist(lapply(tasa.part, SE))

cv.tasa.part = (ee.tasa.part/tasa.part.n)

freq = list()
for (i in 1:length(info)){
  freq[[i]] = xtabs(~I(cae_general=="Ocupado" | 
                         cae_general=="Desocupado" | 
                         cae_general=="Busca Trabajo Primera Vez"), data = info[[i]]$variables)
  
}

freq1 = do.call(rbind, freq) [,2]

tasa.participacion = data.frame(tasa.part.n, ee.tasa.part, cv.tasa.part, freq1)

write.csv(tasa.participacion, "tasa_participacion_nacional.csv")

# Tasa de participacion PROVINCIAL
tasa.part.n = list()
for (i in 1:length(info)){
  tasa.part.n[[i]] = svyratio(~I((cae_general=="Ocupado" | 
                                    cae_general=="Desocupado" | 
                                    cae_general=="Busca Trabajo Primera Vez") & prov==84), 
                              denominator=~I(edad>=15 & prov==84),
                              info[[i]],multicore = TRUE, 
                              drop.empty.groups = FALSE, 
                              na.rm=TRUE)
}

tasa.part.p = unlist(lapply(tasa.part.n, '[[', 1) ) 

ee.tasa.part.p = unlist(lapply(tasa.part.n, SE))

cv.tasa.part.p = (ee.tasa.part.p/tasa.part.p)

freq = list()
for (i in 1:length(info)){
  freq[[i]] = xtabs(~I((cae_general=="Ocupado" | 
                          cae_general=="Desocupado" | 
                          cae_general=="Busca Trabajo Primera Vez") & prov==84), data = info[[i]]$variables)
  
}

freq1 = do.call(rbind, freq) [,2]

tasa.participacion.p = data.frame(tasa.part.p, ee.tasa.part.p, cv.tasa.part.p, freq1)

write.csv(tasa.participacion.p, "tasa_participacion_provincial.csv")

# FIN CUADRO 5.
