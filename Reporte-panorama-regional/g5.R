#-------------------------------------------------------------------------------
# GRAFICO 5: Evolucion de los ocupados nacional y nuble.
#-------------------------------------------------------------------------------
#Total de ocupados nacional
ocupados = c()
for (i in 1:length(info)){
  ocupados[[i]] = svytotal(~I(cae_general=="Ocupado"),info[[i]])
}
ocupados.n = unlist(lapply(ocupados, '[[', 2)) 

ee.ocupados = unlist(lapply(ocupados, SE))[seq(2,length(info),2)]

cv.ocupados = (ee.ocupados/ocupados.n)

freq= list()
for (i in 1:length(info)){
  freq[[i]]=xtabs(~I(cae_general=="Ocupado"), data=info[[i]]$variables)[2]
}
freq1 = do.call(rbind, freq)

ocupados.nacional = data.frame(ocupados.n, ee.ocupados, cv.ocupados, freq1)

write.csv(ocupados.nacional, "ocupados_nacional.csv")

#Total de ocupados provincial
t.ocupados.p = c()
for (i in 1:length(info)){
  t.ocupados.p[[i]] = svytotal(~I(cae_general=="Ocupado" & prov==84),info[[i]])
}

t.ocupados.p. = unlist(lapply(t.ocupados.p, '[[', 2) ) 

ee.t.ocupados.p. = unlist(lapply(t.ocupados.p, SE))[seq(2,length(info),2)]

cv.ocupados.p = (ee.t.ocupados.p./t.ocupados.p.)

freq= list()
for (i in 1:length(info)){
  freq[[i]]=xtabs(~I(cae_general=="Ocupado" & prov==84), data=info[[i]]$variables)[2]
}
freq1 = do.call(rbind, freq)

ocupados.provincial = data.frame(t.ocupados.p., ee.t.ocupados.p., cv.ocupados.p, freq1)

write.csv(ocupados.provincial, "ocupados_provincial.csv")

### FIN GRAFICO 5
