#####################################################
# % de mujeres
####################################################

muj_ocup_nuble = lapply(as.character(ocupaciones4$codigo),
                        function(x) svyby(~I(sexo==2), denominator=~I(activ==1),
                                          subset(diseño, provincia==84), by=~I(oficio4==x),
                                          svyratio, na.rm=TRUE,drop.empty.groups = FALSE))

muj_ocup_nuble. = unlist(lapply(muj_ocup_nuble, '[[', 2,2)) 

ee.muj_ocup_nuble = unlist(lapply(muj_ocup_nuble,'[[',2,3))

muj_ocup_nuble.. = data.frame(muj_ocup_nuble., ee.muj_ocup_nuble) %>% 
  `colnames<-` (c("ocupados", "se")) %>% 
  mutate(cv=se/ocupados, ocupados=round(ocupados*100,1), codigo = ocupaciones4$codigo) 

muj_ocup_nuble = merge(muj_ocup_nuble.., ciuo, by.x="codigo", by.y="codigo")

write.csv(muj_ocup_nuble, "porcentaje_mujeres.csv")
