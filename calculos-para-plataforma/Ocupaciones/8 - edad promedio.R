########################################################################
# Promedio de edad
########################################################################

edad_ocup_nuble = lapply(as.character(ocupaciones4$codigo),
                         function(x) svyby(~edad, subset(diseño, provincia==84),
                                           by=~I(oficio4==x), svymean, na.rm=TRUE, drop.empty.groups = FALSE))

edad_ocup_nuble. = unlist(lapply(edad_ocup_nuble, '[[',2,2))
ee.edad_ocup_nuble = unlist(lapply(edad_ocup_nuble,'[[',2,3))

edad_ocup_nuble.. = data.frame(edad = edad_ocup_nuble., se = ee.edad_ocup_nuble) %>% 
  mutate(cv = se/edad, edad=round(edad,1), codigo = ocupaciones4$codigo)

edad_ocup_nuble = merge(edad_ocup_nuble.., ciuo, by.x="codigo", by.y="codigo")

edad_ocup_nuble = mutate(edad_ocup_nuble, nota = ifelse(cv>=0.25,a,""))

write.csv(edad_ocup_nuble, "edad_promedio.csv")
