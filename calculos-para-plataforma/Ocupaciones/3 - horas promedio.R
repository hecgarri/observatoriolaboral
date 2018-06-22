########################################################################
# Promedio de horas habitualmente trabajadas
########################################################################
diseño$variables = mutate(diseño$variables, o10=ifelse(o10==999, NA, o10))

horas_ocup_nuble = lapply(as.character(ocupaciones4$codigo),
                          function(x) svyby(~o10, subset(diseño, provincia==84),
                                            by=~I(oficio4==x), svymean, na.rm=TRUE,
                                            drop.empty.groups = FALSE))

horas_ocup_nuble. = unlist(lapply(horas_ocup_nuble, '[[',2,2))
ee.horas_ocup_nuble = unlist(lapply(horas_ocup_nuble,'[[',2,3))

horas_ocup_nuble.. = data.frame(horas = horas_ocup_nuble., se = ee.horas_ocup_nuble) %>% 
  mutate(cv = se/horas, horas=round(horas,1), codigo = ocupaciones4$codigo)

horas_ocup_nuble = merge(horas_ocup_nuble.., ciuo, by.x="codigo", by.y="codigo") %>% 
  mutate(nota = ifelse(cv>=0.25,"a",""))

write.csv(horas_ocup_nuble, "horas_promedio.csv")