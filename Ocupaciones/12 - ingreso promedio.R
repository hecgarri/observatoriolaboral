########################################################################
# Promedio de Ingreso mensual líquido de la ocupación principal
########################################################################

ingreso_ocup_nuble = lapply(as.character(ocupaciones4$codigo),
                    function(x) svyby(~yoprCor, subset(diseño, provincia==84),
                    by=~I(oficio4==x), svymean, na.rm=TRUE,
                    drop.empty.groups = FALSE))

ingreso_ocup_nuble. = unlist(lapply(ingreso_ocup_nuble, '[[',2,2))
ee.ingreso_ocup_nuble = unlist(lapply(ingreso_ocup_nuble,'[[',2,3))

ingreso_ocup_nuble.. = data.frame(ingreso = ingreso_ocup_nuble., se = ee.ingreso_ocup_nuble) %>% 
  mutate(cv = se/ingreso, ingreso=round(ingreso,1), codigo = ocupaciones4$codigo)

ingreso_ocup_nuble = merge(ingreso_ocup_nuble.., ciuo, by.x="codigo", by.y="codigo") %>% 
  mutate(nota = ifelse(cv>=0.25,"a",""))

write.csv(ingreso_ocup_nuble, "ingreso_promedio.csv")
