########################################################################
# Promedio de años de escolaridad
########################################################################

esc_ocup_nuble = lapply(as.character(ocupaciones4$codigo),
                        function(x) svyby(~esc, subset(diseño, provincia==84),
                                          by=~I(oficio4==x), svymean, na.rm=TRUE,
                                          drop.empty.groups = FALSE))

lp = length(ocupaciones4$etiqueta) 

esc_ocup_nuble. = unlist(lapply(esc_ocup_nuble, '[[',2,2))
ee.esc_ocup_nuble = unlist(lapply(esc_ocup_nuble,'[[',2,3))

esc_ocup_nuble.. = data.frame(esc = esc_ocup_nuble., se = ee.esc_ocup_nuble) %>% 
  mutate(cv = se/esc, esc=round(esc,1), codigo = ocupaciones4$codigo) 

esc_ocup_nuble = merge(esc_ocup_nuble.., ciuo, by.x ="codigo", by.y = "codigo") %>% 
  mutate(nota = ifelse(cv>=0.25,"a",""))

write.csv(esc_ocup_nuble, "escolaridad.csv")
