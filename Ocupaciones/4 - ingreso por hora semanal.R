#################################################################################
# Promedio de ingreso mensual líquido por hora (variable o10) de la ocupación
# principal.
#################################################################################

diseño$variables = mutate(diseño$variables, horas_mensuales=o10*4)

inc_hora_ocup_nuble = lapply(as.character(ocupaciones4$codigo),
                     function(x) svyby(~yoprCor,
                     denominator=~horas_mensuales, subset(diseño, provincia==84),
                     by=~I(oficio4==x),svyratio, na.rm=TRUE, na.rm.all = TRUE,
                     drop.empty.groups = FALSE))

inc_hora_ocup_nuble. = unlist(lapply(inc_hora_ocup_nuble, '[[', 2,2)) 

ee.inc_hora_ocup_nuble = unlist(lapply(inc_hora_ocup_nuble,'[[',2,3))
  
inc_hora_ocup_nuble.. = data.frame(inc_hora_ocup_nuble., ee.inc_hora_ocup_nuble) %>% 
  `colnames<-` (c("ingreso por hora", "se")) %>% 
  mutate(cv=se/`ingreso por hora`, codigo = ocupaciones4$codigo) 

inc_hora_ocup_nuble = merge(inc_hora_ocup_nuble.., ciuo, by.x="codigo", by.y="codigo") %>% 
  mutate(nota=ifelse(cv>=0.25, "a",""))

write.csv(inc_hora_ocup_nuble, "ingreso_por_hora.csv")
