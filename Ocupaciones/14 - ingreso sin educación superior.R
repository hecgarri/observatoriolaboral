####################################################################################
# Promedio de ingreso mensual líquido de la ocupación principal de los trabajadores
# sin educación superior completa.
####################################################################################

ingreso_no_sup_ocup_nuble = lapply(as.character(ocupaciones4$codigo),
                                   function(x) svyby(~yoprCor, subset(diseño, provincia==84),
                                                     by=~I(oficio4==x & nivel_educ!="Profesional completa"), 
                                                     svymean, na.rm=TRUE,na.rm.all = TRUE, drop.empty.groups = FALSE))

ingreso_no_sup_ocup_nuble. = unlist(lapply(ingreso_no_sup_ocup_nuble, '[[',2,2))
ee.ingreso_no_sup_ocup_nuble = unlist(lapply(ingreso_no_sup_ocup_nuble,'[[',2,3))

ingreso_no_sup_ocup_nuble.. = data.frame(ingreso = ingreso_no_sup_ocup_nuble.,
                                         se = ee.ingreso_no_sup_ocup_nuble) %>% 
  mutate(cv = se/ingreso, ingreso=round(ingreso,1), codigo = ocupaciones4$codigo)

temp = subset(diseño$variables, provincia==84) %>% group_by(oficio4, I(nivel_educ!="Profesional completa")) %>%
  summarise(total.count = n()) %>% 
  filter(oficio4 == 1314 |                                
           oficio4 == 2331 | oficio4 == 4115 |                                                                  
           oficio4 == 5122 | oficio4 == 5131 |                            
           oficio4 == 5220 | oficio4 == 6111 |
           oficio4 == 6112 | oficio4 == 6210 |                                                           
           oficio4 == 7124 | oficio4 == 8322 |
           oficio4 == 8324 | oficio4 == 9131 | 
           oficio4 == 9132 | oficio4 == 9211, 
         `I(nivel_educ != \"Profesional completa\")`==TRUE) %>% 
  select(oficio4, total.count)

ingreso_no_sup_ocup_nuble = merge(ingreso_no_sup_ocup_nuble.., temp, by.x = "codigo", by.y="oficio4")

ingreso_no_sup_ocup_nuble = merge(ingreso_no_sup_ocup_nuble, ciuo, by.x="codigo", by.y="codigo") %>% 
  mutate(nota = ifelse(cv>=0.25 | total.count<50,"a",""))

write.csv(ingreso_no_sup_ocup_nuble, "ingreso_sin_superior.csv")
