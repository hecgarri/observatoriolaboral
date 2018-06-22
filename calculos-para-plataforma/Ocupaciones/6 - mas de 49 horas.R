######################################################################################
# % de trabajadores que trabaja 49 horas o más a la semana.
######################################################################################

explo_ocup_nuble = lapply(as.character(ocupaciones4$codigo),
                          function(x) svyby(~I(o10>=49), denominator=~I(activ==1),
                                            subset(diseño, provincia==84), by=~I(oficio4==x),
                                            svyratio, na.rm=TRUE,drop.empty.groups = FALSE))

explo_ocup_nuble. = unlist(lapply(explo_ocup_nuble, '[[', 2,2)) 

ee.explo_ocup_nuble = unlist(lapply(explo_ocup_nuble,'[[',2,3))

explo_ocup_nuble.. = data.frame(explo_ocup_nuble., ee.explo_ocup_nuble) %>% 
  `colnames<-` (c("porcentaje", "se")) %>% 
  mutate(cv=se/porcentaje, porcentaje=round(porcentaje*100,1), codigo = ocupaciones4$codigo) 

temp = subset(diseño$variables, provincia==84) %>% group_by(oficio4, I(o10>=49)) %>%
  summarise(total.count = n()) %>% 
  filter(oficio4 == 1314 |                                
           oficio4 == 2331 | oficio4 == 4115 |                                                                  
           oficio4 == 5122 | oficio4 == 5131 |                            
           oficio4 == 5220 | oficio4 == 6111 |
           oficio4 == 6112 | oficio4 == 6210 |                                                           
           oficio4 == 7124 | oficio4 == 8322 |
           oficio4 == 8324 | oficio4 == 9131 | 
           oficio4 == 9132 | oficio4 == 9211, 
         `I(o10 >= 49)`==TRUE) %>% 
  select(oficio4, total.count)

explo_ocup_nuble = merge(explo_ocup_nuble.., temp, by.x="codigo", by.y="oficio4")
explo_ocup_nuble = merge(explo_ocup_nuble, ciuo, by.x="codigo", by.y="codigo") %>% 
  mutate(nota = ifelse(cv>=0.25 | total.count<50, "a",""))

write.csv(explo_ocup_nuble, "mas_de_49_horas.csv")
