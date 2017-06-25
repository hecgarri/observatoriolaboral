########################################################################################
# % de trabajadores que se capacitó en los últimos 12 meses
########################################################################################

capacita_ocup_nuble = lapply(as.character(ocupaciones4$codigo),
                             function(x) svyby(~I(o30==1), denominator=~I(activ==1),
                                               subset(diseño, provincia==84), by=~I(oficio4==x),
                                               svyratio, na.rm=TRUE,drop.empty.groups = FALSE))

capacita_ocup_nuble. = unlist(lapply(capacita_ocup_nuble, '[[', 2,2)) 

ee.capacita_ocup_nuble = unlist(lapply(capacita_ocup_nuble,'[[',2,3))

capacita_ocup_nuble.. = data.frame(capacita_ocup_nuble., ee.capacita_ocup_nuble) %>% 
  `colnames<-` (c("porcentaje", "se")) %>% 
  mutate(cv=se/porcentaje, porcentaje=round(porcentaje*100,1), codigo = ocupaciones4$codigo) 

temp = subset(diseño$variables, provincia==84) %>% group_by(oficio4, I(o30==1)) %>%
  summarise(total.count = n()) %>% 
  filter(oficio4 == 1314 |                                
           oficio4 == 2331 | oficio4 == 4115 |                                                                  
           oficio4 == 5122 | oficio4 == 5131 |                            
           oficio4 == 5220 | oficio4 == 6111 |
           oficio4 == 6112 | oficio4 == 6210 |                                                           
           oficio4 == 7124 | oficio4 == 8322 |
           oficio4 == 8324 | oficio4 == 9131 | 
           oficio4 == 9132 | oficio4 == 9211,
         `I(o30 == 1)`==TRUE) 

capacita_ocup_nuble = merge(capacita_ocup_nuble.., temp, by.x="codigo", by.y='oficio4')
capacita_ocup_nuble = merge(capacita_ocup_nuble, ciuo, by.x="codigo", by.y="codigo") %>% 
  mutate(nota = ifelse(cv>=25 | total.count<50,"a",""))

write.csv(capacita_ocup_nuble, "capacitación.csv")

