#####################################################
# % sobre el total de ocupados de la región
####################################################

#p_ocup_nuble = Porcentaje que representan las principales ocupaciones 
#con respecto al total de trabajadores

p_ocup_nuble = lapply(as.character(ocupaciones4$codigo),
                      function(x) svyratio(~I(oficio4==x),denominator=~I(activ==1),
                                           subset(diseño, provincia==84), na.rm=TRUE))

p_ocup_nuble. = unlist(lapply(p_ocup_nuble, '[[', 1) ) 

ee.p_ocup_nuble = unlist(lapply(p_ocup_nuble, SE))

p_ocup_nuble.. = data.frame(p_ocup_nuble., ee.p_ocup_nuble) %>% 
  `colnames<-` (c("ocupados", "se")) %>% 
  mutate(cv=se/ocupados, ocupados=round(ocupados*100,1), codigo = ocupaciones4$codigo)

temp = xtabs(~oficio4, subset(diseño$variables, provincia==84)) %>%
  data.frame() %>%   filter(oficio4 == 1314 |                                
                              oficio4 == 2331 | oficio4 == 4115 |                                                                  
                              oficio4 == 5122 | oficio4 == 5131 |                            
                              oficio4 == 5220 | oficio4 == 6111 |
                              oficio4 == 6112 | oficio4 == 6210 |                                                           
                              oficio4 == 7124 | oficio4 == 8322 |
                              oficio4 == 8324 | oficio4 == 9131 | 
                              oficio4 == 9132 | oficio4 == 9211)  

p_ocup_nuble..$frecuencia = temp$Freq

p_ocup_nuble = merge(p_ocup_nuble.., ciuo, by.x="codigo", by.y="codigo")

write.csv(p_ocup_nuble, "porcentaje_ocupados.csv")
