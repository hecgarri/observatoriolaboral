########################################################################################
# % Con educación superior completa (técnicas o profesional)
########################################################################################

diseño$variables = mutate(diseño$variables,
                          nivel_educ = ifelse(educ==0 | educ==0, "Básica incompleta o inferior",
                                              ifelse(educ==3 | educ==3| educ==4, "Básica completa",
                                                     ifelse(educ==5 | educ==6| educ==7 | educ==9, "Media completa", 
                                                            ifelse(educ==8 | educ==10 | educ==11 |educ==12, "Profesional completa", 
                                                                   NA)))))

educ_ocup_nuble = lapply(as.character(ocupaciones4$codigo),
                         function(x) svyby(~I(nivel_educ=="Profesional completa"),
                                           denominator=~I(activ==1), subset(diseño, provincia==84),
                                           by=~I(oficio4==x),svyratio, na.rm=TRUE,drop.empty.groups = FALSE))

educ_ocup_nuble. = unlist(lapply(educ_ocup_nuble, '[[', 2,2)) 

ee.educ_ocup_nuble = unlist(lapply(educ_ocup_nuble,'[[',2,3))

educ_ocup_nuble.. = data.frame(educ_ocup_nuble., ee.educ_ocup_nuble) %>% 
  `colnames<-` (c("ocupados", "se")) %>% 
  mutate(cv=se/ocupados, ocupados=round(ocupados*100,1), codigo = ocupaciones4$codigo) 

educ_ocup_nuble = merge(educ_ocup_nuble.., ciuo, by.x="codigo", by.y="codigo")

write.csv(educ_ocup_nuble, "con_educacion_superior.csv")
