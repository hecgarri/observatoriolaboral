####################################################################################################################################
# Cuadro 54. Tasa de participación, ocupación y desocupación de personas entre 15 y 60 años de la región, pertenecientes a pueblos indigenas 2015  
#####################################################################################################################################

# --------------------- Tasa de participación 

tasa_participacion_discapacidad = svyby(~I((activ==1 | activ==2) & edad>=15 & edad<60),
                                        denominator=~I(edad>=15 & edad<60), by=~indigena,
                                        diseño, svyratio, na.rm=TRUE, multicore=TRUE) %>% 
  `colnames<-` (c("discapacitado", "tasa_part", "SE"))

freq = xtabs(~I((activ==1 | activ==2) & edad>=15 & edad<60)+discapacitado, data=diseño$variables) %>% 
  data.frame() %>% 
  `colnames<-` (c("verdadero", "discapacitado", "frecuencia")) %>% 
  filter(verdadero==TRUE)

tasa_participacion_discapacidad$frecuencia = freq$frecuencia 

write.csv(tasa_participacion_discapacidad, "tasa_participación_discapacidad.csv")

tasa_participacion_discapacidad.nuble = svyby(~I((activ==1 | activ==2) & edad>=15 & edad<60 & provincia==84),
                                              denominator=~I(edad>=15 & edad<60 & provincia==84), by=~discapacitado,
                                              diseño, svyratio, na.rm=TRUE, multicore=TRUE) %>% 
  `colnames<-` (c("discapacitado", "tasa_part", "SE")) %>% 
  mutate(cv = SE/tasa_part)

freq = xtabs(~I((activ==1 | activ==2) & edad>=15 & edad<60 & provincia==84)+discapacitado, data=diseño$variables) %>% 
  data.frame() %>% 
  `colnames<-` (c("verdadero", "discapacitado", "frecuencia")) %>% 
  filter(verdadero==TRUE)

tasa_participacion_discapacidad.nuble$frecuencia = freq$frecuencia 


write.csv(tasa_participacion_discapacidad.nuble, "tasa_participación_discapacidad.nuble.csv")

