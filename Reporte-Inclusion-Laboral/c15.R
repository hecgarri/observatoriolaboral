##############################################################################################
# Cuadro 15. Características generales de ocupados de Ñuble en situación de discapacidad, 2015 
##############################################################################################

diseño$variables = mutate(diseño$variables, 
                  discapacitado = ifelse(s34_1a!=1 | s34_1b!=1 | s34_1c!=1 |
                                         s34_1d!=1 | s34_1e!=1 | s34_1f!=1 |
                                         s34_1g!=1 | s34_1h!=1 | s34_1i!=1 |
                                         s34_1j!=1,1, 0))
                          
#---- Número de ocupados 

num_ocup_dis = svyby(~I(activ==1), by=~discapacitado, subset(diseño, provincia==84), 
                                     na.rm=TRUE, multicore=TRUE, svytotal) %>% 
  `colnames<-` (c("discapacitado", "no ocupado", "ocupado", "SE_no_ocupado", "SE_ocupado")) %>% 
  select(ocupado, SE_ocupado) %>% 
  mutate(cv = SE_ocupado/ocupado) 

freq = xtabs(~I(activ==1 & discapacitado==1), data=diseño$variables) %>% data.frame() %>% 
  `colnames<-` (c("verdadero", "frecuencia")) %>%  filter(verdadero==TRUE)

num_ocup_dis$frecuencia = freq$frecuencia

write.csv(num_ocup_dis, "numero_ocupados_discapacidad.csv", fileEncoding = "WINDOWS-1252")

num_ocup_dis.nuble = svyby(~I(activ==1 & provincia==84), by=~discapacitado, diseño, 
                                           na.rm=TRUE, multicore=TRUE, svytotal) %>% 
  `colnames<-` (c("discapacitado", "resto_pais", "ocupado", "SE_resto", "SE_ocupado")) %>% 
  select(ocupado, SE_ocupado) %>% 
  mutate(cv = SE_ocupado/ocupado) 

freq = xtabs(~I(activ==1 & discapacitado==1 & provincia==84), data=diseño$variables) %>% data.frame() %>% 
  `colnames<-` (c("verdadero", "frecuencia")) %>%  filter(verdadero==TRUE)

num_ocup_dis.nuble$frecuencia = freq$frecuencia

write.csv(num_ocup_dis.nuble, "numero_ocupados_discapacidad.nuble.csv", fileEncoding = "WINDOWS-1252")

#--------- Edad promedio -----------------------------

edad.ocupados.discapacidad = svyby(~edad, by=~activ+I(discapacitado==1), subset(diseño, provincia==84), svymean, 
                                   na.rm=TRUE, multicore=TRUE) %>% 
  `colnames<-` (c("activ", "discapacitado", "edad", "SE")) %>% 
  filter(activ==1) %>% 
  mutate(cv = SE/discapacitado)

freq = xtabs(~activ+I(discapacitado==1), data=diseño$variables) %>% data.frame() %>% 
  `colnames<-` (c("activ", "discapacitado", "frecuencia")) %>% 
  filter(activ==1) 

edad.ocupados.discapacidad$frecuencia = freq$frecuencia

write.csv(edad.ocupados.discapacidad, "edad_ocupados_discapacidad.csv")

edad.ocupados.discapacidad.nuble = svyby(~edad, by=~activ+I(discapacitado==1 & provincia==84), diseño, svymean, 
                                         na.rm=TRUE, multicore=TRUE) %>% 
  `colnames<-` (c("activ", "discapacitado", "edad", "SE")) %>% 
  filter(activ==1) %>% 
  mutate(cv = SE/discapacitado)

freq = xtabs(~activ+I(discapacitado==1 & provincia==84), data=diseño$variables) %>% data.frame() %>% 
  `colnames<-` (c("activ", "discapacitado", "frecuencia")) %>% 
  filter(activ==1) 

edad.ocupados.discapacidad.nuble$frecuencia = freq$frecuencia

write.csv(edad.ocupados.discapacidad.nuble, "edad_ocupados_discapacidad.nuble.csv")

#--------- Escolaridad promedio -----------------------------

escolaridad.ocupados.discapacidad = svyby(~esc, by=~activ+discapacitado, subset(diseño, provincia==84), svymean, 
                                          na.rm=TRUE, multicore=TRUE) %>% 
  filter(activ==1)

freq = xtabs(~activ+discapacitado, data=subset(diseño$variables, provincia==84)) %>%  data.frame() %>% 
  filter(activ==1)

escolaridad.ocupados.discapacidad$frecuencia = freq$Freq

write.csv(escolaridad.ocupados.discapacidad, "escolaridad_ocupados_discapacidad.csv")

escolaridad.ocupados.discapacidad.nuble = svyby(~esc, by=~activ+discapacitado+I(provincia==84), diseño, svymean, 
                                                na.rm=TRUE, multicore=TRUE) %>% 
  filter(activ==1,`I(provincia == 84)`==TRUE)

freq = xtabs(~activ+discapacitado+I(provincia==84), data=diseño$variables) %>%  data.frame() %>% 
  filter(activ==1, I.provincia....84.==TRUE)

escolaridad.ocupados.discapacidad.nuble$frecuencia = freq$Freq

write.csv(escolaridad.ocupados.discapacidad.nuble, "escolaridad_ocupados_discapacidad.nuble.csv")

#--------- Porcentaje de mujeres -----------------------------

mujeres.ocupados.discapacidad = svyby(~I(activ==1 & sexo==2), denominator=~I(activ==1),
                                      by=~I(discapacitado==1), subset(diseño, provincia==84), svyratio, 
                                      na.rm=TRUE, multicore=TRUE) %>% 
  `colnames<-` (c("discapacitado", "mujeres", "SE")) %>% 
  mutate(cv = SE/mujeres)

freq = xtabs(~I(activ==1 & sexo==2)+discapacitado, data=diseño$variables) %>% data.frame() %>% 
  `colnames<-` (c("verdadero", "discapacitado", "frecuencia")) %>% 
  filter(verdadero == TRUE)

mujeres.ocupados.discapacidad$frecuencia = freq$frecuencia

write.csv(mujeres.ocupados.discapacidad, "mujeres_ocupados_discapacidad.csv", fileEncoding = "WINDOWS-1252")

mujeres.ocupados.discapacidad.nuble = svyby(~I(activ==1 & sexo==2 & provincia==84), denominator=~I(activ==1 & provincia==84), by=~I(discapacitado==1), diseño, svyratio, 
                                            na.rm=TRUE, multicore=TRUE) %>% 
  `colnames<-` (c("discapacitado", "mujeres", "SE")) %>% 
  mutate(cv = SE/mujeres)

freq = xtabs(~I(activ==1 & sexo==2 & provincia == 84)+discapacitado, data=diseño$variables) %>% data.frame() %>% 
  `colnames<-` (c("verdadero", "discapacitado", "frecuencia")) %>% 
  filter(verdadero == TRUE)

mujeres.ocupados.discapacidad.nuble$frecuencia = freq$frecuencia

write.csv(mujeres.ocupados.discapacidad.nuble, "mujeres_ocupados_discapacidad.nuble.csv", fileEncoding = "WINDOWS-1252")
