##############################################################################################
# Cuadro 19. Características generales de ocupados de Ñuble pertenecientes a pueblos indígenas, 2015 
##############################################################################################

diseño$variables = mutate(diseño$variables,
                   indigena =  ifelse(r3!=10 & r3!=99, "Indigena",
                               ifelse(r3==10, "No indigena",NA)))


numero_ocupados_indigena = svyby(~I(activ==1), by=~indigena, diseño, 
                                 na.rm=TRUE, multicore=TRUE, svytotal) %>% 
  `colnames<-` (c("indigena", "no ocupado", "ocupado", "SE_no_ocupado", "SE_ocupado")) %>% 
  select(ocupado, SE_ocupado) %>% 
  mutate(cv = SE_ocupado/ocupado) 

freq = xtabs(~I(activ==1 & indigena=="Indigena"), data=diseño$variables) %>% data.frame() %>% 
  `colnames<-` (c("verdadero", "frecuencia")) %>%  filter(verdadero==TRUE)

numero_ocupados_indigena$frecuencia = freq$frecuencia

write.csv(numero_ocupados_indigena, "numero_ocupados_indigena.csv", fileEncoding = "WINDOWS-1252")

numero_ocupados_indigena.nuble = svyby(~I(activ==1), by=~indigena, subset(diseño, provincia==84), 
                                       na.rm=TRUE, multicore=TRUE, svytotal) %>% 
  `colnames<-` (c("indigena", "resto_pais", "ocupado", "SE_resto", "SE_ocupado")) %>% 
  select(ocupado, SE_ocupado) %>% 
  mutate(cv = SE_ocupado/ocupado) 

freq = xtabs(~I(activ==1 & indigena=="Indigena" & provincia==84), data=diseño$variables) %>% data.frame() %>% 
  `colnames<-` (c("verdadero", "frecuencia")) %>%  filter(verdadero==TRUE)

numero_ocupados_indigena.nuble$frecuencia = freq$frecuencia

write.csv(numero_ocupados_indigena.nuble, "numero_ocupados_indigena.nuble.csv", fileEncoding = "WINDOWS-1252")

#--------- Edad promedio -----------------------------

edad_ocupados_indigena = svyby(~edad, by=~activ+I(indigena=="Indigena"), diseño, svymean, 
                               na.rm=TRUE, multicore=TRUE) %>% 
  `colnames<-` (c("activ", "indigena", "edad", "SE")) %>% 
  filter(activ==1) %>% 
  mutate(cv = SE/indigena)

freq = xtabs(~activ+I(indigena=="Indigena"), data=diseño$variables) %>% data.frame() %>% 
  `colnames<-` (c("activ", "indigena", "frecuencia")) %>% 
  filter(activ==1) 

edad_ocupados_indigena$frecuencia = freq$frecuencia

write.csv(edad_ocupados_indigena, "edad_ocupados_indigena.csv")

edad_ocupados_indigena.nuble = svyby(~edad, by=~activ+I(indigena=="Indigena" & provincia==84), diseño, svymean, 
                                     na.rm=TRUE, multicore=TRUE) %>% 
  `colnames<-` (c("activ", "indigena", "edad", "SE")) %>% 
  filter(activ==1) %>% 
  mutate(cv = SE/indigena)

freq = xtabs(~activ+I(indigena=="Indigena" & provincia==84), data=diseño$variables) %>% data.frame() %>% 
  `colnames<-` (c("activ", "indigena", "frecuencia")) %>% 
  filter(activ==1) 

edad_ocupados_indigena.nuble$frecuencia = freq$frecuencia

write.csv(edad_ocupados_indigena.nuble, "edad_ocupados_indigena.nuble.csv")

#--------- Escolaridad promedio -----------------------------

escolaridad_ocupados_indigena = svyby(~esc, by=~activ+indigena, diseño, svymean, 
                                      na.rm=TRUE, multicore=TRUE) %>% 
  filter(activ==1)

freq = xtabs(~activ+indigena, data=subset(diseño$variables, provincia==84)) %>%  data.frame() %>% 
  filter(activ==1)

escolaridad_ocupados_indigena$frecuencia = freq$Freq

write.csv(escolaridad_ocupados_indigena, "escolaridad_ocupados_indigena.csv")

escolaridad_ocupados_indigena.nuble = svyby(~esc, by=~activ+indigena+I(provincia==84), diseño, svymean, 
                                            na.rm=TRUE, multicore=TRUE) %>% 
  filter(activ==1,`I(provincia == 84)`==TRUE)

freq = xtabs(~activ+indigena+I(provincia==84), data=diseño$variables) %>%  data.frame() %>% 
  filter(activ==1, I.provincia....84.==TRUE)

escolaridad_ocupados_indigena.nuble$frecuencia = freq$Freq

write.csv(escolaridad_ocupados_indigena.nuble, "escolaridad_ocupados_indigena.nuble.csv")

#--------- Porcentaje de mujeres -----------------------------

mujeres_ocupados_indigena = svyby(~I(activ==1 & sexo==2), denominator=~I(activ==1),
                                  by=~I(indigena=="Indigena"), diseño, svyratio, 
                                  na.rm=TRUE, multicore=TRUE) %>% 
  `colnames<-` (c("indigena", "mujeres", "SE")) %>% 
  mutate(cv = SE/mujeres)

freq = xtabs(~I(activ==1 & sexo==2)+indigena, data=diseño$variables) %>% data.frame() %>% 
  `colnames<-` (c("verdadero", "indigena", "frecuencia")) %>% 
  filter(verdadero == TRUE)

mujeres_ocupados_indigena$frecuencia = freq$frecuencia

write.csv(mujeres_ocupados_indigena, "mujeres_ocupados_indigena.csv", fileEncoding = "WINDOWS-1252")

mujeres_ocupados_indigena.nuble = svyby(~I(activ==1 & sexo==2 & provincia==84), denominator=~I(activ==1 & provincia==84), by=~I(indigena=="Indigena"), diseño, svyratio, 
                                        na.rm=TRUE, multicore=TRUE) %>% 
  `colnames<-` (c("indigena", "mujeres", "SE")) %>% 
  mutate(cv = SE/mujeres)

freq = xtabs(~I(activ==1 & sexo==2 & provincia == 84)+indigena, data=diseño$variables) %>% data.frame() %>% 
  `colnames<-` (c("verdadero", "indigena", "frecuencia")) %>% 
  filter(verdadero == TRUE)

mujeres_ocupados_indigena.nuble$frecuencia = freq$frecuencia

write.csv(mujeres_ocupados_indigena.nuble, "mujeres_ocupados_indigena.nuble.csv", fileEncoding = "WINDOWS-1252")

