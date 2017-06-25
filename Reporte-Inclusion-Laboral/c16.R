###############################################################################################################
# Cuadro 16. Inactivos entre 15 y 60 años con discapacidad y sin discapacidad, según razón de inactividad 2015 
###############################################################################################################

diseño$variables = mutate(diseño$variables, discapacitado = ifelse(s34_1a!=1 |s34_1b!=1 |
                                  s34_1c!=1 |
                                  s34_1d!=1 |
                                  s34_1e!=1 |
                                  s34_1f!=1 |
                                  s34_1g!=1 |
                                  s34_1h!=1 |
                                  s34_1i!=1 |
                                  s34_1j!=1,1, 0))

diseño$variables = mutate(diseño$variables, razones = ifelse(o7r1==1, "Iniciadores", 
                            ifelse(o7r1==3 | o7r1==4 |
                            o7r1==5, "Cuidado de hijos o terceros",
                            ifelse(o7r1==6, "Enfermedad o discapacidad",
                            ifelse(o7r1==7 | o7r1==8 |
                            o7r1==9 | o7r1==4, "Percepción negativa del mercado laboral",
                            ifelse(o7r1==10, "Quehaceres del hogar", 
                            ifelse(o7r1==15, "Busca cuando lo necesita", 
                            ifelse(o7r1==16, "No tiene interés",
                            ifelse(o7r1==11, "Estudiante",
                            ifelse(o7r1==13 | o7r1==14, "Tiene otra fuente de ingreso",
                            ifelse(!is.na(o7r1), "Otra razón", NA)))))))))))


numero_inactivos_discapacidad_razones = svyby(~I(activ==3 & provincia==84), by=~razones+discapacitado, diseño, svytotal, 
                                              na.rm=TRUE, multicore = TRUE, drop.empty.groups = FALSE) %>%
  `colnames<-` (c("razones", "discapacitado", "no_inactivo", "inactivo", "SE_no_inactivo", "SE_inactivo")) %>% 
  select(razones, discapacitado, inactivo, SE_inactivo) %>% 
  mutate(cv = SE_inactivo/inactivo)

freq = xtabs(~I(activ==3 & provincia==84)+razones+discapacitado, data=diseño$variables) %>% data.frame() %>% 
  `colnames<-` (c("verdadero", "razones", "discapacitado", "frecuencia")) %>% 
  filter(verdadero==TRUE)

numero_inactivos_discapacidad_razones$frecuencia = freq$frecuencia

write.csv(numero_inactivos_discapacidad_razones, "razones_inactivos_discapacidad.csv", fileEncoding = "WINDOWS-1252")
