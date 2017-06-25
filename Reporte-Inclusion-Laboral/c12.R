############################################################################################
# Cuadro 12. Razones de inactividad de los jóvenes que no estudian ni trabajan. Ñuble, 2015
############################################################################################
diseño$variables = mutate(diseño$variables,
                   razones = ifelse(o7r1==1, "Iniciadores", 
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
                          
raz_ninis = svyby(~I(activ==3 & asiste==2 & edad>=15 & edad<=29 & provincia==84), by=~sexo+razones, diseño, svytotal,
                      na.rm=TRUE, multicore=TRUE) %>% 
  `colnames<-` (c("sexo", "razones", "resto_pais", "biobio", "SE_resto", "SE_biobio")) %>% 
  select(sexo, razones, biobio, SE_biobio) %>% 
  mutate(cv = SE_biobio/biobio)

freq = xtabs(~I(activ==3 & asiste==2 & edad>=15 & edad<=29 & provincia==84)+
               sexo+razones, data=diseño$variables) %>% data.frame() %>% 
  `colnames<-` (c("verdadero", "sexo", "razones", "frecuencia")) %>% 
  filter(verdadero == TRUE) 

raz_ninis$frecuencia = freq$frecuencia

write.csv(raz_ninis, "razones_ninis_sexo.csv")
