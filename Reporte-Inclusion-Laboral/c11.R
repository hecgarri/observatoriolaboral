###################################################################################################
# Cuadro 11. Nivel educacional de los jóvenes que no están estudiando, según condición 
# de actividad económica, Ñuble, 2015
###################################################################################################

diseño$variables$educ2 = ifelse(diseño$variables$educ==0 | diseño$variables$educ==1, "Sin educación formal",
                          ifelse(diseño$variables$educ==2, "Básica completa", 
                          ifelse(diseño$variables$educ==3 | diseño$variables$educ==4, "Básica completa", 
                          ifelse(diseño$variables$educ==5 | diseño$variables$educ==6, "Media completa", 
                          ifelse(diseño$variables$educ==7, "Media completa", 
                          ifelse(diseño$variables$educ==8, "Técnico Nivel Superior Completo", 
                          ifelse(diseño$variables$educ==9, "Media completa", 
                          ifelse(diseño$variables$educ==10, "Profesional Completo", 
                          ifelse(diseño$variables$educ==11, "Profesional Completo", 
                          ifelse(diseño$variables$educ==12, "Profesional Completo", NA))))))))))

#-------- Economicamente Activos 
educ_jov_num_nacional = svyby(~I((activ==1 | activ==2) & edad>=15 & edad<=29 & asiste==2), by=~educ2,
                                     diseño, svytotal, na.rm=TRUE, multicore=TRUE,
                                     drop.empty.groups = FALSE) %>% 
  `colnames<-` (c("nivel", "No_joven", "joven", "SE_no_joven", "SE_joven")) %>% 
  select(nivel, joven, SE_joven) %>% 
  mutate(cv = SE_joven/joven)

freq = xtabs(~I((activ==1 | activ==2) & edad>=15 & edad<=29 & asiste==2)+educ2, data=diseño$variables) %>% 
  data.frame() %>% 
  `colnames<-` (c("verdadero", "nivel", "frecuencia")) %>% 
  filter(verdadero==TRUE)

educ_jov_num_nacional$frecuencia = freq$frecuencia

educ_jov_num_nuble = svyby(~I((activ==1 | activ==2) & edad>=15 & edad<=29 & asiste==2), by=~educ2,
                                  subset(diseño, provincia==84), svytotal, na.rm=TRUE, multicore=TRUE, drop.empty.groups = FALSE) %>% 
  `colnames<-` (c("nivel", "resto_pais", "nuble", "SE_resto_pais", "SE_nuble")) %>% 
  select(nivel, nuble, SE_nuble) %>%  mutate(cv = SE_nuble/nuble)

freq = xtabs(~I((activ==1 | activ==2) & edad>=15 & edad<=29 & provincia ==84)+educ2, 
             data= diseño$variables) %>% data.frame() %>% 
  `colnames<-` (c("verdadero", "nivel", "frecuencia")) %>% 
  filter(verdadero == TRUE)


educ_jov_num_nuble$frecuencia = freq$frecuencia

educ_jov_num_nuble = mutate(educ_jov_num_nuble,  recomen = ifelse(frecuencia >50 & cv<0.25,
                                                                                "recomendable", 
                                                                                "no recomendable"))

#--------- Economicamente Inactivos 

educ_jov_num_inactivos.nacional = svyby(~I(activ==3 & edad>=15 & edad<=29 & asiste==2),
                                               by=~educ2, diseño, svytotal, na.rm=TRUE, multicore=TRUE,
                                               drop.empty.groups = FALSE) %>% 
  `colnames<-` (c("nivel", "No_joven", "joven", "SE_no_joven", "SE_joven")) %>% 
  select(nivel, joven, SE_joven) %>% 
  mutate(cv = SE_joven/joven)

freq = xtabs(~I(activ==3 & edad>=15 & edad<=29 & asiste==2)+educ2, data=diseño$variables) %>% 
  data.frame() %>% 
  `colnames<-` (c("verdadero", "nivel", "frecuencia")) %>% 
  filter(verdadero==TRUE)

educ_jov_num_inactivos.nacional$frecuencia = freq$frecuencia

educ_jov_num_inactivos.nuble = svyby(~I(activ==3 & edad>=15 & edad<=29 & asiste==2), by=~educ2,
                                            subset(diseño, provincia==84), svytotal, na.rm=TRUE, multicore=TRUE, drop.empty.groups = FALSE) %>% 
  `colnames<-` (c("nivel", "resto_pais", "nuble", "SE_resto_pais", "SE_nuble")) %>% 
  select(nivel, nuble, SE_nuble) %>%  mutate(cv = SE_nuble/nuble)

freq = xtabs(~I(activ==3 & edad>=15 & edad<=29 & provincia ==84)+educ2, 
             data= diseño$variables) %>% data.frame() %>% 
  `colnames<-` (c("verdadero", "nivel", "frecuencia")) %>% 
  filter(verdadero == TRUE)


educ_jov_num_inactivos.nuble$frecuencia = freq$frecuencia

educ_jov_num_inactivos.nuble = mutate(educ_jov_num_inactivos.nuble,  recomen = ifelse(frecuencia >50 & cv<0.25,
                                                                                                    "recomendable", 
                                                                                                    "no recomendable"))

