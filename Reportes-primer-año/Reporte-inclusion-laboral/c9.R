################################################################################
# Cuadro 9. Condición de actividad de los jóvenes según si estudia, Ñuble, 2015 
################################################################################


jov_est_trab = svyby(~I(edad>=15 & edad<=29 ),
                by=~asiste+activ, diseño, svytotal, multicore=TRUE,
                drop.empty.groups=FALSE, na.rm=TRUE) %>% 
                `colnames<-`(c("asiste", "activ", "no_joven",
                               "joven", "SE_no_joven","SE_joven")) %>%
                select(asiste, activ, joven, SE_joven) %>% 
                mutate(cv = SE_joven/joven)


freq =  xtabs(~I(edad>=15 & edad<=29)+asiste+activ, data=diseño$variables) %>%
  data.frame() %>%
  `colnames<-`(c("asiste_activ", "asiste", "activ", "freq")) %>% 
  filter(asiste_activ=="TRUE")

jov_est_trab$frecuencia =  freq$freq

write.csv(jov_est_trab, "c42 jovenes_estudia_trabaja_sexo.csv")


jov_est_trabnuble = svyby(~I(edad>=15 & edad<=29 & provincia==84),
                                           by=~asiste+activ, diseño, svytotal, multicore=TRUE,
                                           drop.empty.groups=FALSE, na.rm=TRUE) %>% 
  `colnames<-`(c("asiste", "activ", "resto_país", "Nuble", "SE.resto", "SE.Nuble")) %>%
  select(asiste, activ, Nuble, SE.Nuble) %>% 
  mutate(cv = SE.Nuble/Nuble)

freq =  xtabs(~I(edad>=15 & edad<=29 & provincia==84)+asiste+activ, data=diseño$variables) %>%
  data.frame() %>%
  `colnames<-`(c("asiste_activ", "asiste", "activ", "freq")) %>% 
  filter(asiste_activ=="TRUE")

jov_est_trabnuble$frecuencia =  freq$freq

write.csv(jov_est_trabnuble, "c42 jovenes_estudia_trabaja_sexo_nuble.csv")

