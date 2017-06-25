############################################################################################
# Cuadro 10. Distribución de jóvenes que no estudian ni trabajan (NINI), según edad y género
############################################################################################

diseño$variables$ tramo_jov = cut(diseño$variables$edad, breaks = c(14,19,24,29),
                                  labels= c("15-19","20-24", "25-29"))

jov_nini_sexo_nacional = svyby(~I(edad>=15 & edad<=29 & asiste==2 & activ==3),
                                   by=~sexo+tramo_jov, diseño, svytotal, multicore=TRUE,
                                   drop.empty.groups=FALSE, na.rm=TRUE) %>% 
  `colnames<-` (c("sexo", "tramo_jov", "no_joven", "joven", "SE_no_joven", "SE_joven")) %>%
  select(sexo, tramo_jov, joven, SE_joven) %>% 
  mutate(cv = SE_joven/joven)

jov_nini_sexo_nuble = svyby(~I(edad>=15 & edad<=29 & asiste==2 & activ==3 & provincia==84),
                                by=~sexo+tramo_jov, diseño, svytotal, multicore=TRUE,
                                drop.empty.groups=FALSE, na.rm=TRUE)%>% 
  `colnames<-` (c("sexo", "tramo_jov", "no_joven", "joven", "SE_no_joven", "SE_joven")) %>%
  select(sexo, tramo_jov, joven, SE_joven) %>% 
  mutate(cv = SE_joven/joven)

freq = xtabs(~I(edad>=15 & edad<=29 & asiste==2 & activ==3 & provincia==84)+sexo+tramo_jov, 
             data=diseño$variables) %>%
  data.frame() %>% 
  `colnames<-` (c("verdadero", "sexo", "tramo_jov", "frecuencia")) %>% 
  filter(verdadero==TRUE)

jov_nini_sexo_nuble$freq = freq$frecuencia

write.csv(jov_nini_sexo_nuble, "jovenes_nini_sexo_nuble.csv")
write.csv(jov_nini_sexo_nacional, "jovenes_nini_sexo_nacional.csv")

