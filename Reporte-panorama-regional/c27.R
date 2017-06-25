# CREACION DE LA VARIABLE: SUBEMPLEO
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, subempleo =
                              ifelse(c1==2 & c10==1 & c11==1 & habituales<=30,1,0))
}

# CREACION DE LA VARIABLE: SUBEMPLEO
info2$variables = mutate(info2$variables, subempleo =
                                 ifelse(c1==2 & c10==1 & c11==1 & habituales<=30,1,0))

#-------------------------------------------------------------------------------
### CUADRO 27: Porcentaje de subempleo involuntario en los ocupados de Ñuble, 2010-2016
#-------------------------------------------------------------------------------
#################################################
######## Subempleo Mujeres nacional
#################################################
#
tasa.subempleo.mujeres.nacional = svyratio(~I(subempleo==1 & sexo==2), 
                                           denominator=~I(cae_general=="Ocupado"), 
                                           info2, multicore = TRUE, 
                                           rop.empty.groups = FALSE, na.rm=TRUE) %>%
  unlist() %>% t() %>% data.frame() %>% 
  mutate(var=as.numeric(var), SE = sqrt(var)) 

freq= xtabs(~I(subempleo==1 & sexo==2), data=info2$variables) %>% data.frame() %>% 
  `colnames<-` (c("subeempleado", "freq")) %>% 
  filter(subeempleado == TRUE)

tasa.subempleo.mujeres.nacional$cv = 
  tasa.subempleo.mujeres.nacional$`SE`/tasa.subempleo.mujeres.nacional$`ratio`

tasa.subempleo.mujeres.nacional$frecuencia = freq$freq

names(tasa.subempleo.mujeres.nacional) = 
  c("tasa.subempleo.mujeres.nacional","var", "error estandar","cv","frecuencia")

write.csv(tasa.subempleo.mujeres.nacional, "tasa_subempleo_mujeres_nacional.csv")

#################################################
######## Subempleo Hombres nacional
#################################################
#
tasa.subempleo.hombres.nacional = svyratio(~I(subempleo==1 & sexo==1), 
                                           denominator=~I(cae_general=="Ocupado"), 
                                           info2, multicore = TRUE, 
                                           drop.empty.groups = FALSE, na.rm=TRUE) %>% 
  
  unlist() %>% t() %>% data.frame() %>% 
  mutate(var=as.numeric(var), SE = sqrt(var)) 

freq= xtabs(~I(subempleo==1 & sexo==1), data=info2$variables) %>% data.frame() %>% 
  `colnames<-` (c("subeempleado", "freq")) %>% 
  filter(subeempleado == TRUE)

tasa.subempleo.hombres.nacional$cv = 
  tasa.subempleo.hombres.nacional$`SE`/tasa.subempleo.hombres.nacional$`ratio`

tasa.subempleo.hombres.nacional$frecuencia = freq$freq

names(tasa.subempleo.hombres.nacional) = 
  c("tasa.subempleo.hombres.nacional","var", "error estandar","cv","frecuecia")

write.csv(tasa.subempleo.hombres.nacional, "tasa_subempleo_hombres_nacional.csv")

#########################################################
######## Subempleo promedio Nacional
#########################################################
#
tasa.promedio.subempleo.nacional = svyratio(~I(subempleo==1), 
                                            denominator=~I(cae_general=="Ocupado"), 
                                            info2, multicore = TRUE, 
                                            drop.empty.groups = FALSE, na.rm=TRUE) %>% 
  unlist() %>% t() %>% data.frame() %>% 
  mutate(var=as.numeric(var), SE = sqrt(var)) 

freq= xtabs(~I(subempleo==1), data=info2$variables) %>% data.frame() %>% 
  `colnames<-` (c("subeempleado", "freq")) %>% 
  filter(subeempleado == TRUE)

tasa.promedio.subempleo.nacional$cv = 
  tasa.promedio.subempleo.nacional$`SE`/tasa.promedio.subempleo.nacional$`ratio`

tasa.promedio.subempleo.nacional$frecuencia = freq$freq

names(tasa.promedio.subempleo.nacional) = 
  c("tasa.promedio.subempleo.nacional","var", "error estandar", "cv", "frecuencia")

write.csv(tasa.promedio.subempleo.nacional, "tasa_promedio_subempleo_nacional.csv")

#################################################
######## Subempleo Mujeres Nuble
#################################################
#
tasa.subempleo.mujeres.nuble = svyratio(~I(subempleo==1 & sexo==2 & prov_e==84), 
                                        denominator=~I(prov_e==84), 
                                        info2, multicore = TRUE, 
                                        drop.empty.groups = FALSE, na.rm=TRUE) %>% 
  unlist() %>% t() %>% data.frame() %>% 
  mutate(var=as.numeric(var), SE = sqrt(var)) 

freq= xtabs(~I(subempleo==1 & sexo==2 & prov_e==84), data=info2$variables) %>% data.frame() %>% 
  `colnames<-` (c("subeempleado", "freq")) %>% 
  filter(subeempleado == TRUE)

tasa.subempleo.mujeres.nuble$cv = 
  tasa.subempleo.mujeres.nuble$`SE`/tasa.subempleo.mujeres.nuble$`ratio`

tasa.subempleo.mujeres.nuble$frecuencia = freq$freq

names(tasa.subempleo.mujeres.nuble) = 
  c("tasa.subempleo.mujeres.nuble","var", "error estandar", "cv", "frecuencia")

write.csv(tasa.subempleo.mujeres.nuble, "tasa_subempleo_mujeres_nuble.csv")

#################################################
######## Subempleo Hombres nuble
#################################################
#
tasa.subempleo.hombres.nuble = svyratio(~I(subempleo==1 & sexo==1 & prov_e==84), 
                                        denominator=~I(prov_e==84), 
                                        info2, multicore = TRUE, 
                                        drop.empty.groups = FALSE, na.rm=TRUE) %>% 
  unlist() %>% t() %>% data.frame() %>% 
  mutate(var=as.numeric(var), SE = sqrt(var)) 

freq= xtabs(~I(subempleo==1 & sexo==1 & prov_e==84), data=info2$variables) %>% data.frame() %>% 
  `colnames<-` (c("subeempleado", "freq")) %>% 
  filter(subeempleado == TRUE)

tasa.subempleo.hombres.nuble$cv = 
  tasa.subempleo.hombres.nuble$`SE`/tasa.subempleo.hombres.nuble$`ratio`

tasa.subempleo.hombres.nuble$frecuencia = freq$freq

names(tasa.subempleo.hombres.nuble) = 
  c("tasa.subempleo.hombres.nuble","var", "error estandar", "cv", "frecuencia")

write.csv(tasa.subempleo.hombres.nuble, "tasa_subempleo_hombres_nuble.csv")

#########################################################
######## Subempleo promedio de nuble
#########################################################
#
tasa.promedio.subempleo.nuble = svyratio(~I(subempleo==1 & prov_e==84), 
                                         denominator=~(prov_e==84), 
                                         info2, multicore = TRUE, 
                                         drop.empty.groups = FALSE, na.rm=TRUE) %>% 
  
  unlist() %>% t() %>% data.frame() %>% 
  mutate(var=as.numeric(var), SE = sqrt(var)) 

freq= xtabs(~I(subempleo==1 & prov_e==84), data=info2$variables) %>% data.frame() %>% 
  `colnames<-` (c("subeempleado", "freq")) %>% 
  filter(subeempleado == TRUE)

tasa.promedio.subempleo.nuble$cv = 
  tasa.promedio.subempleo.nuble$`SE`/tasa.promedio.subempleo.nuble$`ratio`

tasa.promedio.subempleo.nuble$frecuencia = freq$freq

names(tasa.promedio.subempleo.nuble) = 
  c("tasa.promedio.subempleo.nuble","var", "error estandar", "cv", "frecuencia")

write.csv(tasa.promedio.subempleo.nuble, "tasa_promedio_subempleo_nuble.csv")

### FIN CUADRO 27
