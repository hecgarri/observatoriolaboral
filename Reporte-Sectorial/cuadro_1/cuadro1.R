#f. Características generales: edad promedio, años de escolaridad promedio,
#% de mujeres, % con educación superior completa y promedio de ingresos de
#la ocupación principal del año más reciente. Estas cifras se deben comparar
#con datos a nivel de región que engloban a todos los sectores económicos
#presentes. Base de datos: ENEtrim y ESI, en donde se utilizan variables
#como edad, sexo, región de trabajo –código
#de la región donde se ubica el trabajo del individuo. 

setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/Reporte Sectorial/Reporte-Sectorial-/")

edad_sector = lapply(1:4, function(x) svyby(~edad,by=~sector,
                      design = subset(info[[x]], prov_e==84),
                      svymean, na.rm=TRUE, na.rm.all = TRUE, 
                      drop.empty.groups = TRUE))
edad_sector = do.call(rbind, edad_sector) %>% 
  mutate(mes = rep(1:4, each=12), cv = round((se/edad)*100,2))

frecuencia = lapply(1:4, function(x) subset(info[[x]]$variables, prov_e==84) %>% 
  group_by(sector) %>% summarise(n()))

frecuencia = do.call(rbind, frecuencia) %>% 
  data.frame() %>% mutate(mes = rep(1:4, each=12))

edad_sector = merge(edad_sector, frecuencia, by=c("sector", "mes"))

edad_ = lapply(1:4, function(x) svymean(~edad,
                      design = subset(info[[x]], prov_e==84),
                      na.rm=TRUE, na.rm.all = TRUE)) 
edad_ =  cbind(unlist(lapply(edad_, '[[', 1)),unlist(lapply(edad_, SE))) %>% 
  data.frame() %>% rename(edad =X1, se=X2) %>% mutate(cv = round((se/edad)*100,2))

  
write.csv(edad_sector, "c1 edad sector.csv")  
write.csv(edad_, "c1 edad.csv")

esc_sector = lapply(1:4, function(x) svyby(~esc,by=~sector,
                                            design = subset(info[[x]], prov_e==84),
                                            svymean, na.rm=TRUE, na.rm.all = TRUE, 
                                            drop.empty.groups = TRUE))
esc_sector = do.call(rbind, esc_sector) %>% 
  mutate(mes = rep(1:4, each=12), cv = round((se/esc)*100,2))

frecuencia = lapply(1:4, function(x) subset(info[[x]]$variables, prov_e==84) %>% 
                      group_by(sector) %>% summarise(n()))

frecuencia = do.call(rbind, frecuencia) %>% 
  data.frame() %>% mutate(mes = rep(1:4, each=12))

esc_sector = merge(esc_sector, frecuencia, by=c("sector", "mes"))

esc_ = lapply(1:4, function(x) svymean(~esc,
                                        design = subset(info[[x]], prov_e==84),
                                        na.rm=TRUE, na.rm.all = TRUE)) 
esc_ =  cbind(unlist(lapply(esc_, '[[', 1)),unlist(lapply(esc_, SE))) %>% 
  data.frame() %>% rename(esc =X1, se=X2) %>% mutate(cv = round((se/esc)*100,2))

write.csv(esc_sector, "c1 escolaridad sector.csv")  
write.csv(esc_, "c1 escolaridad.csv")

#################################################################################
# % de mujeres
#################################################################################

mujeres_sector = lapply(1:4, function(x) svyby(~I(sexo==2), denominator =~I(cae_general=="Ocupado"),
                  by=~sector, design = subset(info[[x]], prov_e==84),
                  svyratio, na.rm=TRUE, na.rm.all = TRUE, drop.empty.groups = TRUE))
mujeres_sector = do.call(rbind, mujeres_sector)  %>% `colnames<-` (c("sector", "mujeres", "se")) %>% 
  mutate(mes = rep(1:4, each=12), cv = round((se/mujeres)*100,2))

frecuencia = lapply(1:4, function(x) subset(info[[x]]$variables, prov_e==84) %>% 
                      group_by(sector, sexo) %>% summarise(n()))
frecuencia = do.call(rbind, frecuencia) %>% filter(sexo==2) %>%  
  data.frame() %>% mutate(mes = rep(1:4, each=11))

mujeres_sector = merge(mujeres_sector, frecuencia, by=c("sector", "mes"))

mujeres_ = lapply(1:4, function(x) svyratio(~I(sexo==2), ~I(cae_general=="Ocupado"),
                                        design = subset(info[[x]], prov_e==84),
                                        na.rm=TRUE, na.rm.all = TRUE)) 
mujeres_ =  cbind(unlist(lapply(mujeres_, '[[', 1)),unlist(lapply(mujeres_, SE))) %>% 
  data.frame() %>% rename(mujeres =X1, se=X2) %>% mutate(cv = round((se/mujeres)*100,2))

write.csv(mujeres_sector, "c1 mujeres sector.csv")
write.csv(mujeres_, "c1 mujeres.csv")

#################################################################################
# % de personas con educación superior
#################################################################################

superior_sector = lapply(1:4, function(x) svyby(~I(educ3==3 | educ3==4), denominator =~I(cae_general=="Ocupado"),
                           by=~sector, design = subset(info[[x]], prov_e==84),
                           svyratio, na.rm=TRUE, na.rm.all = TRUE, drop.empty.groups = TRUE))
superior_sector = do.call(rbind, superior_sector)  %>% `colnames<-` (c("sector", "superior", "se")) %>% 
  mutate(mes = rep(1:4, each=12), cv = round((se/superior)*100,2))

frecuencia = lapply(1:4, function(x) xtabs(~I(educ3==3 | educ3==4)+sector,
             data = subset(info[[x]]$variables, prov_e==84))%>% data.frame()) 
  
frecuencia = do.call(rbind, frecuencia) %>% data.frame() %>% 
  rename(superior = I.educ3....3...educ3....4., n = Freq) %>%
  filter(superior==TRUE) %>% mutate(mes = rep(1:4, each=13))

superior_sector = merge(superior_sector, frecuencia, by=c("sector", "mes"))

superior_ = lapply(1:4, function(x) svyratio(~I(educ3==3 | educ3==4), ~I(cae_general=="Ocupado"),
                                            design = subset(info[[x]], prov_e==84),
                                            na.rm=TRUE, na.rm.all = TRUE)) 
superior_ =  cbind(unlist(lapply(superior_, '[[', 1)),unlist(lapply(superior_, SE))) %>% 
  data.frame() %>% rename(superior =X1, se=X2) %>% mutate(cv = round((se/superior)*100,2))

write.csv(superior_sector, "c1 educación superior por sector.csv")
write.csv(superior_, "c1 educación superior.csv")
