## Distribución por tramo de edad y sexo del año más reciente,
##destacando qué tramo de edad concentra más trabajadores de cada sexo

# 15-25
# 26-35
# 36-45
# 46-55
# 56-65
# 66

setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/Reporte Sectorial/Reporte-Sectorial-/")

for (i in 1:4){
info[[i]]$variables =  mutate(info[[i]]$variables, 
                 tramos_edad = ifelse(edad>=15 & edad<=25, 1, 
                                ifelse(edad>=26 & edad<=35, 2,
                                ifelse(edad>=36 & edad<=45,3, 
                                ifelse(edad>=46 & edad<=55,4, 
                                ifelse(edad>=56 & edad<=65,5, 
                                ifelse(edad>=66,6,NA)))))))
}


tramos_etarios = lapply(1:4, function(x) svyby(~I(cae_general=="Ocupado"), by=~sector+tramos_edad, 
                       subset(info[[x]], prov_e==84), svytotal) %>% mutate(mes=x))

tramos_etarios = do.call(rbind, tramos_etarios) %>%
  `colnames<-` (c("sector", "tramos_edad", "no trabaja", "trabaja", "se no trabaja", "se trabaja", "mes")) %>% 
  select(sector, tramos_edad, `trabaja`, `se trabaja`, mes) %>% 
  mutate(cv = round((`se trabaja`/trabaja)*100,2))

frecuencia = lapply(1:4, function(x) xtabs(~sector+tramos_edad,
             data=subset(info[[x]]$variables, prov_e==84)) %>% data.frame() %>% mutate(mes=x))

frecuencia = do.call(rbind, frecuencia)

tramos_etarios = merge(tramos_etarios, frecuencia, by=c("sector", "tramos_edad", "mes"))

write.csv(tramos_etarios, "c2 tramos etarios sector.csv")

##########################################
# Tramos etarios y sexo 
##########################################

tramos_etarios_sexo = lapply(1:4, function(x) svyby(~I(cae_general=="Ocupado"), by=~sector+tramos_edad+sexo, 
                                               subset(info[[x]], prov_e==84), svytotal) %>% mutate(mes=x))

tramos_etarios_sexo = do.call(rbind, tramos_etarios_sexo) %>%
  `colnames<-` (c("sector", "tramos_edad","sexo", "no trabaja", "trabaja",
                  "se no trabaja", "se trabaja", "mes")) %>% 
  select(sector, sexo, tramos_edad, `trabaja`, `se trabaja`, mes) %>% 
  mutate(cv = round((`se trabaja`/trabaja)*100,2))

frecuencia = lapply(1:4, function(x) xtabs(~sector+tramos_edad+sexo,
             data=subset(info[[x]]$variables, prov_e==84)) %>% data.frame() %>% mutate(mes=x))

frecuencia = do.call(rbind, frecuencia)

tramos_etarios_sexo = merge(tramos_etarios_sexo, frecuencia, by=c("sector", "tramos_edad", "mes", "sexo"))

write.csv(tramos_etarios_sexo, "c2 tramos etarios sexo sector.csv")