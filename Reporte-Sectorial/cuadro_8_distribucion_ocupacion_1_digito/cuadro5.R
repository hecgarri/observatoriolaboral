# r. Distribución por ocupación a 1 dígito, cuyo fin es caracterizar
# la concentración de empleo según tipo de ocupación (CIUO-88 a un dígito,
# como trabajadores no calificados, miembros del poder ejecutivo,
# entre otros). Se debe resaltar qué tipo de ocupación concentra más
# el empleo del sector en la región y qué tipo de ocupación
# concentra menos el empleo del sector en la región. 
# Estas cifras se pueden comparar con datos a nivel de
# región que engloban a todos los sectores económicos presentes.

setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/Reporte Sectorial/Reporte-Sectorial-/")

ocupaciones_digito = lapply(1:4, function(x) svyby(~I(cae_general=="Ocupado"),by=~sector+b1, 
                      design = subset(info[[x]], prov_e==84), svytotal, na.rm.all = TRUE) %>% 
                        mutate(mes=x))

ocupaciones_digito = do.call(rbind, ocupaciones_digito) %>% `colnames<-` (c("sector","b1", "no trabaja", 
                      "trabaja", "se no trabaja", "se trabaja", "mes")) %>% 
  select(sector,b1, trabaja, `se trabaja`, mes)

frecuencia = lapply(1:4, function(x) xtabs(~sector+b1, data=subset(info[[x]]$variables, prov_e==84)) %>% 
                      data.frame() %>% mutate(mes=x))
frecuencia = do.call(rbind, frecuencia)

ocupaciones_digito = merge(ocupaciones_digito, frecuencia, by=c("sector", "b1", "mes")) %>% 
  mutate(cv = round((`se trabaja`/trabaja)*100,2))


ocupaciones_digito2 = lapply(1:4, function(x) svyby(~I(cae_general=="Ocupado"),by=~b1, 
                     design = subset(info[[x]], prov_e==84), svytotal, na.rm.all = TRUE) %>% 
                              mutate(mes=x))
ocupaciones_digito2 = do.call(rbind, ocupaciones_digito2)


write.csv(ocupaciones_digito, "c5 ocupaciones por sector.csv")

write.csv(ocupaciones_digito2, "c5 ocupaciones a un digito.csv")