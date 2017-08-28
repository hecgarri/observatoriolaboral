# h. Distribución según nivel educacional del año más reciente,
# cuyo fin es caracterizar el nivel educacional de los trabajadores
# del sector en la región, señalando el nivel educacional con más concentración
# de trabajadores y el con menos concentración de trabajadores. 
# Estas cifras se deben comparar con datos a nivel de región que engloba
# a todos los sectores económicos presentes. Base de datos: ENEtrim,
# usando las categorías de la variable nivel educacional especificados en la sección 4 de este manual.


setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/Reporte Sectorial/Reporte-Sectorial-/")


nivel_educ = lapply(1:4,function(x) svyby(~I(cae_general=="Ocupado"), by=~sector+educ3, 
                               design = subset(info[[x]], prov_e==84), svytotal, na.rm.all = TRUE) %>% 
                      mutate(mes = x))

nivel_educ = do.call(rbind, nivel_educ) %>% `colnames<-`(c("sector", "educ", "no trabaja", "trabaja", 
                                                           "se no trabaja", "se trabaja", "mes")) %>% 
  select(sector, educ, `trabaja`, `se trabaja`, mes)

frecuencia = lapply(1:4, function(x) xtabs(~sector+educ3,
                   data = subset(info[[x]]$variables, prov_e==84)) %>% data.frame() 
                   %>% mutate(mes=x))

frecuencia = do.call(rbind, frecuencia) %>% rename(educ = educ3)

nivel_educ = merge(nivel_educ, frecuencia, by = c("sector", "educ", "mes")) %>% mutate(cv = `se trabaja`/trabaja)


write.csv(nivel_educ, "c3 nivel educacional.csv")


#############################################################################
#
#############################################################################

nivel_educ2 = lapply(1:4,function(x) svyby(~I(cae_general=="Ocupado"), by=~educ3, 
                                          design = subset(info[[x]], prov_e==84), svytotal, na.rm.all = TRUE) %>% 
                      mutate(mes = x))

nivel_educ2 = do.call(rbind, nivel_educ2) %>% `colnames<-`(c("educ", "no trabaja", "trabaja", 
                                                           "se no trabaja", "se trabaja", "mes")) %>% 
  select(educ, `trabaja`, `se trabaja`, mes)

frecuencia = lapply(1:4, function(x) xtabs(~educ3,
                                           data = subset(info[[x]]$variables, prov_e==84)) %>% data.frame() 
                    %>% mutate(mes=x))

frecuencia = do.call(rbind, frecuencia) %>% rename(educ = educ3)

nivel_educ2 = merge(nivel_educ2, frecuencia, by = c("educ", "mes")) %>% mutate(cv = `se trabaja`/trabaja)


write.csv(nivel_educ2, "c3 nivel educacional sin sector.csv")
