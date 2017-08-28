#Conmutación interregional de ocupados del sector. El objetivo de esta sección
#es mostrar la evolución del porcentaje de empleos generados por el sector
#(en la región) que son ocupados por trabajadores que residen en otras regiones.
#El porcentaje de conmutación se obtiene dividiendo el número de ocupados del
#sector que trabajan en la región, pero residen en otras regiones,
#por el número total de personas que trabajan en el sector a nivel regional.  
#Base de datos: Serie de ocupados por rama y región.

setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/Reporte Sectorial/Reporte-Sectorial-/")

for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables,
                               conmutante = ifelse(prov_e!=prov,1,0))
}

porcentaje_conmutantes = lapply(1:length(info), 
                        function(x) svyby(~I(conmutante==1), denominator=~I(cae_general=="Ocupado"),
                        by=~sector,
                        design = subset(info[[x]], prov_e==84), svyratio,
                        na.rm=TRUE, na.rm.all = TRUE) %>% mutate(mes=x))

porcentaje_conmutantes = do.call(rbind, porcentaje_conmutantes) %>% 
  `colnames<-` (c("sector", "conmutante", "se conmutante", "mes")) %>%
  mutate(cv = round((`se conmutante`/conmutante)*100,2))

frecuencia = lapply(1:length(info), function(x) xtabs(~sector+conmutante, data=subset(info[[x]]$variables, prov==84)) %>%
                      data.frame() %>% mutate(mes=x))

frecuencia = do.call(rbind, frecuencia) 

conmutantes = merge(porcentaje_conmutantes, frecuencia, by=c("sector", "mes"))

write.csv(conmutantes, "c4 conmutantes.csv")
