# Si la conmutación es un fenómeno relevante para el sector,
#se debe caracterizar a los trabajadores del sector que provienen de otras regiones y
#presentar siguientes estadísticos: edad promedio, años de escolaridad promedio,
#% de mujeres, % con educación superior completa y promedio de ingresos de la ocupación
#principal del año más reciente. Adicionalmente, se pueden presentar otras estadísticas,
#en la medida en que agreguen información valiosa sobre este grupo.


setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/Reporte Sectorial/Reporte-Sectorial-/")

for (i in 1:4){
  info[[i]]$variables = mutate(info[[i]]$variables,
                               conmutante = ifelse(prov_e!=prov,1,0))
}
  
conmutantes = lapply(1:4, function(x) svyby(~I(conmutante==1), by=~sector,
              design = subset(info[[x]], prov_e==84), svytotal,
              na.rm=TRUE, na.rm.all = TRUE) %>% mutate(mes=x))

conmutantes = do.call(rbind, conmutantes) %>% 
  `colnames<-` (c("sector", "no conmuta", "conmutante",
                  "se no conmuta", "se conmutante", "mes")) %>% 
  select(sector, conmutante, `se conmutante`, mes) %>% 
  mutate(cv = round((`se conmutante`/conmutante)*100,2))

frecuencia = lapply(1:4, function(x) xtabs(~sector+conmutante, data=subset(info[[x]]$variables, prov==84)) %>%
                      data.frame() %>% mutate(mes=x))

frecuencia = do.call(rbind, frecuencia) %>% filter(conmutante ==1)

conmutantes = merge(conmutantes, frecuencia, by=c("sector", "mes"))

write.csv(conmutantes, "c4 conmutantes.csv")