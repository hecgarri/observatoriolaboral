##########################################################################################
# Número de trabajadores por ocupación 
##########################################################################################
setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/GitHub/observatoriolaboral/Reporte-Sectorial/cuadro_9_principales_ocupaciones_del_sector/")

ciuo = read.csv("codigos_ciuo.csv")

ciuo = ciuo %>% group_by(etiqueta, codigo) %>% filter(row_number(codigo)==1)

ocupaciones_sector = svyby(~I(activ==1), by=~oficio4+rama1, design=subset(diseño, provincia==84),
                     svytotal, multicore=TRUE,
                     drop.empty.groups=FALSE, na.rm=TRUE) %>% 
  `colnames<-` (c("codigo", "sector", "no_ocupado", "ocupado", "se_no_ocupado", "se_ocupado")) %>% 
  select(codigo, sector, ocupado, se_ocupado) %>% 
  mutate(cv = se_ocupado/ocupado) 

temp = xtabs(~oficio4+rama1, data=subset(diseño$variables, provincia==84)) %>% data.frame() 

ocupaciones_sector$frecuencia = temp$Freq

ciuo$codigo = as.factor(ciuo$codigo)

ocupaciones_sector = merge(ocupaciones_sector, ciuo, by = "codigo")

ocupaciones_sector = filter(ocupaciones_sector, frecuencia>=5 & sector==8)

# % sobre el total del sector 

ocupas = as.character(ocupaciones_sector$codigo)
porc_hoteles = lapply(ocupas, function(x) svyby(~I(oficio4==x), denominator=~I(activ==1),
                                                by = ~rama1, svyratio,
                                                design = subset(diseño, provincia==84),
                                                na.rm = TRUE)) %>% rbindlist() %>% 
  filter(rama1==8)

porc_total = lapply(ocupas, function(x) svyby(~I(oficio4==x), denominator=~I(activ==1),
                                                by = ~I(provincia==84), svyratio,
                                                design = subset(diseño, provincia==84),
                                                na.rm = TRUE)) %>% rbindlist() 

ocupaciones_sector_ = cbind(porc_hoteles, porc_total)
write.csv(ocupaciones_sector_, "ocupados_ocupacion.csv")
