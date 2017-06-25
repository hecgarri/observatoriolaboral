##>  Tipo de ocupación y categoría ocupacional, cuyo fin es caracterizar
##>  la concentración del empleo por categoría (empleador, cuenta propia,
##>  contrato definido, indefinido, sin contrato y personal no remunerado),
##>  resaltando las diferencias entre las ocupaciones (CIUO-88 a un dígito).
##>  Estas cifras deben ser comparadas con datos a nivel de región que engloban
##>  a todos los sectores económicos presentes.

setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/Reporte Sectorial/Reporte-Sectorial-/")

cat_ocup_un_digito = lapply(1:4, function(x) svyby(~I(cae_general=="Ocupado"),
                     by=~sector+b1+categoria_ocupacion, design = subset(info[[x]], prov_e==84), 
                     svytotal, na.rm.all = TRUE ) %>% mutate(mes=x))

cat_ocup_un_digito = do.call(rbind, cat_ocup_un_digito) %>% 
  `colnames<-` (c("sector", "b1", "categoria_ocupacion", "no trabaja", 
                  "trabaja", "se no trabaja", "se trabaja", "mes")) %>% 
  select(sector, b1, categoria_ocupacion, trabaja, `se trabaja`, mes) %>% 
  mutate(cv = round((`se trabaja`/trabaja)*100,2))

frecuencia = lapply(1:4, function(x) xtabs(~sector+b1+categoria_ocupacion, 
                     data=subset(info[[x]]$variables, prov_e==84)) %>% data.frame() %>% 
                      mutate(mes=x))

frecuencia = do.call(rbind, frecuencia)

cat_ocup_un_digito = merge(cat_ocup_un_digito, frecuencia,
                     by=c("sector", "b1", "categoria_ocupacion", "mes"))

write.csv(cat_ocup_un_digito, "c6 categoria ocupacional sector.csv")