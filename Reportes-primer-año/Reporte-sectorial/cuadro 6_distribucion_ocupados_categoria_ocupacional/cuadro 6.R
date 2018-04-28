##>  Tipo de ocupación y categoría ocupacional, cuyo fin es caracterizar
##>  la concentración del empleo por categoría (empleador, cuenta propia,
##>  contrato definido, indefinido, sin contrato y personal no remunerado),
##>  resaltando las diferencias entre las ocupaciones (CIUO-88 a un dígito).
##>  Estas cifras deben ser comparadas con datos a nivel de región que engloban
##>  a todos los sectores económicos presentes.

setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/GitHub/observatoriolaboral/Reporte-Sectorial/cuadro 6_distribucion_ocupados_categoria_ocupacional/")

# CREACION DE LA VARIABLE: CATEGORIA OCUPACIONAL 
# (1:Empleador; 2:Cuenta Propia; 3:Trabajador dependiente)

for (i in 1:length(info)){
info[[i]]$variables$cat_ocup = recode(info[[i]]$variables$categoria_ocupacion, `1`= 1, 
                                  `2`= 2, `3` =3, `4`=3, `5`=3, `6`=3,
                                  `7`=4, .default = 0, .missing = 99)
}

for (i in 1:length(info)){
info[[i]]$variables = mutate(info[[i]]$variables, 
                             cat_ocup = ifelse(cat_ocup==3 & b8==2,3,
                                        ifelse(cat_ocup==3 & b8==1 & b9==1,4, 
                                        ifelse(cat_ocup==3 & b8==1 & b9==2,5, 
                                        ifelse(cat_ocup==1,1, 
                                        ifelse(cat_ocup==2,2, 
                                        ifelse(cat_ocup==4,6,NA)))))))
}

for (i in 1:length(info)){
info[[i]]$variables$cat_ocup = factor(info[[i]]$variables$cat_ocup, 
                                  levels=c(1,2,3,4,5,6),
                                  labels=c("empleador","cuenta propia","asalariado sin contrato", 
                                           "asalariado con contrato definido",
                                           "asalariado con contrato indefinido",
                                           "no remunerado"))
}

cat_ocup_un_digito = lapply(1:4, function(x) svyby(~I(cae_general=="Ocupado"),
                     by=~sector+cat_ocup, design = subset(info[[x]], prov_e==84), 
                     svytotal, na.rm.all = TRUE, drop.empty.groups = FALSE ) %>% mutate(mes=x))

cat_ocup_un_digito = do.call(rbind, cat_ocup_un_digito) %>% 
  `colnames<-` (c("sector", "cat_ocup", "no trabaja", 
                  "trabaja", "se no trabaja", "se trabaja", "mes")) %>% 
  select(sector, cat_ocup, trabaja, `se trabaja`, mes) %>% 
  mutate(cv = round((`se trabaja`/trabaja)*100,2))

frecuencia = lapply(1:4, function(x) xtabs(~sector+cat_ocup, 
                     data=subset(info[[x]]$variables, prov_e==84)) %>% data.frame() %>% 
                      mutate(mes=x))

frecuencia = do.call(rbind, frecuencia)

cat_ocup_un_digito = merge(cat_ocup_un_digito, frecuencia,
                     by=c("sector", "cat_ocup", "mes"))

write.csv(cat_ocup_un_digito, "c6 categoria ocupacional sector.csv")