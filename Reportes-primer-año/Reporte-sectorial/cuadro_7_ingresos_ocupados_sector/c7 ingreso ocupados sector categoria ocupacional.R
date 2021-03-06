setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/GitHub/observatoriolaboral/Reporte-Sectorial/cuadro_7_ingresos_ocupados_sector/")
diseño$variables = mutate(diseño$variables, cat_ocup = 
                  ifelse(o15 == 1,"Patrón o empleador", 
                  ifelse(o15==2, "Trabajador por cuenta propia",
                  ifelse((o15==3 | o15==4 | o15==5 | o15==6 | o15==7 | o15==8) & 
                  o17==1,"Dependiente contrato",
                  ifelse((o15==3 | o15==4 | o15==5 | o15==6 | o15==7 | o15==8) & 
                  o17==1,"Dependiente contrato",
                  ifelse((o15==3 | o15==4 | o15==5 | o15==6 |
                  o15==7 | o15==8) & o17!=1 & o17!=4 & o17!=9,"Dependiente sin Contrato",
                  ifelse(o15==9, "No remunerado",
                  NA)))))))

ingreso_cat_sect = svyby(~yoprCor, by=~rama1+cat_ocup,
                    design = subset(diseño, provincia==84), 
                    svymean, na.rm=TRUE, na.rm.all=TRUE, drop.empty.groups = FALSE) %>% 
  mutate(cv = se/yoprCor) 


frecuencia = subset(diseño$variables, provincia==84) %>% group_by(rama1, cat_ocup) %>% count()

ingreso_cat = svyby(~yoprCor, by=~cat_ocup,
                    design = subset(diseño, provincia==84), 
                    svymean, na.rm=TRUE, na.rm.all=TRUE, drop.empty.groups = FALSE)

svyby(~yoprCor, by=~rama1, design=subset(diseño, provincia==84), svymean, 
        na.rm=TRUE, na.rm.all=TRUE)

svymean(~yoprCor, design = subset(diseño, provincia==84), na.rm=TRUE, na.rm.all=TRUE)

write.csv(cbind(ingreso_cat, ingreso_cat_sect), "ingreso_categoria.csv")