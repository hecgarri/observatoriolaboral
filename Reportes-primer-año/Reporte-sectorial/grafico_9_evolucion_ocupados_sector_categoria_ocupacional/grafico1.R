# f. Evolución trimestral de ocupados (miles de personas) del sector a nivel
#regional según categoría ocupacional, con el fin de analizar la situación del
#empleo del sector. Para ello, se debe clasificar a los ocupados en las siguientes 
#categorías: empleador, trabajador por cuenta propia, asalariado sin contrato,
#asalariado con contrato definido, asalariado con contrato indefinido y personal
#no remunerado. Nuevamente, se debe utilizar la región de trabajo de las
#personas, para así poder identificar el empleo generado en la región,
#e incorporar nota explicativa. Se utiliza la revisión 3 de la CIIU (variable b14). 
#Base de datos: Serie de ocupados por rama y región.

setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/Reporte Sectorial/Reporte-Sectorial-/")

for (i in 1:82){
info[[i]]$variables$categoria_ocupacion = recode(info[[i]]$variables$categoria_ocupacion, `1`= 1, 
                                  `2`= 2, `3` =3, `4`=3, `5`=3, `6`=3,
                                  `7`=4, .default = 0, .missing = 99)
}

for (i in 1:82){
info[[i]]$variables = mutate(info[[i]]$variables, categoria_ocupacion = ifelse(categoria_ocupacion==3 & b8==2,3,
                                                    ifelse(categoria_ocupacion==3 & b8==1 & b9==1,4, 
                                                    ifelse(categoria_ocupacion==3 & b8==1 & b9==2,5, 
                                                    ifelse(categoria_ocupacion==1,1, 
                                                    ifelse(categoria_ocupacion==2,2, 
                                                    ifelse(categoria_ocupacion==4,6,NA)))))))
}

for (i in 1:82){
info[[i]]$variables$categoria_ocupacion = factor(info[[i]]$variables$categoria_ocupacion, 
                                  levels=c(1,2,3,4,5,6),
                                  labels=c("empleador","cuenta propia","asalariado sin contrato", 
                                           "asalariado con contrato definido",
                                           "asalariado con contrato indefinido",
                                           "no remunerado"))
}


categoria_ocupacional = lapply(1:82, function(x) svyby(~I(cae_general=="Ocupado"), by=~sector+categoria_ocupacion,
                                            design = subset(info[[x]], prov_e==84), svytotal,
                                            na.rm=TRUE, na.rm.all = TRUE) %>% mutate(mes=x))


categoria_ocupacional = do.call(rbind, categoria_ocupacional) %>% `colnames<-` (c("sector", "categoria", 
                        "no trabaja", "trabaja", "se no trabaja", "se trabaja", "mes")) %>% 
  select(sector, categoria, trabaja, `se trabaja`, mes)



categoria_ocupacional2 = lapply(1:82, function(x) svyby(~I(cae_general=="Ocupado"), by=~categoria_ocupacion,
                                                       design = subset(info[[x]], prov_e==84), svytotal,
                                                       na.rm=TRUE, na.rm.all = TRUE) %>% mutate(mes=x))


categoria_ocupacional2 = do.call(rbind, categoria_ocupacional2) %>% `colnames<-` (c("categoria", 
                                                                                  "no trabaja", "trabaja", "se no trabaja", "se trabaja", "mes")) %>% 
  select(categoria, trabaja, `se trabaja`, mes)
