ocupaciones4 = svyby(~I(activ==1), by=~oficio4, design=subset(diseño, provincia==84),
                     svytotal, multicore=TRUE,
                     drop.empty.groups=FALSE, na.rm=TRUE) %>% 
  `colnames<-` (c("codigo", "no_ocupado", "ocupado", "se_no_ocupado", "se_ocupado")) %>% 
  select(codigo, ocupado, se_ocupado) %>% 
  mutate(cv = se_ocupado/ocupado) 

temp = xtabs(~oficio4, data=subset(diseño$variables, provincia==84)) %>% data.frame() 

ocupaciones4$frecuencia = temp$Freq

ciuo$codigo = as.factor(ciuo$codigo)

ocupaciones4 = merge(ocupaciones4, ciuo, by = "codigo")

ocupaciones4 = filter(ocupaciones4, frecuencia>=50 & cv<=0.25)


contrato_ocup = lapply(as.character(ocupaciones4$codigo),
                 function(x) svyby(~I(o17==1), denominator=~I(activ==1),
                  subset(diseño, provincia==84), by=~I(oficio4==x),
                  svyratio, na.rm=TRUE,drop.empty.groups = FALSE))
