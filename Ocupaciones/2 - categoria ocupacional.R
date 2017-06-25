###################################################################################
# Distribución por categoría ocupacional: % de empleadores, % de ocupados por
# cuenta propia, % de ocupados con contrato indefinido, % de ocupados con contrato
# definido, % de ocupados sin contrato y % de trabajadores no remunerados.
###################################################################################

diseño$variables = mutate(diseño$variables, cat_ocup = 
                  ifelse(o15 == 1,"Patrón o empleador", 
                  ifelse(o15==2, "Trabajador por cuenta propia",
                  ifelse((o15==3 | o15==4 | o15==5 | o15==6 | o15==7 | o15==8) & 
                  o17==1 & o16==1,"Dependiente contrato indefinido",
                  ifelse((o15==3 | o15==4 | o15==5 | o15==6 | o15==7 | o15==8) & 
                  o17==1 & o16==2,"Dependiente contrato plazo fijo",
                  ifelse((o15==3 | o15==4 | o15==5 | o15==6 |
                   o15==7 | o15==8) & o17!=1 & o17!=4 & o17!=9,"Dependiente sin Contrato",
                  ifelse(o15==9, "No remunerado",
                   NA)))))))

categorías  = names(table(diseño$variables$cat_ocup))
cat_ocup_nuble = lapply(categorías,
                        function(x) svyby(~I(cat_ocup==x), denominator=~I(activ==1),
                                          subset(diseño, provincia==84), by=~oficio4,
                                          svyratio, na.rm = TRUE, na.rm.all = TRUE, drop.empty.groups = FALSE) %>% 
                          `colnames<-` (c("categoría", "porcentaje", "se")) %>% 
                          mutate(cv = se/porcentaje, porcentaje = round(porcentaje*100,1)) %>% 
                          filter(categoría == 1314 |                                
                                   categoría == 2331 | categoría == 4115 |                                                                  
                                   categoría == 5122 | categoría == 5131 |                            
                                   categoría == 5220 | categoría == 6111 |
                                   categoría == 6112 | categoría == 6210 |                                                           
                                   categoría == 7124 | categoría == 8322 |
                                   categoría == 8324 | categoría == 9131 | 
                                   categoría == 9132 | categoría == 9211))

cat_ocup_nuble_ = do.call(rbind, cat_ocup_nuble) %>% 
  mutate(codigo = rep(categorías,each = 15))

temp = subset(diseño$variables, provincia==84) %>% group_by(oficio4, cat_ocup) %>%
  summarise(total.count = n()) %>% 
  filter(oficio4 == 1314 |                                
           oficio4 == 2331 | oficio4 == 4115 |                                                                  
           oficio4 == 5122 | oficio4 == 5131 |                            
           oficio4 == 5220 | oficio4 == 6111 |
           oficio4 == 6112 | oficio4 == 6210 |                                                           
           oficio4 == 7124 | oficio4 == 8322 |
           oficio4 == 8324 | oficio4 == 9131 | 
           oficio4 == 9132 | oficio4 == 9211) 

cat_ocup_nuble = merge(cat_ocup_nuble_, temp, by.x = c("categoría", "codigo"),
                       by.y = c("oficio4", "cat_ocup"))

cat_ocup_nuble_ = merge(cat_ocup_nuble, ciuo, by.x="categoría", by.y="codigo")

write.csv(cat_ocup_nuble_, "categoría_ocupaciones.csv")
