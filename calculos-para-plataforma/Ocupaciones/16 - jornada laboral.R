###########################################################################################
# % de trabajadores con jornada completa, con jornada parcial y jornada prolongada
# (clasificar los que están en categoría “otra jornada” de acuerdo al número de horas
#  habitualmente trabajadas).
###########################################################################################

# Para ver cuántas horas declaran trabajar los individuos pertenecientes
# a la categoría otra jornada se hizo lo siguiente:

xtabs(~o10+I(o18==4), data=subset(diseño$variables, provincia==84))

diseño$variables = mutate(diseño$variables, jornada = ifelse(o18==1, "Jornada completa", 
                    ifelse(o18==2, "Jornada parcial", 
                    ifelse(o18==3, "Jornada prolongada",
                    ifelse(o18==4 & o10<30, "Jornada parcial", 
                    ifelse(o18==4 & o10>=30, "Jornada completa", NA))))))

jornadas  = names(table(diseño$variables$jornada))

jornada_ocup_nuble = lapply(jornadas,
                            function(x) svyby(~I(jornada==x), denominator=~I(activ==1),
                                              subset(diseño, provincia==84), by=~oficio4,
                                              svyratio, na.rm = TRUE, na.rm.all = TRUE, drop.empty.groups = FALSE) %>% 
                              `colnames<-` (c("codigo", "porcentaje", "se")) %>% 
                              mutate(cv = se/porcentaje, porcentaje = round(porcentaje*100,1)) %>% 
                              filter(codigo == 1314 |                                
                                       codigo == 2331 | codigo == 4115 |                                                                  
                                       codigo == 5122 | codigo == 5131 |                            
                                       codigo == 5220 | codigo == 6111 |
                                       codigo == 6112 | codigo == 6210 |                                                           
                                       codigo == 7124 | codigo == 8322 |
                                       codigo == 8324 | codigo == 9131 | 
                                       codigo == 9132 | codigo == 9211))

jornada_ocup_nuble_ = do.call(rbind, jornada_ocup_nuble) %>% 
  mutate(sector = rep(jornadas,each = 15))

temp = subset(diseño$variables, provincia==84) %>% group_by(oficio4, jornada) %>%
  summarise(total.count = n()) %>% 
  filter(oficio4 == 1314 |                                
           oficio4 == 2331 | oficio4 == 4115 |                                                                  
           oficio4 == 5122 | oficio4 == 5131 |                            
           oficio4 == 5220 | oficio4 == 6111 |
           oficio4 == 6112 | oficio4 == 6210 |                                                           
           oficio4 == 7124 | oficio4 == 8322 |
           oficio4 == 8324 | oficio4 == 9131 | 
           oficio4 == 9132 | oficio4 == 9211) 

jornada_ocup_nuble = merge(jornada_ocup_nuble_, temp, by.x = c("codigo", "sector"),
                           by.y = c("oficio4", "jornada"))

jornada_ocup_nuble = merge(jornada_ocup_nuble, ciuo, by.x="codigo", by.y="codigo")

write.csv(jornada_ocup_nuble, "jornada_de_trabajo.csv")
