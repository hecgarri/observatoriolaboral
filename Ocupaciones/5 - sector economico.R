diseño$variables$sector = recode_factor(diseño$variables$rama1,
                                        `1`="a. Agricultura, ganadería, caza y silvicultura",
                                        `2`="b. Pesca", 
                                        `3`="c. Explotación de minas y canteras",
                                        `4`= "d. Industrias manufactureras",
                                        `5`="e. Suministro de electricidad, gas y agua", 
                                        `6`="f. Construcción", `7`="Comercio", 
                                        `8`="Hoteles y Restaurantes",
                                        `9`="i. Transporte, almacenamiento y comunicaciones", 
                                        `10`="j. Intermediación financiera",
                                        `11`="k. Actividades inmobiliarias, empresariales y de alquiler", 
                                        `12`="Administración pública y defensa", 
                                        `17`="Administración pública y defensa", 
                                        `13`="Servicios Personales", `14`="Servicios Personales", 
                                        `15`="Servicios Personales", `16`="Servicios Personales",
                                        `99` = "ns/nr",
                                        .default=NA_character_)

sectores  = names(table(diseño$variables$sector))

sect_ocup_nuble = lapply(sectores,
                         function(x) svyby(~I(sector==x), denominator=~I(activ==1),
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

sect_ocup_nuble_ = do.call(rbind, sect_ocup_nuble) %>% 
  mutate(sector = rep(sectores,each = 15))

temp = subset(diseño$variables, provincia==84) %>% group_by(oficio4, sector) %>%
  summarise(total.count = n()) %>% 
  filter(oficio4 == 1314 |                                
           oficio4 == 2331 | oficio4 == 4115 |                                                                  
           oficio4 == 5122 | oficio4 == 5131 |                            
           oficio4 == 5220 | oficio4 == 6111 |
           oficio4 == 6112 | oficio4 == 6210 |                                                           
           oficio4 == 7124 | oficio4 == 8322 |
           oficio4 == 8324 | oficio4 == 9131 | 
           oficio4 == 9132 | oficio4 == 9211) 

sect_ocup_nuble = merge(sect_ocup_nuble_, temp, by.x = c("codigo", "sector"),
                        by.y = c("oficio4", "sector")) %>% filter(porcentaje>=5)

sect_ocup_nuble = merge(sect_ocup_nuble, ciuo, by.x="codigo", by.y="codigo")

write.csv(sect_ocup_nuble, "Por_sector_económico.csv")
