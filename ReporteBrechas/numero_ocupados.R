source("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/GitHub/observatoriolaboral/reporte-inclusion-laboral/0 - Carga de datos CASEN 2015.R")


empleos = c(1210,9211,3213,6112,8275,3212,3421,8278,7415,3415,8331,8322,
            7414,3152,8152,5121,4222,1210,7412,9132,5122,9152)
ocupaciones = svyby(~I(activ==1), by = ~oficio4,
      design = subset(diseno, provincia==84),
      svytotal, multicore = TRUE, na.rm = TRUE, drop.empty.groups = FALSE) %>%
  filter(oficio4 %in% empleos) %>% 
  rename(oficio4 = oficio4, 'no_ocupados'= `I(activ == 1)FALSE`, 
         'ocupados'= `I(activ == 1)TRUE`, 
         'se_no_ocupados' = `se.I(activ == 1)FALSE`, 
         'se_ocupados' = `se.I(activ == 1)TRUE`) %>% 
  select(oficio4, ocupados, se_ocupados) %>% 
  mutate(cv = (se_ocupados/ocupados)*100)

frecuencia = subset(casen2015, provincia==84) %>% group_by(oficio4) %>%
  count() %>% 
  filter(oficio4 %in% empleos)
  
n_ocupados = merge(ocupaciones, frecuencia, by = "oficio4") %>% 
  mutate(quality = ifelse(cv<=30 & n>=50,1,0)) %>% 
  filter(quality ==1) %>% mutate(region= "Ñuble")

problematicas = merge(ocupaciones, frecuencia, by = "oficio4") %>% 
  mutate(quality = ifelse(cv<=30 & n>=50,1,0)) %>% 
  filter(quality ==0)

ocupaciones_prob = svyby(~I(activ==1), by = ~oficio4,
                    design = subset(diseno, provincia %in% c(81,82,83,84)),
                    svytotal, multicore = TRUE, na.rm = TRUE, 
                    drop.empty.groups = FALSE) %>%
  filter((oficio4 %in% problematicas$oficio4) |
           oficio4==8152 | oficio4==8275 | oficio4==8278) %>% 
  rename(oficio4 = oficio4, 'no_ocupados'= `I(activ == 1)FALSE`, 
         'ocupados'= `I(activ == 1)TRUE`, 
         'se_no_ocupados' = `se.I(activ == 1)FALSE`, 
         'se_ocupados' = `se.I(activ == 1)TRUE`) %>% 
  select(oficio4, ocupados, se_ocupados) %>% 
  mutate(cv = (se_ocupados/ocupados)*100)

frecuencia_prob = subset(casen2015, provincia %in% c(81,82,83,84)) %>%
  group_by(oficio4) %>%
  count() %>% filter(oficio4 %in% problematicas$oficio4)

n_ocupados_prob = merge(ocupaciones_prob, frecuencia_prob, by = "oficio4") %>% 
  mutate(quality = ifelse(cv<=30 & n>=50,1,0)) %>% 
  filter(quality ==1) %>% mutate(region = "Ñuble+Nueva Biobío")


problematicas2 = merge(ocupaciones_prob, frecuencia_prob, by = "oficio4") %>% 
  mutate(quality = ifelse(cv<=30 & n>=50,1,0)) %>% 
  filter(quality ==0)

ocupaciones_prob2 = svyby(~I(activ==1), by = ~oficio4,
                         design = subset(diseno, region %in% c(7,8)),
                         svytotal, multicore = TRUE, na.rm = TRUE, 
                         drop.empty.groups = FALSE) %>%
  filter((oficio4 %in% problematicas2$oficio4) |
           oficio4==8152 | oficio4==8275 | oficio4==8278) %>% 
  rename(oficio4 = oficio4, 'no_ocupados'= `I(activ == 1)FALSE`, 
         'ocupados'= `I(activ == 1)TRUE`, 
         'se_no_ocupados' = `se.I(activ == 1)FALSE`, 
         'se_ocupados' = `se.I(activ == 1)TRUE`) %>% 
  select(oficio4, ocupados, se_ocupados) %>% 
  mutate(cv = (se_ocupados/ocupados)*100)

frecuencia_prob2 = subset(casen2015, region %in% c(7,8)) %>%
  group_by(oficio4) %>%
  count() %>% filter(oficio4 %in% problematicas2$oficio4)

n_ocupados_prob2 = merge(ocupaciones_prob2, frecuencia_prob2,
                         by = "oficio4") %>% 
  mutate(quality = ifelse(cv<=30 & n>=50,1,0)) %>% 
  filter(quality ==1) %>% mutate(region = "Maule+Ñuble+Nueva Biobío")

problematicas3 = merge(ocupaciones_prob2, frecuencia_prob2,
                         by = "oficio4") %>% 
  mutate(quality = ifelse(cv<=30 & n>=50,1,0)) %>% 
  filter(quality ==0)


ocupaciones_prob3 = svyby(~I(activ==1), by = ~oficio4,
                          design = diseno,
                          svytotal, multicore = TRUE, na.rm = TRUE, 
                          drop.empty.groups = FALSE) %>%
  filter((oficio4 %in% problematicas3$oficio4 ) |
           oficio4==8152 | oficio4==8275 | oficio4==8278) %>% 
  rename(oficio4 = oficio4, 'no_ocupados'= `I(activ == 1)FALSE`, 
         'ocupados'= `I(activ == 1)TRUE`, 
         'se_no_ocupados' = `se.I(activ == 1)FALSE`, 
         'se_ocupados' = `se.I(activ == 1)TRUE`) %>% 
  select(oficio4, ocupados, se_ocupados) %>% 
  mutate(cv = (se_ocupados/ocupados)*100)

frecuencia_prob3 = casen2015 %>%
  group_by(oficio4) %>%
  count() %>% filter(oficio4 %in% problematicas3$oficio4)

n_ocupados_prob3 = merge(ocupaciones_prob3, frecuencia_prob3,
                         by = "oficio4") %>% 
  mutate(quality = ifelse(cv<=30 & n>=50,1,0))  %>%
  mutate(region = "Nacional")

# Muy problemáticas 

ocupaciones_prob4 = svyby(~I(activ==1), by = ~oficio4,
                          design = diseno,
                          svytotal, multicore = TRUE, na.rm = TRUE, 
                          drop.empty.groups = FALSE) %>%
  filter(oficio4==8152 | oficio4==8275 | oficio4==8278) %>% 
  rename(oficio4 = oficio4, 'no_ocupados'= `I(activ == 1)FALSE`, 
         'ocupados'= `I(activ == 1)TRUE`, 
         'se_no_ocupados' = `se.I(activ == 1)FALSE`, 
         'se_ocupados' = `se.I(activ == 1)TRUE`) %>% 
  select(oficio4, ocupados, se_ocupados) %>% 
  mutate(cv = (se_ocupados/ocupados)*100)

frecuencia_prob4 = casen2015 %>%
  group_by(oficio4) %>%
  count() %>% filter(oficio4 %in% c(8152,8275,8278))

n_ocupados_prob4 = merge(ocupaciones_prob4, frecuencia_prob4,
                         by = "oficio4") %>% 
  mutate(quality = ifelse(cv<=30 & n>=50,1,0))  %>%
  mutate(region = "Nacional")




ocupaciones = rbind(n_ocupados, n_ocupados_prob, n_ocupados_prob2, n_ocupados_prob3, 
                    n_ocupados_prob4)

write.csv(ocupaciones, "/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/GitHub/observatoriolaboral/ReporteBrechas/ocupaciones_escasas.csv")
