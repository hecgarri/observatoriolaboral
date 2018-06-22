rm(list = ls())

if (!require(dplyr)) installed.packages("dplyr"); require(dplyr)
if (!require(readr)) installed.packages("readr"); require(readr)
if (!require(lubridate)) installed.packages("lubridate"); require(lubridate)


path = file.path("/home/hector/GoogleDriveUBB/",
"OLR Ñuble - Observatorio laboral de Ñuble/",
"Análisis Cuantitativo/Boletines/datos_hist.csv")

datos = read_csv(path)

promedios = datos %>% mutate(Año = year(date)) %>%
  filter(Año!=2002) %>% 
  group_by(Año) %>% 
  summarise(sal_real = mean(sal_real) %>% round(0),
            sal_real_dig = mean(sal_real_dig) %>% round(0), 
            sal_real_ita = mean(sal_real_ita) %>% round(0),
            sal_real_pun = mean(sal_real_pun) %>% round(0)) %>% 
  mutate(var_real = (((sal_real/lag(sal_real))-1)*100) %>% round(1), 
         var_real_dig = (((sal_real_dig/lag(sal_real_dig))-1)*100) %>% round(1),
         var_real_ita = (((sal_real_ita/lag(sal_real_ita))-1)*100) %>% round(1), 
         var_real_pun = (((sal_real_pun/lag(sal_real_pun))-1)*100) %>% round(1))


path_promedio = file.path("/home/hector/GoogleDriveUBB/",
"OLR Ñuble - Observatorio laboral de Ñuble/",
"Análisis Cuantitativo/Boletines/")

write.csv(promedios, paste0(path_promedio,"promedio_anual.csv"))