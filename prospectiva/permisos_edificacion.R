rm(list=ls())

if (!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
if (!require(readxl)) install.packages("readxl"); require(readxl)
if (!require(stringr)) install.packages("stringr"); require(stringr)

path = file.path("/home/hector/GoogleDriveUBB/",
"OLR Ñuble - Observatorio laboral de Ñuble/","Bases de datos/pe_2010-2017.xlsx")

permisos =read_excel(path)

total = permisos %>% mutate(aux = stringi::stri_length(CUT),
                            provincia = ifelse(aux==5, 
                                   stringi::stri_sub(CUT,1,3), 
                                   ifelse(aux==4, stringi::stri_sub(CUT,1,2),NA))) %>% 
  filter(provincia == 84) %>% group_by(Año, uso_destino) %>%
  summarise(total = sum(superficie)) 

anual = total %>% group_by(Año) %>% summarise(total = sum(total))

ruta = file.path("/home/hector/GoogleDriveUBB/",
                "OLR Ñuble - Observatorio laboral de Ñuble/",
                "Bases de datos/resumen_permisos.xlsx")
write_excel_csv(total,ruta)

ruta2 = file.path("/home/hector/GoogleDriveUBB/",
                 "OLR Ñuble - Observatorio laboral de Ñuble/",
                 "Bases de datos/resumen_permisos_anual.xlsx")

write_excel_csv(anual,ruta2)
