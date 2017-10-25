rm(list=ls())
if (!require(readr)) install.packages("readr"); require(readr)
if (!require(stringi)) install.packages("stringi"); require(stringi)
if (!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
if (!require(survey)) install.packages("survey"); require(survey)

afiliados <- read_delim("~/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Base de Datos Seguro de Cesantía/muestraasc5%/1_afiliados.csv",
                        ";", escape_double = FALSE,
                        col_names = FALSE, trim_ws = TRUE)
names(afiliados) = c("id", "sex", "date of birth", "birth county", 
                     "education", "esc", "ecivil", "county", "institution")

rentas_imponibles <- read_delim("~/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Base de Datos Seguro de Cesantía/muestraasc5%/5_rentas_imponibles.csv", 
                                ";", escape_double = FALSE, col_names = FALSE, 
                                trim_ws = TRUE)
names(rentas_imponibles) = c("id_per", "id_emp", "fecha_deven", "tipo", 
                             "subsidio", "econ_activ", "comuna_emp", 
                             "ingreso", "id ingreso", "id tope")


rentas_imponibles = filter(rentas_imponibles, fecha_deven == "201603" | fecha_deven = "201602")

data = merge(afiliados, rentas_imponibles, by.x = "id", by.y = "id_per")

data = filter(data, grepl('^084', county))

data = mutate(data, county = recode_factor(county, '08401' = "Chillán", 
                                                   '08402' = "Bulnes",
                                                   '08403' = "Cobquecura",
                                                   '08404' = "Coelemu",
                                                   '08405' = "Coihueco",
                                                   '08406' = "Chillán Viejo",
                                                   '08407' = "El Carmen",
                                                   '08408' = "Ninhue",
                                                   '08409' = "Ñiquén",
                                                   '08410' = "Pemuco",
                                                   '08411' = "Pinto",
                                                   '08412' = "Portezuelo",
                                                   '08413' = "Quillón",
                                                   '08414' = "Quirihue",
                                                   '08415' = "Ránquil",
                                                   '08416' = "San Carlos",
                                                   '08417' = "San Fabián",
                                                   '08418' = "San Ignacio",
                                                   '08419' = "San Nicolás",
                                                   '08420' = "Treguaco",
                                                   '08421' = "Yungay"))


data = mutate(data, provincia = ifelse(county == "Cobquecura" | county == "Coelemu"| 
                                       county == "Ninhue" | county == "Portezuelo" |
                                       county == "Quirihue" | county == "Quirihue" |
                                       county == "Ránquil" | county == "Treguaco", "Itata",
                                ifelse(county == "Bulnes" | county == "Chillán Viejo" |
                                       county == "Chillán" | county == "El Carmen" |
                                       county == "Pemuco" | county == "Pinto" | 
                                       county == "Quillón" | county == "San Ignacio" |
                                       county == "Yungay","Diguillín", "Punilla")))
data = mutate(data, ingreso = as.numeric(ingreso))

data2 = aggregate(data['ingreso'], by = data['id'], sum)

data = merge(data, data2)

diseno = svydesign(~id, probs = NULL, weights = NULL, data = data)

ingreso_medio = svyby(~ingreso, by = ~provincia+econ_activ,design = diseno,  svymean) %>% 
  mutate(cv = (se/ingreso)*100)

frecuencia = data %>% group_by(provincia, econ_activ) %>%
  count() 

ingreso_medio = mutate(ingreso_medio, freq = frecuencia$n, 
                       confiable = ifelse(cv<=25 | freq<=50, "No confiable", "confiable"))

