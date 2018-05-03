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

afiliados = filter(afiliados, grepl('^08401', county))

rentas_imponibles <- read_delim("~/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Base de Datos Seguro de Cesantía/muestraasc5%/5_rentas_imponibles.csv", 
                                ";", escape_double = FALSE, col_names = FALSE, 
                                trim_ws = TRUE)

names(rentas_imponibles) = c("id", "id_emp", "fecha_deven", "tipo", 
                             "subsidio", "econ_activ", "comuna_emp", 
                             "ingreso", "id ingreso", "id tope")


#rentas_imponibles = filter(rentas_imponibles, fecha_deven == "201603" |
#                             fecha_deven == "201602" | fecha_deven == "201601")

data = left_join(afiliados, rentas_imponibles) 
  

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


data = mutate(data, 
      actividad = recode_factor(econ_activ,
                              '00' = "Actividad no especificada",
                              '01' = "Agricultura, Ganadería, Caza y Silvicultura",
                              '02' = 'Pesca',
                              '03' = 'Explotación de Minas y Canteras',
                              '04' = 'Industrias Manufactureras No Metálicas',
                              '05' = 'Industrias Manufactureras Metálicas',
                              '06' = 'Suministro de Electricidad, Gas y Agua',
                              '07' = 'Construcción',
                              '08' = 'Comercio al Por Mayor y Menor; Rep. Vehículos Automotores/Enseres Domésticos',
                              '09' = 'Hoteles y Restaurantes',
                              '10' = 'Transporte, Almacenamiento y Comunicaciones',
                              '11' = 'Intermediación Financiera',
                              '12' = 'Actividades Inmobiliarias, Empresariales y de Alquiler',
                              '13' = 'Adm. Pública y Defensa; Planes de Seg. Social, Afiliación Obligatoria',
                              '14' = 'Enseñanza',
                              '15' = 'Servicios Sociales y de Salud',
                              '16' = 'Otras Actividades de Servicios Comunitarias, Sociales y Personales',
                              '17' = 'Consejo de Administración de Edificios y Condominios',
                              '18' = 'Organizaciones y Órganos Extraterritoriales'))


data = mutate(data, sector = recode_factor(actividad, 
                                 'Pesca'="Pesca", 
                                 'Industrias Manufactureras No Metálicas'="Industria Manufacturera",
                                 'Industrias Manufactureras Metálicas'="Industria Manufacturera",
                                 'Suministro de Electricidad, Gas y Agua'="Electricidad, Gas y Agua",
                                 'Comercio al Por Mayor y Menor; Rep. Vehículos Automotores/Enseres Domésticos'="Comercio",
                                 'Transporte, Almacenamiento y Comunicaciones'="Transporte y Comunicaciones",
                                 'Enseñanza'="Servicios Sociales y Personales",
                                 'Servicios Sociales y de Salud'="Servicios Sociales y Personales",
                                 'Otras Actividades de Servicios Comunitarias, Sociales y Personales'="Servicios Sociales y Personales",
                                 'Consejo de Administración de Edificios y Condominios'="Administracion Publica",
                                 'Organizaciones y Órganos Extraterritoriales'="Administracion Publica", 
                                 'Adm. Pública y Defensa; Planes de Seg. Social, Afiliación Obligatoria' = "Administracion Publica"))


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

data = mutate(data, residente = ifelse(grepl('^084', comuna_emp),1,0))

diseno = svydesign(~id, probs = NULL, weights = NULL, data = data)


obs = data %>% group_by(id) %>% count() # Número de cotizaciones por individuo
meses = data %>% group_by(fecha_deven) %>% count() #Observaciones por mes



### Ingreso don pepito 

pepito = data %>% filter(id == 945850611) # Don pepito tiene más de un trabajo

# Por eso desarrollo lo siguiente con el fin de unir los ingresos duplicados
data = data %>% group_by(id, fecha_deven) %>% 
                   mutate(ingreso_total = sum(ingreso)) 

### Ingreso por mes: 
ingreso_medio = svyby(~ingreso, by = ~provincia+sector,design = diseno,  svymean) %>% 
  mutate(cv = (se/ingreso)*100)



ingreso_medio = svyby(~ingreso, by = ~provincia+sector,design = diseno,  svymean) %>% 
  mutate(cv = (se/ingreso)*100)

frecuencia = data %>% group_by(provincia, sector) %>%
  count() 
ingreso_medio = merge(ingreso_medio, frecuencia, by.x = c("provincia", "sector"), by.y = c("provincia", "sector"))

ingreso_medio = mutate(ingreso_medio, 
                       confiable = ifelse(cv>=30 | n<=50,
                      "No confiable", "confiable"))

write.csv(ingreso_medio, "./seguro_cesantia/ingreso_promedio.csv")

ingreso_ = svyby(~ingreso, by = ~sector,design = diseno,  svymean) %>% 
  mutate(cv = (se/ingreso)*100)

frecuencia = data %>% group_by(sector) %>%
  count() 

ingreso_ = merge(ingreso_, frecuencia, by.x = "sector", by.y = "sector")
ingreso_ = mutate(ingreso_,
          confiable = ifelse(cv>=30 | n<=50,
          "No confiable", "confiable"))


write.csv(ingreso_, "./seguro_cesantia/ingreso_sector.csv")