rm(list=ls())

path = file.path("/home/hector/GoogleDriveUBB",
                 "OLR Ñuble - Observatorio laboral de Ñuble",
                 "Bases de datos/Base de Datos Seguro de Cesantía") 
setwd(path)

if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE); require(dplyr)
if (!require(RSQLite)) install.packages("sqldf"); require(RSQLite)
if (!require(stringr))     install.packages("stringr"); require(stringr)
if (!require(survey)) install.packages("survey"); require(survey)
#if (!require(devtools)) install.packages("devtools", dependencies = TRUE); require(devtools)

#devtools::install_github("tidyverse/dbplyr")

####################################
## Creacioń de la base de datos 
####################################

db = dbConnect(RSQLite::SQLite(), dbname = "seguro")

dbWriteTable(conn = db, name = "afiliados", value ="1_afiliados.csv",
             overwrite = TRUE,header = FALSE,row.names=FALSE, sep = ";")
dbWriteTable(conn = db, name = "rentas_imponibles",
             value ="5_rentas_imponibles.csv",
             overwrite = TRUE,header = FALSE,row.names=FALSE, sep = ";")

dbListTables(db)
dbListFields(db, "afiliados")
dbListFields(db, "rentas_imponibles")

afil_db = tbl(db, "afiliados")
rentas_db = tbl(db, "rentas_imponibles")

afil_db = afil_db %>% rename( id = V1, sex = V2, date_birth = V3,
                              birth_county = V4, education = V5,
                              esc = V6, county = V7)

rentas_db = rentas_db %>% rename(id = V1, fecha_deven = V2, tipo = V3,
                                 subs = V4, act_econ = V5, 
                                 county_emp = V6, ingreso = V7,
                                 num_trab = V8, renta_pro = V9, std_dev = V10,
                                 id_ing = V11, id_ingt = V12, id_ingm = V13,
                                 id_emp = V14)

com_nuble = c(8401,8402,84,03,8404,8405,8406,8407,8408,8409,
              8410,8411,8412,8413,8414,8415,8416,8417,8418,
              8419,8420,8421)

afil_db = afil_db %>% filter(county %in% com_nuble)

data = left_join(afil_db, rentas_db)

datos = as.data.frame(data)
##################################
## Análisis de la base de datos
##################################

datos = mutate(datos, county = recode_factor(county, '08401' = "Chillán", 
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


datos = mutate(datos, 
               actividad = recode_factor(act_econ,
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


datos = mutate(datos, sector = recode_factor(actividad, 
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


datos = mutate(datos, provincia = ifelse(county == "Cobquecura" | county == "Coelemu"| 
                                           county == "Ninhue" | county == "Portezuelo" |
                                           county == "Quirihue" | county == "Quirihue" |
                                           county == "Ránquil" | county == "Treguaco", "Itata",
                                         ifelse(county == "Bulnes" | county == "Chillán Viejo" |
                                                  county == "Chillán" | county == "El Carmen" |
                                                  county == "Pemuco" | county == "Pinto" | 
                                                  county == "Quillón" | county == "San Ignacio" |
                                                  county == "Yungay","Diguillín", "Punilla")))
datos = mutate(datos, ingreso = as.numeric(ingreso))

datos = mutate(datos, residente = ifelse(grepl('^084', comuna_emp),1,0))

diseno = svydesign(~id, probs = NULL, weights = NULL, data = datos)


obs = datos %>% group_by(id) %>% count() # Número de cotizaciones por individuo
meses = datos %>% group_by(fecha_deven) %>% count() #Observaciones por mes

# Por eso desarrollo lo siguiente con el fin de unir los ingresos duplicados
datos = datos %>% group_by(id, fecha_deven) %>% 
  mutate(ingreso_total = sum(ingreso))

### Ingreso por mes: 
ingreso_medio = svyby(~ingreso, by = ~fecha_deven+sex,design = diseno,  svymean) %>% 
  mutate(cv = (se/ingreso)*100)

frecuencia = datos %>% group_by(fecha_deven,sex) %>% count()

ingresos_ = left_join(ingreso_medio, frecuencia)


ts.plot(ingresos_$ingreso)
