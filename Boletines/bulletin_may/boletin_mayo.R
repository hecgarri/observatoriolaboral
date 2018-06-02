rm(list=ls())

path = file.path("/home/hector/GoogleDriveUBB",
                 "OLR Ñuble - Observatorio laboral de Ñuble",
                 "Bases de datos/Base de Datos Seguro de Cesantía") 
setwd(path)

if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE); require(dplyr)
if (!require(RSQLite)) install.packages("sqldf"); require(RSQLite)
if (!require(stringr))     install.packages("stringr"); require(stringr)
if (!require(survey)) install.packages("survey"); require(survey)
if (!require(lubridate)) install.packages("lubridate"); require(lubridate)
#if (!require(devtools)) install.packages("devtools", dependencies = TRUE); require(devtools)

#devtools::install_github("tidyverse/dbplyr")

####################################
## Creacioń de la base de datos 
####################################

db = dbConnect(RSQLite::SQLite(), dbname = "seguro")

dbWriteTable(conn = db, name = 'afiliados', value ="1_afiliados_3.csv",
            append = TRUE, header = FALSE, rownames = FALSE, sep = ";")
dbWriteTable(conn = db, name = "afiliados", value ="1_afiliados_5.csv",
             append = TRUE,header = FALSE,row.names=FALSE, sep = ";")
dbWriteTable(conn = db, name = "afiliados", value ="1_afiliados_12.csv",
             append = TRUE,header = FALSE,row.names=FALSE, sep = ";")
dbWriteTable(conn = db, name = "rentas_imponibles_3",
             value ="5_rentas_imponibles_3.csv",
             overwrite = TRUE,header = FALSE,row.names=FALSE, sep = ";")
dbWriteTable(conn = db, name = "rentas_imponibles_5",
             value ="5_rentas_imponibles_5.csv",
             overwrite = TRUE,header = FALSE,row.names=FALSE, sep = ";")
dbWriteTable(conn = db, name = "rentas_imponibles_12",
             value ="5_rentas_imponibles_12.csv",
             overwrite = TRUE,header = FALSE,row.names=FALSE, sep = ";")



dbListTables(db)
dbListFields(db, "afiliados_3")
dbListFields(db, "rentas_imponibles_12")


afil3_db = tbl(db, "afiliados_3") 
afil5_db = tbl(db, "afiliados_5") 
afil12_db = tbl(db, "afiliados_12") 
rentas3_db = tbl(db, "rentas_imponibles_3") 
rentas5_db = tbl(db, "rentas_imponibles_5") 
rentas12_db = tbl(db, "rentas_imponibles_12") 

afil3_db = afil3_db %>% rename( id = V1, sex = V2, date_birth = V3,
                              birth_county = V4, education = V5,
                              esc = V6, county = V7)
afil5_db = afil5_db %>% rename( id = V1, sex = V2, date_birth = V3,
                              birth_county = V4, education = V5,
                              esc = V6, county = V7)
afil12_db = afil12_db %>% rename( id = V1, sex = V2, date_birth = V3,
                              birth_county = V4, education = V5,
                              esc = V6, county = V7)


rentas3_db = rentas3_db %>% rename(id = V1, fecha_deven = V2, tipo = V3,
                                 subs = V4, act_econ = V5, 
                                 county_emp = V6, ingreso = V7,
                                 num_trab = V8, renta_pro = V9, std_dev = V10,
                                 id_ing = V11, id_ingt = V12, id_ingm = V13,
                                 id_emp = V14)
rentas5_db = rentas5_db %>% rename(id = V1, fecha_deven = V2, tipo = V3,
                                 subs = V4, act_econ = V5, 
                                 county_emp = V6, ingreso = V7,
                                 num_trab = V8, renta_pro = V9, std_dev = V10,
                                 id_ing = V11, id_ingt = V12, id_ingm = V13,
                                 id_emp = V14)
rentas12_db = rentas12_db %>% rename(id = V1, fecha_deven = V2, tipo = V3,
                                 subs = V4, act_econ = V5, 
                                 county_emp = V6, ingreso = V7,
                                 num_trab = V8, renta_pro = V9, std_dev = V10,
                                 id_ing = V11, id_ingt = V12, id_ingm = V13,
                                 id_emp = V14)

com_nuble = c(8401,8402,84,03,8404,8405,8406,8407,8408,8409,
              8410,8411,8412,8413,8414,8415,8416,8417,8418,
              8419,8420,8421)

afil3_db = afil3_db %>% filter(county %in% com_nuble)
afil5_db = afil5_db %>% filter(county %in% com_nuble)
afil12_db = afil12_db %>% filter(county %in% com_nuble)


data1 = left_join(afil3_db, rentas3_db)
data2 = left_join(afil5_db, rentas5_db)
data3 = left_join(afil12_db, rentas12_db)

datos1 = as.data.frame(data1)
datos2 = as.data.frame(data2)
datos3 = as.data.frame(data3)


datos = bind_rows(datos1, datos2, datos3)

rm(datos1, datos2, datos3)
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

#datos = mutate(datos, residente = ifelse(grepl('^084', comuna_emp),1,0))

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

ingreso_todos = svyby(~ingreso, by = ~fecha_deven, design = diseno, svymean) 
ingreso_todos = ingreso_todos %>% mutate(cv = se/ingreso)

frecuencia_ing = datos %>% group_by(fecha_deven) %>% count()

ing_todos = left_join(ingreso_todos, frecuencia_ing) %>% mutate(quality = ifelse(cv<=0.25 & n>=50,"Sí","No"))


ingreso_prov = svyby(~ingreso, by = ~fecha_deven+provincia, design = diseno, svymean) 
ingreso_prov = ingreso_prov %>% mutate(cv = se/ingreso)

frecuencia_ing_prov = datos %>% group_by(fecha_deven,provincia) %>% count()

ing_todos_prov = left_join(ingreso_prov, frecuencia_ing_prov) %>% mutate(quality = ifelse(cv<=0.25 & n>=50,"Sí","No"))


ingreso_act = svyby(~ingreso, by = ~fecha_deven+sector,
                    design = diseno, svymean, multicore = 4) 
ingreso_act = ingreso_act %>% mutate(cv = se/ingreso)

frecuencia_ing_act = datos %>% group_by(fecha_deven,sector) %>% count()

ing_todos_act = left_join(ingreso_act, frecuencia_ing_act) %>%
  mutate(quality = ifelse(cv<=0.25 & n>=50,"Sí","No")) %>% 
  mutate(ingreso = ifelse(quality=="Sí",ingreso,NA))


###############################################################
# Empalme de la serie de ipc 
###############################################################

# La serie de ipc se presenta en dos años base diferentes. 
#Para homogeneizar su tratamiento la llevaremos al mismo año base. 
# Mientras la base ipc2013 tiene como año base el promedio del 2013,
# la base ipc2008 tiene como base el promedio del año 2008. 
# El procedimiento utilizado es el descrito en Correa, V., Escandón, A.,
#Luengo, R., & Venegas, J. (2003). Empalme de series anuales y trimestrales
#del PIB. Notas de Investigación Journal Economía Chilena (The Chilean Economy), 6(1), 77-86.

if (!require(readxl)) install.packages("readxl"); require(readxl)

ipc_2013 = file.path("/home/hector/GoogleDriveUBB","OLR Ñuble - Observatorio laboral de Ñuble",
                     "Bases de datos/Banco Central","IPC serie histórica empalmada base 2013.xls")

ipc_2008 = file.path("/home/hector/GoogleDriveUBB","OLR Ñuble - Observatorio laboral de Ñuble",
                     "Bases de datos/Banco Central","IPC serie histórica empalmada base 2008.xls")

ipc_base_2013 = read_xls(ipc_2013,sheet = 1, range = "A3:B104") %>% 
                      rename(year = 'Periodo', ipc = '1. IPC serie histórica empalmada base 2013=100')
ipc_base_2008 = read_xls(ipc_2008,sheet = 1, range = "A3:B99") %>% 
  rename(year = 'Periodo', ipc = '1. IPC General')

ipc = data.frame(date = seq.Date(as.Date("2002/01/01"),
                                 as.Date("2018/04/01"), by = "month"),
                 ipc_2008 = c(ipc_base_2008$ipc, rep(NA,100)),
                 ipc_2013 = c(rep(NA,95), ipc_base_2013$ipc))
ipc = ipc %>% mutate(ipc_2008 = ts(ipc_2008,start =c(2002,1), end = c(2018,4), freq = 12),
                     ipc_2013 = ts(ipc_2013,start =c(2002,1), end = c(2018,4), freq = 12))


# Se observa un cambio importante en ambas series debido a la diferencia entre años base

ts.plot(ipc$ipc_2008,ipc$ipc_2013, col=2:3)

D = ipc$ipc_2013[96]/ipc$ipc_2008[96]
t_ = sum(!is.na(ipc$ipc_2008))
r = D^(1/t_)
index = rownames(ipc) %>% as.numeric()
ipc = ipc %>% mutate(ipc_b_2013 = ipc_2008*r^(index-1)) %>% 
              mutate(ipc_b_2013 = ifelse(is.na(ipc_b_2013), ipc_2013, ipc_b_2013))
ipc$ipc_b_2013[96] = ipc$ipc_2013[96]

###############################################################
# Creación de una base de datos de ingresos reales históricos
###############################################################

# sal_nom = Salario nominal para todo Ñuble
# sal_real = Salario real para todo Ñuble en pesos de marzo de 2017
# sal_nom_dig = Salario nominal provincia de Díguillin 
datos_hist = data.frame(date = seq.Date(from = as.Date("2002/10/01"),to = as.Date("2017/03/01"), by = "month"))
datos_hist = datos_hist %>% mutate(sal_nom = ing_todos$ingreso, 
   ipc = ipc$ipc_b_2013[1:174], 
   sal_real = ts(sal_nom*(ipc[174]/ipc), start=c(2002,10), end = c(2017,3), freq=12),
   sal_nom_dig = subset(ing_todos_prov, provincia =="Diguillín")$ingreso, 
   sal_real_dig = ts(sal_nom_dig*(ipc[174]/ipc), start=c(2002,10), end = c(2017,3), freq=12),
   sal_nom_ita = subset(ing_todos_prov, provincia =="Itata")$ingreso, 
   sal_real_ita = ts(sal_nom_ita*(ipc[174]/ipc), start=c(2002,10), end = c(2017,3), freq=12),
   sal_nom_pun = subset(ing_todos_prov, provincia =="Punilla")$ingreso, 
   sal_real_pun = ts(sal_nom_pun*(ipc[174]/ipc), start=c(2002,10), end = c(2017,3), freq=12),
   sal_nom_p = subset(ing_todos_act, sector =="Pesca")$ingreso,
   sal_nom_im = subset(ing_todos_act, sector =="Industria Manufacturera")$ingreso,
   sal_nom_ega = subset(ing_todos_act, sector =="Electricidad, Gas y Agua")$ingreso,
   sal_nom_c = subset(ing_todos_act, sector =="Comercio")$ingreso,
   sal_nom_tc = subset(ing_todos_act, sector =="Transporte y Comunicaciones")$ingreso,
   sal_nom_ssp = subset(ing_todos_act, sector =="Servicios Sociales y Personales")$ingreso,
   sal_nom_ap = subset(ing_todos_act, sector =="Administracion Publica")$ingreso,
   sal_nom_ane = subset(ing_todos_act, sector =="Actividad no especificada")$ingreso,
   sal_nom_agcs = subset(ing_todos_act, sector =="Agricultura, Ganadería, Caza y Silvicultura")$ingreso,
   sal_nom_emc = subset(ing_todos_act, sector =="Explotación de Minas y Canteras")$ingreso,
   sal_nom_con = subset(ing_todos_act, sector =="Construcción")$ingreso,
   sal_nom_hr = subset(ing_todos_act, sector =="Hoteles y Restaurantes")$ingreso,
   sal_nom_if = subset(ing_todos_act, sector =="Intermediación Financiera")$ingreso,
   sal_nom_aiea = subset(ing_todos_act, sector =="Actividades Inmobiliarias, Empresariales y de Alquiler")$ingreso, 
   sal_real_p = sal_nom_p*(ipc[174]/ipc),
   sal_real_im = sal_nom_im*(ipc[174]/ipc),
   sal_real_ega = sal_nom_ega*(ipc[174]/ipc),
   sal_real_c = sal_nom_c*(ipc[174]/ipc),
   sal_real_tc = sal_nom_tc*(ipc[174]/ipc),
   sal_real_ssp = sal_nom_ssp*(ipc[174]/ipc),
   sal_real_ap = sal_nom_ap*(ipc[174]/ipc),
   sal_real_ane = sal_nom_ane*(ipc[174]/ipc),
   sal_real_agcs = sal_nom_agcs*(ipc[174]/ipc),
   sal_real_emc = sal_nom_emc*(ipc[174]/ipc),
   sal_real_con = sal_nom_con*(ipc[174]/ipc),
   sal_real_hr = sal_nom_hr*(ipc[174]/ipc),
   sal_real_if = sal_nom_if*(ipc[174]/ipc),
   sal_real_aiea = sal_nom_aiea*(ipc[174]/ipc),
   sal_nom_hom = subset(ingresos_, sex =="M")$ingreso,
   sal_nom_muj = subset(ingresos_, sex =="F")$ingreso,
   sal_real_hom = sal_nom_hom*(ipc[174]/ipc),
   sal_real_muj = sal_nom_muj*(ipc[174]/ipc))


diccionario = data.frame(variable = names(datos_hist)) %>% 
  mutate(descripción = c("fecha", "Salario Nominal Ñuble", 
                         "Índice de Precios al Consumidor", 
                         "Salario Real Ñuble", "Salario Nominal Diguillín", 
                         "Salario Real Diguillín", "Salario Nominal Itata", 
                         "Salario Real Itata", "Salario Nominal Punilla", 
                         "Salario Real Punilla", "Salario Nominal Pesca",
                         "Salario Nominal Industria Manufacturera", 
                         "Salario Nominal Electricidad, Gas y Agua", 
                         "Salario Nominal Comercio", "Salario Nominal transporte", 
                         "Salario Nominal servicios sociales",
                         "salario Nominal administración pública",
                         "Salario Nominal Actividad no especificada",
                         "Salario Nominal Agricultura", 
                         "Salario Nominal Minería", 
                         "Salario Nominal Construcción",
                         "Salario Nominal Hoteles y Restaurantes", 
                         "Salario Nominal Intermediacion Financiera", 
                         "Salario Nominal Actividades Inmobiliarias",
                         "Salario Real Pesca",
                         "Salario Real Industria Manufacturera", 
                         "Salario Real Electricidad, Gas y Agua", 
                         "Salario Real Comercio", "Salario Real transporte", 
                         "Salario Real servicios sociales",
                         "salario Real administración pública",
                         "Salario Real Actividad no especificada",
                         "Salario Real Agricultura", 
                         "Salario Real Minería", 
                         "Salario Real Construcción",
                         "Salario Real Hoteles y Restaurantes", 
                         "Salario Real Intermediacion Financiera", 
                         "Salario Real Actividades Inmobiliarias", 
                         "Salario Nominal Hombres Ñuble", 
                         "Salario Nominal Mujeres Ñuble", 
                         "Salario Real Hombres Ñuble",
                         "Salario Real Mujeres Ñuble"))
 
write.csv(datos_hist, "/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/observatoriolaboral/Boletines/bulletin_may/datos_hist.csv")
write.csv(diccionario, "/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/observatoriolaboral/Boletines/bulletin_may/diccionario.csv")

options(scipen = 10, digits = 10)
ts.plot(datos_hist$sal_real, datos_hist$sal_real_dig,datos_hist$sal_real_ita,
     datos_hist$sal_real_pun,col = 2:5,
     ylim = c(datos_hist$sal_real_pun[1],600000), lwd=1.5)
legend("topleft",legend = c("Ñuble", "Diguillín","Itata","Punilla"), 
       col =2:5, lty = 1)

datos_hist = datos_hist %>% mutate(year = year(date))

excluidas = c("sal_real","sal_real_dig","sal_real_ita","sal_real_pun","sal_real_hom","sal_real_muj")
act_econ = datos_hist %>% filter(year == 2016 | year == 2017) %>% 
  select(date,which(grepl(pattern = "^sal_real",names(datos_hist)))) %>% 
  select(-one_of(excluidas)) 

act_econ = data.table::setnames(act_econ, old = colnames(act_econ), 
                           new = as.character(c("date",names(table(datos$sector)))))
  
act_econ = act_econ[-c(1,2,3),]  
act_econ[,-1] = round(act_econ[,-1],0)

t_act_econ = data.table::transpose(act_econ[,-1])

colnames(t_act_econ) <- act_econ$date
rownames(t_act_econ) <- names(table(datos$sector))

write.csv(t_act_econ,"/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/observatoriolaboral/Boletines/bulletin_may/sector.csv")