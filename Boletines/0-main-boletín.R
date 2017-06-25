if (!require("data.table")) install.packages("data.table")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringi")) install.packages("stringi")
if (!require("survey")) install.packages("survey")
if (!require("parallel")) install.packages("parallel")
if (!require("xlsx")) install.packages("xlsx")

#-------------------------------------------------------------------------------
sessionInfo()
#-------------------------------------------------------------------------------
rm(list=ls())
#-------------------------------------------------------------------------------
# SE FIJA EL DIRECTORIO DE TRABAJO:
#setwd("C:\\Users\\Miguelo\\Documents\\NENE\\")
setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Nueva Encuesta Nacional de Empleo (ENE)/")

#-------------------------------------------------------------------------------
t <- proc.time() # Inicia el cronometro
#

nene = list.files(pattern=".csv$")

# ENEs
# 2010:01-11: Central: 01,04,07,10 
# 2011:12-23: Central: 13,16,19,22
# 2012:24-35: Central: 25,28,31,34
# 2013:36-47: Central: 37,40,43,46
# 2014:48-59: Central: 49,52,55,58
# 2015:60-71: Central: 61,64,67,70
# 2016:72-83: Central: 73,76,79,82
# 2017:84-95: Central: 85,88,91,94

# Se importan todas las bases establecidas en la secuencia de una sola vez
todas =  mclapply(nene,mc.cores=4,function(x) fread(x, sep=",", header=TRUE,
                select=c("id_directorio", "estrato", 
                         "fact", "cae_general", "mes_central", "b8", "b9", 
                         "ano_trimestre", "b18_codigo", "region", 
                         "categoria_ocupacion", "r_p_c","edad",
                         "c1", "c10", "c11", "habituales", "b7_3", "b7_4")))

#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLES region_e y prov_e
#-------------------------------------------------------------------------------
# Creamos una variable auxiliar que cuenta el numero de caracteres
for (i in 1:length(todas)){
  todas[[i]]$aux = stri_length(todas[[i]]$b18_codigo)
}

# Aqui el orden es muy importante. Primero el comando anterior, luego este
for (i in 1:length(todas)){
  todas[[i]]$region_e = todas[[i]]$b18_codigo
}

for (i in 1:length(todas)){
  todas[[i]]$region_e = ifelse(todas[[i]]$aux==5, 
                               stri_sub(todas[[i]]$region_e,1,2), 
                               todas[[i]]$region_e)
}

for (i in 1:length(todas)){
  todas[[i]]$region_e = ifelse(todas[[i]]$aux==4 
                               ,stri_sub(todas[[i]]$region_e,1,1),
                               todas[[i]]$region_e)
}

# Ahora la variable region_e contiene la region donde la persona trabaja.
for (i in 1:length(todas)){
  todas[[i]]$region_e<-strtoi(todas[[i]]$region_e)
}

for (i in 1:length(todas)){
  todas[[i]]$prov_e = ifelse(todas[[i]]$aux==5,                                 
                             stri_sub(todas[[i]]$b18_codigo,1,3), 
                             todas[[i]]$b18_codigo)
}

for (i in 1:length(todas)){
  todas[[i]]$prov_e = ifelse(todas[[i]]$aux==4 
                             ,stri_sub(todas[[i]]$prov_e,1,2),
                             todas[[i]]$prov_e)
}

#-------------------------------------------------------------------------------
for (i in 1:length(todas)){
  todas[[i]]$cae_general = recode(todas[[i]]$cae_general,`0`=0,`1` = 1,`2` = 1,
                                  `3` = 1, `4` = 2,`5` = 3,`6` = 4,
                                  `7` = 4,`8` = 4,`9` = 4)
}

for (i in 1:length(todas)){
  todas[[i]]$cae_general = factor(todas[[i]]$cae_general,levels = c(0,1,2,3,4), 
                                  labels = c("menor de 15","Ocupado", "Desocupado", 
                                             "Busca Trabajo Primera Vez","Inactivo"))
}

#-------------------------------------------------------------------------------
# Creamos una variable auxiliar que cuenta el numero de caracteres
for (i in 1:length(todas)){
  todas[[i]]$aux<-stri_length(todas[[i]]$r_p_c)
}

# Este codigo es para extraer la provincia 
for (i in 1:length(todas)){
  todas[[i]]$prov = ifelse(todas[[i]]$aux==5,                                 
                           stri_sub(todas[[i]]$r_p_c,1,3), 
                           todas[[i]]$r_p_c)
}

for (i in 1:length(todas)){
  todas[[i]]$prov = ifelse(todas[[i]]$aux==4,                                 
                           stri_sub(todas[[i]]$r_p_c,1,2), 
                           todas[[i]]$prov)
}

#-------------------------------------------------------------------------------
# Incorporar el info y realizar inferencia para muestras complejas
#-------------------------------------------------------------------------------
#Para evitar clusters con una sola observacion
options(survey.lonely.psu = "certainty") 

# info muestral de la NENE
info = mclapply(1:length(todas),mc.cores=4, function(i)
        svydesign(id = ~id_directorio, strata = ~estrato, weights  = 
                          ~fact,nest = TRUE, data = todas[[i]]))


#-------------------------------------------------------------------------------
# FIN DEL MAIN
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# ESTADISTICAS PRINCIPALES DE NUBLE
#-------------------------------------------------------------------------------
# Numero y tasa de desempleo a nivel de Nuble
#-------------------------------------------------------------------------------
#
n = length(info)

desocupados.nuble. =mclapply(1:n,mc.cores=4, function(i) svytotal(~I(cae_general=="Desocupado" | 
                                          cae_general=="Busca Trabajo Primera Vez"), 
                                     subset(info[[i]],prov==84), multicore= TRUE, 
                                     drop.empty.groups = FALSE, na.rm=TRUE))

tasa.desempleo.nuble. = mclapply(1:n,mc.cores=4, function(i)
                                svyratio(~I(cae_general=="Desocupado" | 
                                cae_general=="Busca Trabajo Primera Vez"),
                                denominator=~I(cae_general=="Ocupado" |
                                cae_general=="Desocupado" |
                                cae_general=="Busca Trabajo Primera Vez"),
                                subset(info[[i]],prov==84), multicore= TRUE, 
                                drop.empty.groups = FALSE, na.rm.all=TRUE))

desocupados.nuble = unlist(lapply(desocupados.nuble., '[[', 2) ) 
tasa.desempleo.nuble = unlist(lapply(tasa.desempleo.nuble., '[[', 1) ) 

ee.desocupados.nuble = unlist(lapply(desocupados.nuble., SE)) [seq(2, 2*length(desocupados.nuble),2)]
ee.tasa.desempleo.nuble = unlist(lapply(tasa.desempleo.nuble., SE))

#-------------------------------------------------------------------------------
# Numero y tasa de ocupados a nivel de nuble
#-------------------------------------------------------------------------------
#
ocupados.nuble. = mclapply(1:n,mc.cores=4, function(i) svytotal(~I(cae_general=="Ocupado"), 
                                  subset(info[[i]],prov==84), 
                                  multicore= TRUE, 
                                  drop.empty.groups = FALSE, na.rm=TRUE))
tasa.ocupados.nuble. =mclapply(1:n,mc.cores=4, function(i) svyratio(~I(cae_general=="Ocupado"),
                                       denominator=~I(edad>=15),
                                       subset(info[[i]],prov==84), 
                                       multicore= TRUE, 
                                       drop.empty.groups = FALSE, na.rm=TRUE))


ocupados.nuble = unlist(lapply(ocupados.nuble., '[[', 2) ) 
tasa.ocupados.nuble = unlist(lapply(tasa.ocupados.nuble., '[[', 1) ) 

ee.ocupados.nuble = unlist(lapply(ocupados.nuble., SE)) [seq(2, 2*length(ocupados.nuble),2)]
ee.tasa.ocupados.nuble = unlist(lapply(tasa.ocupados.nuble., SE))

#-------------------------------------------------------------------------------
# fuerza de trabajo y tasa de participacion de Nuble
#-------------------------------------------------------------------------------
#
fuerza.trabajo.nuble. =mclapply(1:n,mc.cores=4, function(i) svytotal(~I(cae_general=="Ocupado" | 
                                           cae_general=="Desocupado" | 
                                           cae_general=="Busca Trabajo Primera Vez"), 
                                          subset(info[[i]],prov==84), multicore= TRUE, 
                                          drop.empty.groups = FALSE, na.rm=TRUE))

tasa.participacion.nuble. = mclapply(1:n,mc.cores=4, function(i) svyratio(~I(cae_general=="Ocupado" | 
                                  cae_general=="Desocupado" | 
                                  cae_general=="Busca Trabajo Primera Vez"), 
                                  denominator=~I(edad>=15), subset(info[[i]],prov==84), 
                                  multicore= TRUE, drop.empty.groups = FALSE, na.rm=TRUE))

fuerza.trabajo.nuble = unlist(lapply(fuerza.trabajo.nuble., '[[', 2) ) 
tasa.participacion.nuble = unlist(lapply(tasa.participacion.nuble., '[[', 1) ) 

ee.fuerza.trabajo.nuble = unlist(lapply(fuerza.trabajo.nuble., SE)) [seq(2, 2*length(fuerza.trabajo.nuble),2)]
ee.tasa.participacion.nuble = unlist(lapply(tasa.participacion.nuble., SE))


#-------------------------------------------------------------------------------
# Tasa de subempleo Nacional y Nuble (sobre el total de ocupados)
#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLE: SUBEMPLEO
for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, subempleo =
                                 ifelse(c1==2 & c10==1 & c11==1 & habituales<=30,1,0))
}

#

tasa.promedio.subempleo.nacional. =mclapply(1:n,mc.cores=4, function(i) svyratio(~I(subempleo==1), 
                                   denominator=~I(cae_general=="Ocupado"),
                                   info[[i]],multicore = TRUE, 
                                   drop.empty.groups = FALSE, na.rm=TRUE))

tasa.promedio.subempleo.nuble. = mclapply(1:n,mc.cores=4, function(i) svyratio(~I(subempleo==1), 
                                    denominator=~I(cae_general=="Ocupado"),
                                    subset(info[[i]],prov==84),multicore = TRUE, 
                                    drop.empty.groups = FALSE, na.rm=TRUE))


tasa.promedio.subempleo.nacional = unlist(lapply(tasa.promedio.subempleo.nacional., '[[', 1) ) 
tasa.promedio.subempleo.nuble = unlist(lapply(tasa.promedio.subempleo.nuble., '[[', 1) ) 

ee.tasa.promedio.subempleo.nacional = unlist(lapply(tasa.promedio.subempleo.nacional., SE))
ee.tasa.promedio.subempleo.nuble = unlist(lapply(tasa.promedio.subempleo.nuble., SE))


#-------------------------------------------------------------------------------
# Trabajadores a cuenta propia en Nuble y Nacional
#-------------------------------------------------------------------------------
# CREACION DE LA VARIABLE: CATEGORIA OCUPACIONAL 
# (1:Empleador; 2:Cuenta Propia; 3:Trabajador dependiente)

for (i in 1:length(info)){
  info[[i]]$variables$cat.ocup = recode(info[[i]]$variables$categoria_ocupacion, `1`= 1, 
                                        `2`= 2, `3` =3, `4`=3, `5`=3, `6`=3,
                                        `7`=4, .default = 0, .missing = 99)
}

for (i in 1:length(info)){
  info[[i]]$variables = mutate(info[[i]]$variables, cat.ocup = ifelse(cat.ocup==3 & b8==2,3,
                                            ifelse(cat.ocup==3 & b8==1 & b9==1,4, 
                                            ifelse(cat.ocup==3 & b8==1 & b9==2,5, 
                                            ifelse(cat.ocup==1,1, 
                                            ifelse(cat.ocup==2,2, 
                                            ifelse(cat.ocup==4,6,NA)))))))
}

for (i in 1:length(info)){
  info[[i]]$variables$cat.ocup = factor(info[[i]]$variables$cat.ocup, 
                                        levels=c(1,2,3,4,5,6),
                                        labels=c("empleador","cuenta propia","asalariado sin contrato", 
                                                 "asalariado con contrato definido",
                                                 "asalariado con contrato indefinido",
                                                 "no remunerado"))
}


ocup_cuenta_propia_nuble. = mclapply(1:length(info),mc.cores = 4, function(i) svytotal(~I(cat.ocup=="cuenta propia"), 
                                        subset(info[[i]],prov==84), multicore= TRUE, 
                                        drop.empty.groups = FALSE, na.rm=TRUE))


ocup_cuenta_propia_nacional. =mclapply(1:length(info),mc.cores = 4, function(i) svytotal(~I(cat.ocup=="cuenta propia"), 
                                            info[[i]], multicore= TRUE, 
                                            drop.empty.groups = FALSE, na.rm=TRUE))

ocup_cuenta_propia_nuble = unlist(lapply(ocup_cuenta_propia_nuble., '[[', 2) ) 
ocup_cuenta_propia_nacional = unlist(lapply(ocup_cuenta_propia_nacional., '[[', 2) ) 

ee.ocup_cuenta_propia_nuble = unlist(lapply(ocup_cuenta_propia_nuble., SE)) [seq(2, 2*length(ocup_cuenta_propia_nuble),2)]
ee.ocup_cuenta_propia_nacional = unlist(lapply(ocup_cuenta_propia_nacional., SE)) [seq(2, 2*length(ocup_cuenta_propia_nacional),2)]


#-------------------------------------------------------------------------------
# Tasa de trabajadores a cuenta propia en Nuble y Nacional 
# (sobre el total de ocupados)
#-------------------------------------------------------------------------------

t_ocup_cuenta_prop_nuble. =mclapply(1:length(info),mc.cores = 4,
                          function(x) svyratio(~I(cat.ocup=="cuenta propia"), 
                          denominator=~I(cae_general=="Ocupado"),
                          subset(info[[x]],prov_e==84),multicore = TRUE, 
                          drop.empty.groups = FALSE, na.rm=TRUE))


t_ocup_cuenta_prop_nacional. =mclapply(1:n,mc.cores = 4,
                              function(x) svyratio(~I(cat.ocup=="cuenta propia"), 
                              denominator=~I(cae_general=="Ocupado"),
                              info[[x]],multicore = TRUE, 
                              drop.empty.groups = FALSE, na.rm=TRUE))

t_ocup_cuenta_prop_nuble =
  unlist(mclapply(t_ocup_cuenta_prop_nuble., '[[', 1) ) 
t_ocup_cuenta_prop_nacional = 
  unlist(mclapply(t_ocup_cuenta_prop_nacional., '[[', 1) ) 

ee.t_ocup_cuenta_prop_nuble = 
  unlist(mclapply(t_ocup_cuenta_prop_nuble., SE))
ee.t_ocup_cuenta_prop_nacional = 
  unlist(mclapply(t_ocup_cuenta_prop_nacional., SE))


#------------------------------------------------------------------
# Contrato de trabajo 
#------------------------------------------------------------------

tasa_contrato. = mclapply(1:n,mc.cores = 4, function(i)
  svyratio(~I(cat.ocup=="asalariado sin contrato"),
           denominator = ~I(cat.ocup=="asalariado sin contrato" | 
                            cat.ocup=="asalariado con contrato definido" | 
                            cat.ocup=="asalariado con contrato indefinido"), 
           design = info[[i]], multicore=TRUE, na.rm=TRUE))

tasa_contrato =
  unlist(mclapply(tasa_contrato., '[[', 1) ) 

ee_tasa_contrato = 
  unlist(mclapply(tasa_contrato., SE))


tasa_contrato_nuble. = mclapply(1:n,mc.cores = 4, function(i)
  svyratio(~I(cat.ocup=="asalariado sin contrato"),
           denominator = ~I(cat.ocup=="asalariado sin contrato" | 
                              cat.ocup=="asalariado con contrato definido" | 
                              cat.ocup=="asalariado con contrato indefinido"), 
           design = subset(info[[i]], prov_e==84), multicore=TRUE, na.rm=TRUE))

tasa_contrato_nuble =
  unlist(mclapply(tasa_contrato_nuble., '[[', 1) ) 

ee_tasa_contrato_nuble = 
  unlist(mclapply(tasa_contrato_nuble., SE))


proc.time()-t    # Detiene el cronometro



rm(list=ls(pattern="^ee"))

setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/Boletines/")

vectores <- Filter( function(x) 'numeric' %in% class( get(x) ), ls() )
indicadores = mclapply(vectores, function(x) get(x))
indicadores = do.call(cbind, indicadores) %>% `colnames<-` (vectores) %>% data.frame()


write.xlsx(indicadores, file = "indicadores.xlsx", sheetName = "principal", rownames = FALSE)


if (!require("ggplot2")) install.packages("ggplot2")

tasa_contrato_ = cbind(Nacional = tasa_contrato, Ñuble = tasa_contrato_nuble) %>% data.frame()

tasa_contrato_$fecha = seq(as.Date("2010/2/1"), by = "month", length.out = 86)

meltcontrato = melt(tasa_contrato_, id = "fecha") %>% 
  `colnames<-` (c("fecha", "tasa", "porcentaje"))

p = ggplot(meltcontrato,
           aes(x=fecha,y=porcentaje,colour=tasa,group=tasa)) +
  geom_line()

p

ggsave("sin_contrato.png")

subempleo_ = cbind(Nacional = tasa.promedio.subempleo.nacional, Ñuble = tasa.promedio.subempleo.nuble) %>% 
  data.frame()
subempleo_$fecha = seq(as.Date("2010/2/1"), by = "month", length.out = 86)

meltsubempleo = melt(subempleo_, id = "fecha") %>% 
  `colnames<-` (c("fecha", "tasa", "porcentaje"))

p2 = ggplot(meltsubempleo,
           aes(x=fecha,y=porcentaje,colour=tasa,group=tasa)) +
  geom_line()

ggsave("subempleo.png")
# FIN
