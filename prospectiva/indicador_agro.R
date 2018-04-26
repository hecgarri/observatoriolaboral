rm(list=ls())

if (!require(readxl)) install.packages("readxl"); require(readxl)
if (!require(dplyr)) install.packages("dplyr"); require(dplyr)
if (!require(forecast)) install.packages("forecast"); require(forecast)
if (!require(xts)) install.packages("xts"); require(xts)
if (!require(stringr)) install.packages("stringr"); require(stringr)


data <- read_excel("~/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Odepa cultivosRegional042017.xls", 
                     sheet = "Serie producción regional", 
                     range = "A4:AF319",
                     col_types = c("text", 
                     "text", "numeric", "numeric", "numeric", "numeric", 
                     "numeric", "numeric", "numeric", "numeric", "numeric", 
                     "numeric", "numeric", "numeric", "numeric", "numeric", 
                     "numeric", "numeric", "numeric", "numeric", "numeric", 
                     "numeric", "numeric", "numeric", "numeric", 
                     "numeric", "numeric", "numeric", "numeric", "numeric", 
                     "numeric", "numeric"))

cultivos = c('Trigo Total', 'Maíz Total',
             'Avena', 'Poroto Total',
             'Cebada Total',
             'Arroz','Papa', 
             'Raps',
             'Tabaco')

peso_original = c(0.02058,0.02615,
                  0.00500,0.00862,
                  0.00068,
                  0.00169,0.02121,
                  0.00648,
                  0.00054)
peso = peso_original/sum(peso_original)

ponderado = data.frame(cultivos,peso_original, peso)


datos = data %>% filter(Región =="08 Bío Bío") %>% select(c('Año agrícola',cultivos))

year = str_split_fixed(datos$`Año agrícola`,"/",2)

datos = datos %>% mutate(año = as.numeric(year[,1]))

datos[,-1] = as.xts(datos[,-1])

datos = mutate(datos, indicador = weighted.mean(datos[,-1]))










