rm(list=ls())

if (!require(dplyr)) install.packages("dplyr"); require(dplyr)
if (!require(foreign)) install.packages("foreign"); require(foreign)
if (!require(survey)) install.packages("survey"); require(survey)
if (!require(convey)) install.packages("convey"); require(convey)
if (!require(xtable)) install.packages("xtable"); require(xtable)

#setwd("C:\\Users\\Usuario\\Google Drive\\OLR Ñuble - Observatorio laboral de Ñuble\\Bases de datos\\Encuesta de Caracterización Socioeconómica Nacional (CASEN)\\")
setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Encuesta de Caracterización Socioeconómica Nacional (CASEN)/")

list.files()

casen2015 = read.spss("Casen 2015 SPSS.sav",use.value.labels = FALSE,
                      to.data.frame= FALSE, use.missings = TRUE)

etiquetas.casen = attr(casen2015, "label.table")
directorio.casen = attr(casen2015,"variable.labels")
directorio.casen = data.frame(names(directorio.casen),directorio.casen)

casen2015 = data.frame(casen2015)
names(casen2015) = tolower(names(casen2015))

nva_biobio = casen2015 %>% filter(provincia==81 |
                                    provincia==82 | provincia==83) 
nuble = casen2015 %>% filter(provincia==84)

design_n = svydesign(id = ~varunit, strata = ~varstrat,
                   weights = ~expr, nest = TRUE, data = casen2015)

design_bio = svydesign(id = ~varunit, strata = ~varstrat,
                       weights = ~expr, nest = TRUE, data = nva_biobio)

design_nub = svydesign(id = ~varunit, strata = ~varstrat,
                       weights = ~expr, nest = TRUE, data = nuble)

design_rep = convey_prep(design_n)
gini_nac = svygini(~yautcor, design = design_rep, na.rm = TRUE)

design_rep_bio = convey_prep(design_bio)
gini_bio = svygini(~yautcor, design = design_rep_bio, na.rm = TRUE)

design_rep_nuble = convey_prep(design_nub)
gini_nuble = svygini(~yautcor, design = design_rep_nuble, na.rm = TRUE)

minimo_nacional = svyratio(~I(yoprcor<=241000),
                           denominator = ~I(activ==1), 
                           design = design_n,na.rm = TRUE)

minimo_bio = svyratio(~I(yoprcor<=241000),
                           denominator = ~I(activ==1), 
                           design = design_bio,na.rm = TRUE)

minimo_nuble = svyratio(~I(yoprcor<=241000),
                      denominator = ~I(activ==1), 
                      design = design_nub,na.rm = TRUE)

prome_nac = svymean(~yoprcor, design = design_n, na.rm = TRUE)
prome_bio = svymean(~yoprcor, design = design_bio, na.rm = TRUE)
prome_nuble = svymean(~yoprcor, design = design_nub, na.rm = TRUE)

options("scipen"=10, "digits"=4)
mediana_nac = svyquantile(~yoprcor, design = design_n,
                          quantile = c(.25,0.5,.75), ci = FALSE, 
                          na.rm = TRUE)

mediana_bio = svyquantile(~yoprcor, design = design_bio,
                          quantile = c(.25,0.5,.75), ci = FALSE, 
                          na.rm = TRUE)

mediana_nuble = svyquantile(~yoprcor, design = design_nub,
                          quantile = c(.25,0.5,.75), ci = FALSE, 
                          na.rm = TRUE)


nuble = c(481355,482491,396159,355194,498864,558929,
                      1005850,380472,597514,921445,578980,529745)

diguillin = c(508076,497005,416278,360662,516462,561109,1002507,
              367194,636377,955557,601409,543011)
itata = c(471682,NA,318041,345542,447103,359894,NA,
          NA,522035,NA,460882,522175)
punilla = c(380865,NA,380655,350849,444137,604412,NA,
            449477,520423,NA,529433,462387)

data = data.frame(nuble,diguillin, itata, punilla)
rownames(data) = c("Actividades inmobiliares, empresariales y de alquiler",
                   "Actividad no especificada", "Administración Pública", 
                   "Agricultura, ganadería, caza y silvicultura", 
                   "comercio", "construcción", "Electricidad, gas y agua", 
                   "hoteles y restaurantes", "industria manufacturera", 
                   "intermediación financiera", "servicios sociales y personales",
                   "transporte y comunicaciones")
colnames(data) = c("Región de Ñuble", "Diguillín", "Itata", "Punilla")

xtable(data)