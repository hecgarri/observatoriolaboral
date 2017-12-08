## ------------------------------------------------------------------------
rm(list=ls())

## ------------------------------------------------------------------------
ls()

## ---- eval=FALSE---------------------------------------------------------
## file.choose()

## ---- eval=FALSE---------------------------------------------------------
## install.packages("foreign")

## ------------------------------------------------------------------------
if (!require(foreign)) install.packages("foreign"); require(foreign)

## ---- message=FALSE, warning=FALSE---------------------------------------
setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Encuesta de Caracterización Socioeconómica Nacional (CASEN)/")

casen<- read.spss("Casen 2015 SPSS.sav", use.value.labels = FALSE, 
                  to.data.frame = FALSE) 

## ---- eval=FALSE---------------------------------------------------------
## View(casen)

## ------------------------------------------------------------------------
class(casen)

## ------------------------------------------------------------------------
names(casen)[1:10]

## ------------------------------------------------------------------------
dim(casen)

## ------------------------------------------------------------------------

format(object.size(casen), units="MB")


## ------------------------------------------------------------------------
etiquetas <- attr(casen,"variable.labels") #Para extraer las etiquetas de las variables
diccionario.variables <- data.frame(var.name=names(casen),etiquetas) #Para hacer una tabla

casen = data.frame(casen)

## ---- eval=FALSE---------------------------------------------------------
## View(diccionario.variables)

## ------------------------------------------------------------------------
head(casen$expr,15)

tail(casen$expr,15)
## ------------------------------------------------------------------------
mean(casen$yautcorh)

## ------------------------------------------------------------------------
weighted.mean(casen$yautcorh, casen$expr)

## ---- eval=FALSE---------------------------------------------------------
## install.packages("survey")

## ---- message=FALSE------------------------------------------------------
if (!require(survey)) install.packages("survey"); require(survey)

## ------------------------------------------------------------------------
options(survey.lonely.psu="adjust") #Para evitar clusters con una sola observación
design<-svydesign(ids=casen$varunit, strata=casen$varstrat, weights=casen$expr, nest=TRUE, data=casen)

## ------------------------------------------------------------------------
svymean(casen$yautcorh, design)

## ------------------------------------------------------------------------
svyciprop(~sexo=="hombre", design, method="logit")

## ---- fig.align='center', fig.cap="Tipo de Contrato", fig.width=7, fig.height=5----
tabla.contrato<-svytable(~casen$o16, design)
tabla.contrato
tipo.contrato<-prop.table(tabla.contrato)*100
tipo.contrato
pie(tipo.contrato, labels=c("Plazo indefinido - 74.51%", "Plazo fijo - 25.28%", "ns/nr - 0.2%"),
    main="Tipo de contrato", family="Times", col=c("red", "blue", "green"))

## ---- eval=FALSE---------------------------------------------------------
## install.packages("plotrix")

## ------------------------------------------------------------------------
if (!require(plotrix)) install.packages("plotrix"); require(plotrix)

## ------------------------------------------------------------------------
lbls<-c("Plazo indefinido - 74.51%", "Plazo fijo - 25.28%", "ns/nr - 0.2%")
pie3D(as.numeric(tipo.contrato), labels=lbls,
      explode=0.1,main="Tipo de Contrato ")


## ------------------------------------------------------------------------
cat.activ<-svytable(~casen$activ, design)
cat.activ
prop.activ<-prop.table(cat.activ)*100
prop.activ
lbls<-c("Ocupados", "Desocupados", "Inactivos")
pie3D(as.numeric(prop.activ), labels=lbls,
      explode=0.1,main="Categoría Ocupacional")

## ------------------------------------------------------------------------
pie3D(as.numeric(prop.activ), labels=lbls,
      explode=0.1,main="Categoría Ocupacional", col=gray(seq(0.4,1.0, length=3)))

## ------------------------------------------------------------------------
cat.activ.sex<-svytable(~casen$activ+sexo, design)
cat.activ.sex
prop.activ.sex<-prop.table(cat.activ.sex)*100
prop.activ.sex

## ------------------------------------------------------------------------
class(cat.activ.sex)

## ------------------------------------------------------------------------
dim(prop.activ.sex)

## ------------------------------------------------------------------------
par(mfrow=c(1,2))
lbls<-c("Ocupados", "Desocupados", "Inactivos")
pie3D(as.numeric(prop.activ.sex[,1]), labels=lbls,
      explode=0.1,main="Categoría Ocupacional - Hombres")
pie3D(as.numeric(prop.activ.sex[,2]), labels=lbls,
      explode=0.1,main="Categoría Ocupacional - Mujeres")

## ------------------------------------------------------------------------
cat.ocup<-svytable(~casen$o15, design)
cat.ocup
prop.ocup<-prop.table(cat.ocup)*100
prop.ocup

## ---- eval = FALSE-------------------------------------------------------
## library(ggplot2)
## 
## cat.ocup.grafico<-ggplot(subset(casen, !is.na(o15)), aes(factor(o15), weight=expr))+geom_bar()+ylab("Porcentaje")+xlab("Categoría Ocupacional")+coord_flip()+geom_text(aes(label =ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black")
## 
## cat.ocup.grafico
## 

## ------------------------------------------------------------------------
levels(casen$o15)<-c("Patrón o empleador", "Trabajador por cuenta propia","Empleado u obrero del sector público", "Empleado u obrero de empresas públicas", "Empleado u obrero del sector privado", "Servicio Doméstico puertas adentro", "Servicio Doméstico puertas afuera", "FF.AA. y del Orden", "Familiar no remunerado")

## ------------------------------------------------------------------------
if (!require(ggplot2)) install.packages("ggplot2"); require(ggplot2)

cat.ocup.grafico = ggplot(subset(casen, !is.na(o15)), aes(factor(o15), weight=expr))+ylab("Número de trabajadores")+xlab("Categoría Ocupacional")+geom_bar(fill="blue")+coord_flip()+geom_text(aes(label =ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust=0.5, hjust=-.05)+scale_y_continuous(labels=scales::comma, limits=c(0,520000))

cat.ocup.grafico




