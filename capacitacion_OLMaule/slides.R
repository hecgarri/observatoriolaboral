## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE)

## ------------------------------------------------------------------------

manager <- c(1, 2, 3, 4, 5)
date <- c("10/24/08", "10/28/08", "10/1/08", "10/12/08", "5/1/09")
country <- c("US", "US", "UK", "UK", "UK")
gender <- c("M", "F", "F", "M", "F")
age <- c(32, 45, 25, 39, 99)
q1 <- c(5, 3, 3, 3, 2)
q2 <- c(4, 5, 5, 3, 2)
q3 <- c(5, 2, 5, 4, 1)
q4 <- c(5, 5, 5, NA, 2)
q5 <- c(5, 5, 2, NA, 1)

leadership <- data.frame(manager, date, country, gender, age,
q1, q2, q3, q4, q5, stringsAsFactors=FALSE)


## ------------------------------------------------------------------------
leadership 

## ---- echo = FALSE, results = 'asis', message = "FALSE"------------------
Operador = c("+", "-", "*", "/", "^ o **", "x%%y", "x%/%y")
Descripción = c("Suma", "Resta", "Multiplicación", "División",
                "Exponenciación", "Modulo", "División entera")

if (!require(knitr)) install.packages("knitr"); require(knitr)

kable(cbind(Operador, Descripción), caption = "Diferentes operaciones")

## ---- eval = FALSE-------------------------------------------------------
## sumx = x1+x2
## 
## meanx = (x1+x2)/2
## 

## ------------------------------------------------------------------------

# forma 1
mydata<-data.frame(x1 = c(2, 2, 6, 4),
x2 = c(3, 4, 2, 8))
mydata$sumx <- mydata$x1 + mydata$x2
mydata$meanx <- (mydata$x1 + mydata$x2)/2

# forma 2
mydata <- transform(mydata,
sumx = x1 + x2,
meanx = (x1 + x2)/2)

# forma 3
if (!require(dplyr)) install.packages("dplyr"); require(dplyr)

mydata = mutate(mydata, 
sumx = x1+x2,
meanx = (x1+x2)/2)


## ---- results='asis'-----------------------------------------------------

operador = c("<", "<=", ">", ">=", "==", "!=", "!x",
             "x| y", "x & y", "isTRUE(x)")
descripción = c("Menor que", "Menor o igual que", 
                "Mayor que", "Mayor o igual que", 
                "Exactamene igual a", "Distinto de",
                "Negacinó de x", "x o y",
                "x y y (intersección)", "Prueba sobre x")
kable(cbind(operador,descripción))


## ------------------------------------------------------------------------
leadership$age[leadership$age== 99] <- NA

## ------------------------------------------------------------------------

# Forma 1

leadership$agecat[leadership$age > 75] <- "Elder"
leadership$agecat[leadership$age >= 55 & leadership$age <= 75] <- "Middle Aged"
leadership$agecat[leadership$age < 55] <- "Young"

# Forma 2 (dplyr)

leadership = mutate(leadership, agecat = ifelse(age>75,"Elder",
                                         ifelse(age>=55 & age<=75, "Middle Aged",
                                         ifelse(age<55, "Young", NA))))


## ---- eval = FALSE-------------------------------------------------------
## fix(leadership)

## ---- eval = TRUE--------------------------------------------------------

# Forma 1 paquete reshape

if (!require(reshape)) install.packages("reshape"); require(reshape)

leadership <- reshape::rename(leadership, c(manager="managerID", date="testDate"))
                     
# Forma 2 (dplyr)                     

#leadership = dplyr::rename(leadership, managerID = manager, testDate = date)


# Forma 3

names(leadership)[2] <- "testDate"

## ------------------------------------------------------------------------
leadership

## ------------------------------------------------------------------------
y = c(1,2,3,4,NA)

is.na(y)

which(is.na(y))
## ------------------------------------------------------------------------
is.na(leadership[,6:10])

## ------------------------------------------------------------------------
x <- c(1, 2, NA, 3)
y <- x[1] + x[2] + x[3] + x[4]
z <- sum(x)
y <- sum(x,na.rm = TRUE)

## ------------------------------------------------------------------------

newdata = na.omit(leadership)

newdata 


## ---- echo = FALSE, results = "asis"-------------------------------------

símbolo = c("%d", "%a", "%A", "%m", "%b", "%B","%Y", "%y")
significado = c("día como numero", "día de la semana abreviado", 
                "día de la semana completo", "mes número",
                "mes abreviado", "mes completo", "año dos dígitos",
                "año 4 dígitos")

if (!require(knitr)) install.packages("knitr"); require(knitr)


kable(cbind(símbolo, significado))


## ------------------------------------------------------------------------

strDates <- c("01/05/1965", "08/16/1975")

dates <- as.Date(strDates, "%m/%d/%Y")

myformat <- "%m/%d/%y"

leadership$testDate <- as.Date(leadership$testDate, myformat)


## ------------------------------------------------------------------------
Sys.Date()

date()

## ---- eval = FALSE-------------------------------------------------------
## help("ISOdatetime")
## 
## #ISOdate(year, month, day, hour = 12, min = 0, sec = 0, tz = "GMT")
## 
## ISOdate(2005,10,21,18,47,22,tz="PDT")
## 

## ---- echo = FALSE, results='asis'---------------------------------------
test = c("is.numeric()", "is.character()", "is.vector()", 
         "is.matrix()", "is.data.frame()", "is.factor()", 
         "is.logical()")
convert = c("as.numeric()", "as.character()", "as.vector()", 
            "as.matrix()", "as.data.frame()", "as.factor()", 
            "as.logical()")

kable(cbind(test,convert))

## ------------------------------------------------------------------------
 a <- c(1,2,3)
 a

 is.numeric(a)

 is.vector(a)

 a <- as.character(a)
 a

 is.numeric(a)

 is.vector(a)

 is.character(a)
 


## ------------------------------------------------------------------------
leadership 


newdata <- leadership[order(leadership$age),]

newdata

## ------------------------------------------------------------------------
# Forma 1 
newdata <- leadership[, c(6:10)]

# forma 2 (dplyr)

newdata = select(leadership, 'testDate', 'age')


## ------------------------------------------------------------------------
myvars <- names(leadership) %in% c("q3", "q4")
newdata <- leadership[!myvars]

newdata 

newdata <- leadership[c(-8,-9)]

## ------------------------------------------------------------------------
newdata <- leadership[1:3,]

newdata <- leadership[which(leadership$gender=="M" &
leadership$age > 30),]

## Forma 2 (dplyr)

newdata = filter(leadership, gender=="M" & age>30)


## ------------------------------------------------------------------------
set.seed(1234)

dplyr::sample_n(leadership, 2, replace = TRUE)


## ------------------------------------------------------------------------
if (!require(sqldf)) install.packages("sqldf"); require(sqldf)

newdf <- sqldf("select * from mtcars where carb=1 order by mpg",
row.names=TRUE)

## ---- echo = FALSE, results = 'asis', warning= FALSE---------------------
Function = c("abs(x)", "sqrt(x)", "ceiling(x)", "floor(x)",
             "trunc(x)", "round(x, digits = n)", 
             "signif(x,digits = n)", "cos(x), sin(x), tan(x)", 
             "log(x, base=n)", "log(x)", "log10(x)")

kable(Function)

