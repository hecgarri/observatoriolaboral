
#Fijo el directorio de trabajo 
setwd("~/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/GitHub/observatoriolaboral/Redes Sociales")

# Para utilizar la API de Facebook es necesario utilizar o un token temporal
# o permanente. 

# token generated here: https://developers.facebook.com/tools/explorer 
# El token es temporal, hay que redirigirse a la dirección cada vez. 
if (!require(Rfacebook)) devtools::install_github("pablobarbera/Rfacebook/Rfacebook")
if (!require(data.table)) install.packages("data.table")

token = "EAACEdEose0cBAIC0ZAruTKqvsJQciDxvqZCDwkYvRypGAyrJf7QKYHUPuP3dHZC9nomuBi7FL93W6BXQ5WX4ZCeQ1b8GVbwNwA3Hp0w46cZC34KQZACCagNw0TLWWU8IvyVxMrEFXLxBZCgkrnmxvgcG7IB7rtTZAMtOez1R74wE3B2HkS669wVFcFFXQLTCjIgZD"


me <- getUsers("me", token, private_info=TRUE) #mis datos como usuario 

# con esta orden descargo los posts del grupo "Datos de trabajo en Chillán"
# Ojo que o el grupo debe ser público o bien, debo ser el administrador de éste. 
group3 = getGroup(group_id = 241293439569652, token = token, n=Inf)


# Para solucionar un problema con el ENCODING tuve que utilizar la siguiente
# secuencia de comandos en la consola
#Si hubiera en el fichero original, caracteres que no pudieran pasarse a la codificación final, el comando se detendría en el primer carácter erróneo y mostraría un mensaje de error. Es posible en estos casos hacer la conversión saltando los caracteres erróneos, para ello usaríamos la opción -c:
#  
#  $ iconv -f ascii -t utf8 -o destino.txt origen.txt
#iconv: secuencia de entrada ilegal en la posición 7
#$ iconv -c -f ascii -t utf8 -o destino.txt origen.txt


# Una vez descargados los datos procedo a guardarlos en un archivo .csv

write.csv(group3, paste0((Sys.Date()-1)," Datos de trabajo en Chillan.csv"), 
          fileEncoding = "UTF-8")

# Para solucionar un problema con el ENCODING tuve que utilizar la siguiente
# secuencia de comandos en la consola
#Si hubiera en el fichero original, caracteres que no pudieran pasarse a la codificación final, el comando se detendría en el primer carácter erróneo y mostraría un mensaje de error. Es posible en estos casos hacer la conversión saltando los caracteres erróneos, para ello usaríamos la opción -c:
#  
#  $ iconv -f ascii -t utf8 -o destino.txt origen.txt
#iconv: secuencia de entrada ilegal en la posición 7
#$ iconv -c -f ascii -t utf8 -o destino.txt origen.txt

saveRDS(group3,paste0((Sys.Date()-1)," Datos de trabajo en Chillan.rds"))

list.files(pattern = ".rds$")

posts = readRDS("2017-08-01 Datos de trabajo en Chillan.rds")

Encoding(posts$message) <- "UTF-8"

posts$message = iconv(posts$message, "UTF-8", "UTF-8",sub='')

#cargo una serie de librerías de utilidad en el análisis del discurso. 

if (!require(tm)) install.packages(tm) # Paquete desarrollado para minería de texto
if (!require(qdap)) install.packages("qdap") # Análisis cuantitativo de discursos transcritos 
if (!require(qdapDictionaries)) install.packages("qdapDictionaries") 
if (!require(tidyverse)) install.packages("tidyverse") # Gestión de datos operadores de pipa 
if (!require(RColorBrewer)) install.packages("RcolorBrewer") # Paletas de colores para los gráficos 
if (!require(Scale)) install.packages("Scale") # Cambiar escalas 
if (!require(wordcloud)) install.packages("wordcloud")

# Para tomar sólo los mensajes de los posteos y transformarlos en un 
# corpus, utilizaremos la siguiente secuencia de comandos. 
corpus = Corpus(VectorSource(posts$message))

class(corpus) # para ver la clase del corpus

inspect(corpus[1]) # para inspeccionar los mensajes 

viewDocs = function(d,n){d %>% magrittr::extract2(n) %>% as.character() %>%  writeLines()}

viewDocs(corpus,150)

corpus = tm_map(corpus, content_transformer(gsub), pattern = "-", replacement = " ")

corpus = tm_map(corpus, removeNumbers) # Quitar números 

corpus = tm_map(corpus, removePunctuation) # Quitar comas, puntos, etcétera

corpus = tm_map(corpus, content_transformer(tolower))
#stopwords("spanish")

molestas = c(stopwords("spanish"),"v", "e", "acá", "creo", "tema","entonces",
             "área", "igual", "ahí","por", "pero", "cgt", "ejemplo", 
             "por","porque", "entonces", "p", "van", "día", "tener", "hacer", 
             "aquí", "hoy", "san", "ahora", "ser", "ver", "año", "bien", "bueno", 
             "años", "hace", "usted", "ustedes", "así", "hecho", "mejor", "mismo", "cómo", "fabián", 
             "podría", "puede", "veces", "después", "dentro", "obra", "omil", 
             "vamos", "parte", "cuáles", "persona", "gente", "comuna", 
             "personas", "sector", "empresas", "cada", "cantidad", 
             "carlos", "chillán", "cosas", "crecimiento", "fuerte", "grandes", 
             "hablando", "harto", "importante", "mano", "menos", "provincia", 
             "siempre", "trabajo", "vez", "vienen", "agua", "comunas", 
             "cuenta", "decía", "empresa", "futuro","generar", "local", 
             "menor", "muchas", "nivel", "obviamente", "país", "poder", 
             "potencial", "realidad", "tiempo", "todavía", "trabajar", 
             "veo", "afuera", "alguna", "bastante", "cuanto", "decir", 
             "generan", "hacen", "hacia", "personal", "potenciar", "producción",
             "productivo", "proyectos", "pueda", "tipo", "trabajan", "viendo", 
             "visto", "antiguamente", "chile", "ciudad", "claro", "digamos", 
             "digo", "genera", "lado", "manejo", "mismos", "muchas", 
             "mucha", "papá", "pasa", "principalmente", "productos", "quiere", 
             "quizás", "seguir", "sectores", "trabajando", "vecinos", "zona", 
             "recursos", "allá", "atrás", "cultura", "general", "condiciones", 
             "gran", "grande", "hijos", "lados", "ligados", 
             "línea", "meses", "necesita", "permite", "propia", "quieren", 
             "resulta", "salen", "tan", "iba", "lamentablemente", "debe", 
             "final", "falta", "dependemos", "crecido", "cambiar", 
             "cambiado", "verdad", "tampoco", "quedar", "queda", "nuevas", 
             "necesitan", "mayor", "ido", "dando", "casi", "buenas", 
             "solo", "solamente", "sentido", "principal", "nueva", "mil", 
             "mejorar", "mayoría", "intento", "haciendo", "generando", "creciendo", 
             "existe", "cosa", "cierto", "caballero", "buscar", "buen", "baja", 
             "algún", "todas", "punto", "pueden", "problema", "ligado", "dado", 
             "adelante", "aumento", "minuto", "misma", "sé", "poquito", "también", 
             "esten", "tambien", "duble")

corpus = tm_map(corpus, removeWords, molestas)

#Pattern Matching and Replacement
toString = content_transformer(function(x,from, to) gsub(from, to, x))
corpus = tm_map(corpus, toString, "empleo", "trabajo")

dtm = DocumentTermMatrix(corpus)

inspect(dtm[1:5,substr(colnames(dtm), 1, 3) == "agr"])

inspect(dtm[1:5,substr(colnames(dtm), 1, 3) == "tur"])

inspect(dtm[1:5,substr(colnames(dtm), 1, 3) == "ser"])

inspect(dtm[1:5,substr(colnames(dtm), 1, 5) == "const"])

inspect(dtm[1:5,substr(colnames(dtm), 1, 4) == "alim"])



turismo = list(
  list(word = "turismo", syns = c("turismo", "turista", "turistas", 
                                  "turística", "turístico", "turísticos"))
)

agricultura = list(
  list(word = "agricultura", syns = c("agricultura", "agrícola", "agricultor", 
                                      "agricultoras", "agricultores","agro", 
                                      "cosecha"))
)

construccion  = list(
  list(word = "construcción", syns = c("constructora", "construcción", 
                                       "construir"))
)
servicio = list(
  list(word = "servicios", syns = c("servicio", "servicios"))
)

agroindustria = list(
  list(word = "agroindustria", syns = c("agroindustria", "industrial",
                                        "industrias", "industria"))
)
arándano = list(
  list(word = "arándano", syns = c("arándano", "arándanos",
                                   "arandano", "arandano"))
)
frutales = list(
  list(word="frutales", syns =c("fruta", "frutas", "fruto", "frutos", "frutales"))
)


replaceSynonyms <- content_transformer(function(x, syn=NULL) { 
  Reduce(function(a,b) {
    gsub(paste0("\\b(", paste(b$syns, collapse="|"),")\\b"), b$word, a)}, syn, x)   
})


#corpus <- tm_map(corpus, replaceSynonyms, agropecuario)
#corpus <- tm_map(corpus, replaceSynonyms, turismo)
corpus <- tm_map(corpus, replaceSynonyms, servicio)
corpus <- tm_map(corpus, replaceSynonyms, construccion)
corpus <- tm_map(corpus, replaceSynonyms, agroindustria)
corpus <- tm_map(corpus, replaceSynonyms, agricultura)
corpus <- tm_map(corpus, replaceSynonyms, frutales)



dtm = DocumentTermMatrix(corpus)

#dtm <- removeSparseTerms(dtm, 0.8) # This makes a matrix that is only 20% empty space, maximum.   

m = as.matrix(dtm)   
dim(m)   


findFreqTerms(dtm, lowfreq = 5)

#findAssocs(dtm, "agricultura", corlimit=0.9)

freq = sort(colSums(as.matrix(dtm)), decreasing=TRUE)

head(freq,30)

wf = data.frame(word = names(freq), freq = freq) # frecuencia de las palabras 

write.csv(wf, file="frecuencia.csv")

subset(wf, freq>150) %>%  ggplot(aes(word, freq))+ geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=45, hjust=1))


set.seed(1234)
wordcloud(names(freq), freq, min.freq = 2, scale=c(5,0.6), colors=brewer.pal(8, "Dark2"), 
          random.order = FALSE, random.color = TRUE)



require(fpc)
require(cluster)


d = dist(t(dtm), method="euclidian")

fit = hclust(d=d, method="ward.D")

plot(fit, hang=-1)


par(bty="l", family="times", bg="white")
d <- dist(t(dtm), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0, 
         main="Conglomerados")

write.csv(wf, "frecuencia.csv")

findAssocs(dtm, "turismo", 0.6)

findAssocs(dtm, "comercio", 0.6)

findAssocs(dtm, "agrícola", 0.6)

findAssocs(dtm, "agroindustria", 0.6)

findAssocs(dtm, "servicios", 0.6)
