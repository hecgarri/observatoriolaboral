setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cualitativo/Minería de Texto/FOCUS GROUP/")

rm(list=ls())

if (!require(tm)) install.packages("tm") # Paquete desarrollado para minería de texto
if (!require(qdap)) install.packages("qdap") # Análisis cuantitativo de discursos transcritos 
if (!require(qdapDictionaries)) install.packages("qdapDictionaries") 
if (!require(dplyr))  install.packages("dplyr")# Gestión de datos operadores de pipa 
if (!require(RColorBrewer)) install.packages("RColorBrewer") # Paletas de colores para los gráficos 
if (!require(ggplot2)) install.packages("ggplot2") # Herramientas gráficas 
if (!require(Scale)) install.packages("Scale") # Cambiar escalas 
if (!require(magrittr)) install.packages("magrittr") #
if (!require(Rgraphviz)) install.packages("Rgraphviz") # Gráficos de correlación (no disponible )
if (!require(wordcloud)) install.packages("wordcloud")

carpeta = "/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cualitativo/Minería de Texto/FOCUS GROUP/"

length(dir(carpeta))
dir(carpeta)
docs = Corpus(DirSource(carpeta, pattern =".txt$"))
class(docs)
summary(docs)
inspect(docs[1])
viewDocs = function(d,n){d %>% extract2(n) %>% as.character() %>%  writeLines()}
viewDocs(docs,1)

docs = tm_map(docs, removeNumbers) # Quitar números 

docs = tm_map(docs, removePunctuation) # Quitar comas, puntos, etcétera

docs = tm_map(docs, content_transformer(tolower))
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
             "esten", "tambien", "duble", "molestas", "mes", "ahed", "deda",
             "tfa", "erea", "aqued", "pequefos", "direda", "chiquillos", "cfmo", 
             "deceda", "tecnico", "afos", "habeda", "ased", "ace", "alle", "nifos", 
             "prectica", "sfaper", "sed", "med", "después", "afo", "diría", "incluso", 
             "voy", "dos", "haber", "forma", "súper", "dar", "toda", "buena", "posibilidad", 
             "x", "r", "e", "a", "c", "l", "m", "s", "h", "f", "v", "p", "n", "ir", 
             "si")

docs = tm_map(docs, removeWords, molestas)

#Pattern Matching and Replacement
toString = content_transformer(function(x,from, to) gsub(from, to, x))
docs = tm_map(docs, toString, "empleo", "trabajo")

dtm = DocumentTermMatrix(docs)

inspect(dtm[1:3,substr(colnames(dtm), 1, 3) == "vit"])

inspect(dtm[1:3,substr(colnames(dtm), 1, 3) == "tec"])

inspect(dtm[1:3,substr(colnames(dtm), 1, 3) == "tur"])

inspect(dtm[1:3,substr(colnames(dtm), 1, 3) == "ser"])

inspect(dtm[1:3,substr(colnames(dtm), 1, 5) == "const"])

inspect(dtm[1:3,substr(colnames(dtm), 1, 4) == "alim"])



turismo = list(
  list(word = "turismo", syns = c("turismo", "turista", "turistas", 
                                  "turística", "turístico", "turísticos"))
)
región = list(
  list(word = "región", syns = c("región", "region", "regifn"))
)

agricultura = list(
  list(word = "agricultura", syns = c("agricultura", "agrícola", "agricultor", 
                                      "agricultoras", "agricultores","agro", 
                                      "cosecha", "agredcola"))
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
  list(word = "berries", syns = c("arándano", "arándanos",
                                  "arandano", "arandano", "arendanos", "berries"))
)
frutales = list(
  list(word="frutales", syns =c("fruta", "frutas", "fruto", "frutos", "frutales"))
)
vino = list(
  list(word="vitivinícola", syns =c("vino", "vinos", "vitivinícola", "vitivinícolas",
                                    "viña", "viñedos", "vitivinicola", "viñas"))
)



replaceSynonyms <- content_transformer(function(x, syn=NULL) { 
  Reduce(function(a,b) {
    gsub(paste0("\\b(", paste(b$syns, collapse="|"),")\\b"), b$word, a)}, syn, x)   
})


#docs <- tm_map(docs, replaceSynonyms, agropecuario)
#docs <- tm_map(docs, replaceSynonyms, turismo)
docs <- tm_map(docs, replaceSynonyms, servicio)
docs <- tm_map(docs, replaceSynonyms, construccion)
docs <- tm_map(docs, replaceSynonyms, agroindustria)
docs <- tm_map(docs, replaceSynonyms, agricultura)
docs <- tm_map(docs, replaceSynonyms, frutales)
docs <- tm_map(docs, replaceSynonyms, región)
docs <- tm_map(docs, replaceSynonyms, arándano)
docs <- tm_map(docs, replaceSynonyms, vino)

dtm = DocumentTermMatrix(docs)

#dtm <- removeSparseTerms(dtm, 0.8) # This makes a matrix that is only 20% empty space, maximum.   

m = as.matrix(dtm)   
dim(m)   


findFreqTerms(dtm, lowfreq = 5)

#findAssocs(dtm, "agricultura", corlimit=0.9)

freq = sort(colSums(as.matrix(dtm)), decreasing=TRUE)

head(freq,30)

wf = data.frame(word = names(freq), freq = freq) # frecuencia de las palabras 

write.csv(wf, file="frecuencia.csv")

subset(wf, freq>20) %>%  ggplot(aes(word, freq))+ geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=45, hjust=1))


png("nube_transversal.png")
set.seed(1234)
wordcloud(names(freq), freq, min.freq = 2, scale=c(5,0.6), colors=brewer.pal(8, "Dark2"), 
          random.order = FALSE, random.color = TRUE)
dev.off()


library(fpc)
library(cluster)


d = dist(t(dtm), method="euclidian")

fit = hclust(d=d, method="ward.D")

plot(fit, hang=-1)


par(bty="l", family="times", bg="white")
d <- dist(t(dtm), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0, 
         main="Conglomerados")

write.csv(wf, "frecuencia.csv")

findAssocs(dtm, "turismo", 0.9)

findAssocs(dtm, c("control", "calidad"), 0.9)

findAssocs(dtm, "agrícola", 0.6)

findAssocs(dtm, "agroindustria", 0.6)

findAssocs(dtm, "servicios", 0.6)