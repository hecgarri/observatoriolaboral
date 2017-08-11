
#Fijo el directorio de trabajo 
setwd("~/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/GitHub/observatoriolaboral/Redes Sociales")

# Para utilizar la API de Facebook es necesario utilizar o un token temporal
# o permanente. 

# token generated here: https://developers.facebook.com/tools/explorer 
# El token es temporal, hay que redirigirse a la dirección cada vez. 
if (!require(Rfacebook)) devtools::install_github("pablobarbera/Rfacebook/Rfacebook")
if (!require(data.table)) install.packages("data.table")

token = "EAACEdEose0cBAMQ2J6OpefeK0ZBUCyiYlKNLQcMIWyYjgMwWVABbuPmxbXtdZCNkZBRbZBTRoBjbdr6DdWxQCSEZALOo1SMpsqGymqMaugxSXI0ZCs1xPSkmKoZB6dvL48V2BONNffzyjhTJXoqT2eDmq4KcaiMYEwYlfaptaSqcgeCAroZCWZAlsZCbvdZCoVqTXwZD"


me <- getUsers("me", token, private_info=TRUE) #mis datos como usuario 

# con esta orden descargo los posts del grupo "Datos de trabajo en Chillán"
# Ojo que o el grupo debe ser público o bien, debo ser el administrador de éste. 
group3 = getGroup(group_id = 241293439569652, token = token, n=Inf)

# Tuve muchas dificultades al guardar el data.frame resultante en un archivo .csv
# Finalmente, decidí utilizar un archivo .Rdata y el problema se solucionó totalmente 

saveRDS(group3,paste0((Sys.Date()-1)," Datos de trabajo en Chillan.rds"))

list.files(pattern = ".rds$")

posts = readRDS("2017-08-09 Datos de trabajo en Chillan.rds")

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

viewDocs(corpus,200)

corpus = tm_map(corpus, content_transformer(gsub), pattern = "-", replacement = " ")

corpus = tm_map(corpus, removeNumbers) # Quitar números 

corpus = tm_map(corpus, removePunctuation) # Quitar comas, puntos, etcétera

corpus = tm_map(corpus, content_transformer(tolower))
#stopwords("spanish")


#Pattern Matching and Replacement
toString = content_transformer(function(x,from, to) gsub(from, to, x))
corpus = tm_map(corpus, toString, "empleo", "trabajo")

dtm = DocumentTermMatrix(corpus)

inspect(dtm[,substr(colnames(dtm), 1, 3) == "agr"])

inspect(dtm[,substr(colnames(dtm), 1, 3) == "tur"])

inspect(dtm[1:5,substr(colnames(dtm), 1, 3) == "ser"])

inspect(dtm[,substr(colnames(dtm), 1, 5) == "const"])

inspect(dtm[1:5,substr(colnames(dtm), 1, 4) == "alim"])

inspect(dtm[1:5,substr(colnames(dtm), 1, 3) == "dat"])

dato = list(
  list(word = "dato", syns = c("data", "datito", "datitogracias", "datitos", 
                               "dato", "datocomunicarse", "datomesirve", 
                               "datoooooo", "datopara", "datos"))
)
turismo = list(
  list(word = "turismo", syns = c("turismo", "turista", "turistas", 
                                  "turística", "turístico", "turísticos"))
)

vendedor = list(
  list(word = "vendedor/a", syns = c("vender", "vendedor", "vendedora", "ventas"))
)
gracias = list(
  list(word = "gracias", syns = c("agradece", "agradecería", "agradeceria", 
                                  "agradese", "agradezco"))
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
corpus <- tm_map(corpus, replaceSynonyms, vendedor)
corpus <- tm_map(corpus, replaceSynonyms, servicio)
corpus <- tm_map(corpus, replaceSynonyms, dato)
corpus = tm_map(corpus, replaceSynonyms, gracias)
corpus <- tm_map(corpus, replaceSynonyms, construccion)
corpus <- tm_map(corpus, replaceSynonyms, agroindustria)
corpus <- tm_map(corpus, replaceSynonyms, agricultura)
corpus <- tm_map(corpus, replaceSynonyms, frutales)

molestas = c(stopwords("spanish"),"v", "e", "acá", "creo", "tema","entonces",
             "área", "igual", "ahí","por", "pero", "cgt", "ejemplo", 
             "por","porque", "entonces", "p", "van", "día", "tener", "hacer", 
             "aquí", "hoy", "san", "ahora", "ser", "ver", "año", "bien", "bueno", 
             "años", "hace", "usted", "ustedes", "así", "hecho", "mejor", "mismo", "cómo", "fabián", 
             "podría", "puede", "veces", "después", "dentro", "obra", 
             "vamos", "parte", "cuáles", "persona", "gente", "comuna", 
             "personas", "cada", "cantidad", 
             "carlos", "cosas", "crecimiento", "fuerte", "grandes", 
             "hablando", "harto", "importante", "mano", "menos", "provincia", 
             "siempre", "trabajo", "vez", "vienen", "agua", "comunas", 
             "cuenta", "decía", "futuro","generar", "local", 
             "menor", "muchas", "nivel", "obviamente", "país", "poder", 
             "potencial", "realidad", "tiempo", "todavía", "trabajar", 
             "veo", "afuera", "alguna", "bastante", "cuanto", "decir", 
             "generan", "hacen", "hacia", "personal", "potenciar", "producción",
             "productivo", "proyectos", "pueda", "tipo", "trabajan", "viendo", 
             "visto", "antiguamente", "chile", "ciudad", "claro", "digamos", 
             "digo", "genera", "lado", "mismos", "muchas", 
             "mucha", "papá", "pasa", "principalmente", "productos", "quiere", 
             "quizás", "seguir", "sectores", "vecinos", "zona", 
             "recursos", "allá", "atrás", "cultura", "general", "condiciones", 
             "gran", "grande", "hijos", "lados", "ligados", 
             "línea", "meses", "permite", "propia", "quieren", 
             "resulta", "salen", "tan", "iba", "lamentablemente", "debe", 
             "final", "falta", "dependemos", "crecido", "cambiar", 
             "cambiado", "verdad", "tampoco", "quedar", "queda", "nuevas", 
             "necesitan", "mayor", "ido", "dando", "casi", "buenas", 
             "solo", "solamente", "sentido", "principal", "nueva", "mil", 
             "mejorar", "mayoría", "intento", "haciendo", "generando", "creciendo", 
             "existe", "cosa", "cierto", "caballero", "buscar", "buen", "baja", 
             "algún", "todas", "punto", "pueden", "problema", "ligado", "dado", 
             "adelante", "aumento", "minuto", "misma", "sé", "poquito", "también", 
             "esten", "tambien", "duble", "dato", "necesita", "cualquier", 
             "busco", "experiencia", "gracias", "hola", "httpelresumenclse",
             "mas", "disponibilidad", "llamar", "algun", "inmediata", 
             "interesados", "edad", "necesito","hogar", "interesadas", "alguien", 
             "inbox", "información", "lunes", "quieres", "esperamos", "sirve", 
             "urgente", "hrs", "niños", "ganar", "horas", "favor", "obten", 
             "wasap", "obten", "requisitos", "curriculum", "ofrece", "horario", 
             "busca", "laboral", "medio", "buena", "requiere", "oportunidad", 
             "responsable", "contratar","cupos", "ingresos", "interno", 
             "traslado", "mañana", "celular", "sabe", "consultas", "semana", 
             "presentar", "quieran", "quieran", "deudas", "asistencia", "aprovecha", 
             "antecedentes", "comentarios", "contacto", "whatsapp", 
             "domicilio", "consulta", "dia", "trabaja", "casa", "sólo", 
             "socios", "extra", "mail", "vigente", "cancela", 
             "chillan", "enviar","semanal", "chillán", "obtén", 
             "horarios", "fono", "ganas","regístrate", "viernes", 
             "pedidos", "etc", "presentarse", "httpspartnersubercomifudtue",
             "tardes", "aprox", "necesitamos", "numero", "llegan", "días", 
             "tarde", "villa", "dudas", "comodidad", "comunicarse", "pagos", 
             "por favor", "hogares", "presencia", "alta", "gana", 
             "oriente", "platita", "cubrir", "valor", "sábado", "atenta", 
             "arriba", "decide", "puedes", "verdaderas", 
             "sueldo","además", "diciembre", "puertas", "gratis", "extras", 
             "ambiente", "uberdostclgmailcom", "calle", "descuento", 
             "número","correo", "boleta", "iquique", "pack","premios", 
             "pago", "venegas", "don", "vida")

corpus = tm_map(corpus, removeWords, molestas)


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

subset(wf, freq>100) %>%  ggplot(aes(x = reorder(word, -freq),y= freq))+ geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=45, hjust=1))+ xlab("Términos") +ylab("Frecuencia")+
  ggtitle("Los términos más frecuentes", 
          subtitle = "mencionados más de 100 veces")+labs(caption="Elaboración propia a partir de datos de Facebook")

subset(wf, freq>80 & freq<100) %>%  ggplot(aes(x = reorder(word, -freq),y= freq))+ geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=45, hjust=1))+ xlab("Términos") +ylab("Frecuencia")+
  ggtitle("Los términos más frecuentes", 
          subtitle ="mencionados entre 75 y 99 veces")+labs(caption="Elaboración propia a partir de datos de Facebook")

subset(wf, freq>65 & freq<80) %>%  ggplot(aes(x = reorder(word, -freq),y= freq))+ geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=45, hjust=1))+ xlab("Términos") +ylab("Frecuencia")+
  ggtitle("Los términos más frecuentes", 
          subtitle ="mencionados entre 60 y 74 veces")+labs(caption="Elaboración propia a partir de datos de Facebook")

head(wf, 150)

palabras = as.character(head(wf,10)$word)
findAssocs(dtm,palabras,rep(0.6,10))



findAssocs(dtm, "part",0.6)

findAssocs(dtm, "vendedor",0.6)

findAssocs(dtm, "certificado", 0.6)

findAssocs(dtm, "curso", 0.6)

findAssocs(dtm, "vender", 0.6)

findAssocs(dtm, "aseo", 0.6)

findAssocs(dtm, "licencia", 0.6)

findAssocs(dtm, "maestro", 0.6)

findAssocs(dtm, "comisiones", 0.6)



set.seed(1234)
wordcloud(names(freq), freq, min.freq = 2, scale=c(5,0.6), colors=brewer.pal(8, "Dark2"), 
          random.order = FALSE, random.color = TRUE)

if (!require(fpc)) install.packages("fpc")
require(cluster)


d = dist(t(dtm), method="euclidian")

fit = hclust(d=d, method="ward.D")

plot(fit, hang=-1)


par(bty="l", family="times", bg="white")
d <- dist(t(dtm), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0, 
         main="Conglomerados")



