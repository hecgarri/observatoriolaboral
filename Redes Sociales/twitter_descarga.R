
#> Abrir consola Ctrl+alt+t
#> sudo apt-get install libcurl4-gnutls-dev
#> https://apps.twitter.com/app/13429836/keys

#install.packages(c("twitteR", "RCurl"))

rm(list = ls())
#library(RCurl)
#library(ROAuth)
if(!require(twitteR)) install.packages("twitteR")
if(!require(httr)) install.packages("httr")
if(!require(magrittr)) install.packages("magrittr")

setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo/")

#?setup_twitter_oauth

key = "zVMw2wh4ByDmkfwjh9XBA1ErX"

secret = "4ELj9MXlJhwxuIY10dpRDraXo3P4a67nCNWOFGpHTsIi1vvonG"

secrettk = "1E7kdVG8aW10vexfQoZTkVca8IWZh1wFb14N6GShW8JBT"

mytoken = "237948745-lMxedMKS8iY8lt3KtbW5TW9elYJkJI2ZRQ4uEKM2"


setup_twitter_oauth(key, secret, mytoken, secrettk)

today = Sys.Date() %>% as.character()

yesterday = (Sys.Date()-6) %>% as.character()

Tweets = searchTwitter('empleo OR desempleo OR Salario OR Sence OR Laboral OR Economía',
                       n=50000, since=yesterday, 
                       resultType = "recent", geocode ='-36.60664,-72.10344,100km')

dfTweets = data.table::rbindlist(lapply(Tweets, as.data.frame))


x = " Percepciones sobre el trabajo.csv"

write.csv(dfTweets, paste0(Sys.Date(),x))