if (!require(gtrendsR)) install.packages("gtrendsR")
usr <- "<Google account email>"
psw <- "<Google account password>"
gconnect(usr, psw) 
lang_trend <- gtrends(c("data is", "data are"), res="week")
plot(lang_trend)

