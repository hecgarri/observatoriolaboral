rm(list=ls())


setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Análisis Cuantitativo")

archivos = list.files(pattern=".csv$")
page = read.csv(archivos)

page[which.max(page$likes_count), ]

if (!require(tidyverse)) install.packages("tidyverse")
if (!require(stringr)) install.packages("stringr")

anuncios = page %>% filter(str_detect(message, 'trabajo|empleo|personal|trabajar|currículum|
                                      curriculum|pega|chofer|trabaje|maestro'))

## convert Facebook date format to R date format
format.facebook.date <- function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}
## aggregate metric counts over month
aggregate.metric <- function(metric) {
  m <- aggregate(page[[paste0(metric, "_count")]], list(month = page$month), 
                 sum)
  m$month <- as.Date(paste0(m$month, "-15"))
  m$metric <- metric
  return(m)
}

# create data frame with average metric counts per month
page$datetime <- format.facebook.date(page$created_time)
page$month <- format(page$datetime, "%Y-%m")
df.list <- lapply(c("likes", "comments", "shares"), aggregate.metric)

df <- data.table::rbindlist(df.list)
# visualize evolution in metric
library(ggplot2)
library(scales)
ggplot(df, aes(x = month, y = x, group = metric)) + geom_line(aes(color = metric)) + 
  scale_x_date(breaks = "years", labels = date_format("%Y")) + scale_y_log10("Average count per post", 
                                                                             breaks = c(10, 100, 1000, 10000, 50000)) + theme_bw() + theme(axis.title.x = element_blank())
