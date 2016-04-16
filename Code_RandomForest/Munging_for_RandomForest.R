memory.limit(32324)
library(data.table)  
library(Metrics)
library(forecast)
require(dplyr)
library(tidyr)

setwd("C:/Users/ShaneKong/Desktop/IRDM_group")
load_history <- read.csv("Load_history.csv")
col2cvt <- 5:28
load_history[,col2cvt] <- lapply(load_history[,col2cvt],function(x){as.numeric(gsub(",", "", x))})

load_history_tidy <- load_history %>%  gather(hour, load, h1:h24)
load_history_tidy$hour <- sapply(load_history_tidy$hour, function(x) strsplit(x,"h")[[1]][2])
load_history_tidy$hour <- as.numeric(load_history_tidy$hour)

temperature_history <- read.csv("temperature_history.csv")
temperature_history_tidy <- temperature_history %>%  gather(hour, temp, h1:h24)
temperature_history_tidy$hour <- sapply(temperature_history_tidy$hour, function(x) strsplit(x,"h")[[1]][2])
temperature_history_tidy$hour <- as.numeric(temperature_history_tidy$hour)
temperature_history_tidy <- spread(temperature_history_tidy, key = station_id, value = temp)
colnames(temperature_history_tidy)[5:15] <- sapply(
  colnames(temperature_history_tidy)[5:15],function(x) paste("station",x,sep="_")
)

whole = left_join(load_history_tidy,temperature_history_tidy,
                  by=c("year","month","day","hour"))



whole$load <- as.integer(whole$load)

write.csv(whole, file = "wholeRF.csv")
