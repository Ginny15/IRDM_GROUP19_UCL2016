memory.limit(32324)
library(data.table)  
library(Metrics)
library(forecast)
require(dplyr)
library(tidyr)
library(caret)
library(mlbench)

# data preprocessing
source("data_preprocessing.r")

for (i in 1:17){
  whole[[i]] <- as.numeric(whole[[i]])
}

# firstly pick out those rows missing load, which means that they need to be forecast or backcast
# the specific hours in June 30th, 2008 have to be removed later
testRaw <-whole[which(is.na(whole$load)),]
testRaw <- testRaw [order(testRaw$zone_id,testRaw$year,testRaw$month,testRaw$day,testRaw$hour),]

# picking out the ids where we need to predict the temperature data
id <- which(is.na(testRaw$station_1)) 

# for each zone, the temperature on the same day is the same
# so pick out zone 1 and use its history data to forecast temperature and copy into other zones
zone.j <- whole[which(whole$zone_id==1),]
zone.j$zone_id<-NULL
zone.j<-zone.j[order(zone.j$year,zone.j$month,zone.j$day,zone.j$hour),]

#8 week holes in the data, each containing (24*7=)168 pieces of data

#head index of the last continuous data segment 
which(is.na(zone.j$load))[8*168+1]  # =25513

#tail index of the last continuous data segment  
which(is.na(zone.j$load))[8*168] # =39414  

#tail index of the last week we need to predict 
rev(which(is.na(zone.j$load)))[1]  # =39600

#select columns with only temperature & rows used as training set
zone.j.whole <- zone.j[25513:39414,6:16]

for(i in 1:11){
  # using data from 29/11/2006 01:00 to 30/6/2008 06:00
  # to forecast temperature from 30/6/2008 07:00 to 7/7/2008 24:00
  temp.hist <- ts(zone.j.whole[[i]],frequency = 24)
  #using ARIMA(0,0,0)(1,0,2)_24  
  model <- Arima(temp.hist, order=c(0,0,0),seasonal = list(order=c(1,0,2)))
  fore.load <-forecast(model,h=39600-39414)[4]$mean
  for (n in 1:20){
    testRaw[id,][((n-1)*186+1):(186*n),(i+6)] <- fore.load
  }
}

write.csv(testRaw,"testRaw.csv")

