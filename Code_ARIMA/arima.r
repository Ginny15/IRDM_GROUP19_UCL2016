memory.limit(32324)
library(data.table)  
library(Metrics)
library(forecast)
require(dplyr)
library(tidyr)
library(caret)
library(mlbench)
# data preprocessing
load_history <- read.csv("../input/Load_history.csv")
col2cvt <- 5:28
load_history[,col2cvt] <- lapply(load_history[,col2cvt],function(x){as.numeric(gsub(",", "", x))})
load_history_tidy <- load_history %>%  gather(hour, load, h1:h24)
load_history_tidy$hour <- sapply(load_history_tidy$hour, function(x) strsplit(x,"h")[[1]][2])
load_history_tidy$hour <- as.numeric(load_history_tidy$hour)
temperature_history <- read.csv("../input/temperature_history.csv")
temperature_history_tidy <- temperature_history %>%  gather(hour, temp, h1:h24)
temperature_history_tidy$hour <- sapply(temperature_history_tidy$hour, function(x) strsplit(x,"h")[[1]][2])
temperature_history_tidy$hour <- as.numeric(temperature_history_tidy$hour)
temperature_history_tidy <- spread(temperature_history_tidy, key = station_id, value = temp)
colnames(temperature_history_tidy)[5:15] <- sapply(
  colnames(temperature_history_tidy)[5:15],function(x) paste("station",x,sep="_")
)
whole = left_join(load_history_tidy,temperature_history_tidy,
                  by=c("year","month","day","hour"))
for (i in 1:17){
  whole[[i]] <- as.numeric(whole[[i]])
}

k <- 999                  # 1000 to be the size of training set
n <- 167                  # 168 to be the size of test set
itr <- 10320 - (1+k+n)    # itr is the steps ARIMA needs to move forward

zoneID = c(4,8,12,18)    # only picking these four zones to research

# used to store the average RMSE and R2 for each zone  
trainRMSE<- testRMSE<- trainR2 <- testR2<-  rep(0,4) 

index.j = 1 
# ARIMA using only history load data
for (j in c(4,8,12,18)){
  # initializing
  avgTrainRMSE <- avgTestRMSE<-  avgTrainR2 <- avgTestR2 <- 0
  zone.j <- whole[which(whole$zone_id==j),]
  zone.j <- whole[which(whole$zone_id==j),]
  zone.j$zone_id<-NULL   # choosing data for the specific zone
  zone.j<-zone.j[order(zone.j$year,zone.j$month,zone.j$day,zone.j$hour),]   #order by time
  zone.j.whole <- zone.j[1:10320,]  # choosing the firsr continuous period
  loadTS <-ts(zone.j.whole$load, frequency=24)  # transforming it into time series data 
  for(i in 1:itr){
    #using the first (k+1) data to train the model, fit it on the next (n+1) pieces of data
    training <- window(loadTS, start=1+(i-1)/24, end=1+(i+k-1)/24)  
    test <- window(loadTS, start=1 + (i+k)/24, end=1 + (i+k+n)/24)
    model <- Arima(training, order=c(0,0,0),seasonal = list(order=c(1,0,0)))
    fore.load <-forecast(model,h=n+1)
    back.load <- fitted(model)
    # calculate the average RMSE and R2 on both training set and test set
    avgTrainRMSE <- avgTrainRMSE+RMSE(training,back.load)/itr
    avgTestRMSE<- avgTestRMSE+RMSE(test,fore.load[4]$mean)/itr
    avgTrainR2 <- avgTrainR2+R2(training,back.load)/itr
    avgTestR2 <- avgTestR2 + R2(test,fore.load[4]$mean)/itr
  }
  trainRMSE[index.j] <- avgTrainRMSE
  testRMSE[index.j]<- avgTestRMSE
  trainR2[index.j] <- avgTrainR2
  testR2[index.j]<- avgTestR2
  index.j = index.j+1
}

#print the results
for (i in 1:4){
  print(paste(zoneID[i],trainRMSE[i],testRMSE[i],trainR2[i],testR2[i]))
}

index.j <- 1
# ARIMA with all temperatures as external variables
for (j in zoneID){
  # initializing
  avgTrainRMSE <- avgTestRMSE<-  avgTrainR2 <- avgTestR2 <- 0
  zone.j <- whole[which(whole$zone_id==j),]
  zone.j$zone_id<-NULL
  zone.j<-zone.j[order(zone.j$year,zone.j$month,zone.j$day,zone.j$hour),]
  zone.j.whole <- zone.j[1:10320,]
  xMatrix <-cbind(staion1=model.matrix(~zone.j.whole$station_1), 
                  station2=zone.j.whole$station_2,station3=zone.j.whole$station_3,
                  station4=zone.j.whole$station_4,station5=zone.j.whole$station_5,
                  station6=zone.j.whole$station_6,station7=zone.j.whole$station_7,
                  station8=zone.j.whole$station_8,station9=zone.j.whole$station_9,
                  station10=zone.j.whole$station_10,station11=zone.j.whole$station_11)
  loadTS <-ts(zone.j.whole$load, frequency=24) 
  for(i in 1:itr){
    training <- window(loadTS, start=1+(i-1)/24, end=1+(i+k-1)/24)
    test <- window(loadTS, start=1 + (i+k)/24, end=1 + (i+k+n)/24)
    xreg <- xMatrix[i:(i+k),-1]
    xregTest <- xMatrix[(1+i+k):(1+i+k+n),-1]
    model <- Arima(training, xreg=xreg,order=c(0,0,0),seasonal = list(order=c(0,0,0)))
    fore.load <-forecast(model,xreg=xregTest)
    back.load <- fitted(model)
    # RMSEtrain[i,index.j] <- RMSE(training,back.load)
    # RMSEtest[i,index.j]<- RMSE(test,fore.load[4]$mean)
    # R2train[i,index.j] <- R2(training,back.load)
    # R2test[i,index.j]<- R2(test,fore.load[4]$mean)
    avgTrainRMSE <- avgTrainRMSE+RMSE(training,back.load)/itr
    avgTestRMSE<- avgTestRMSE+RMSE(test,fore.load[4]$mean)/itr
    avgTrainR2 <- avgTrainR2+R2(training,back.load)/itr
    avgTestR2 <- avgTestR2 + R2(test,fore.load[4]$mean)/itr
    }
  trainRMSE[index.j] <- avgTrainRMSE
  testRMSE[index.j]<- avgTestRMSE
  trainR2[index.j] <- avgTrainR2
  testR2[index.j]<- avgTestR2
  index.j = index.j+1
}

#print the results
for (i in 1:4){
  print(paste(zoneID[i],trainRMSE[i],testRMSE[i],trainR2[i],testR2[i]))
}


  