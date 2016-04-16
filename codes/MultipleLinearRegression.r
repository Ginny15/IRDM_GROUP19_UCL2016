memory.limit(32324)
library(data.table)  
library(Metrics)
library(forecast)
require(dplyr)
library(tidyr)
library(caret)
# data preprocessing
setwd("E:/GoogleDrive/Academic/Information_Retrieval_and_Data_Mining/Group")
load_history <- read.csv("Load_history.csv")
temperature_history <- read.csv("temperature_history.csv")

# removing the comma in the load data
col2cvt <- 5:28
load_history[,col2cvt] <- lapply(load_history[,col2cvt],function(x){as.numeric(gsub(",", "", x))})

# taking columns h1:h24, and gathering them into (hour, load) pairs
load_history_tidy <- load_history %>%  gather(hour, load, h1:h24)
load_history_tidy$hour <- sapply(load_history_tidy$hour, function(x) as.numeric(strsplit(x,"h")[[1]][2]))

# taking columns h1:h24, and gathering them into (hour, temperature) pairs
temperature_history_tidy <- temperature_history %>%  gather(hour, temp, h1:h24)
temperature_history_tidy$hour <- sapply(temperature_history_tidy$hour, function(x) as.numeric(strsplit(x,"h")[[1]][2]))

# spreading the (staion_id, temperature) pairs into colomns station 1:station 11
temperature_history_tidy <- spread(temperature_history_tidy, key = station_id, value = temp)
colnames(temperature_history_tidy)[5:15] <- sapply(
  colnames(temperature_history_tidy)[5:15],function(x) paste("station",x,sep="_")
)

# merging the load data and temperature data up by the time
whole = left_join(load_history_tidy,temperature_history_tidy,
                  by=c("year","month","day","hour"))
whole = whole[which(!is.na(whole$load)),]


for (i in 1:17){
  whole[[i]] <- as.numeric(whole[[i]])
}

# performing a 10-fold cross validation on each training set
# and fitting on the rest 20% test set
# and comparing RMSE and R2 among these models

control = trainControl(method="cv", number=10, repeats=1)

# 1. treating all variables as numeric
for (j in c(4,8,12,18)) {
  zone.j <- whole[which(whole$zone_id==j),]
  zone.j$zone_id<-NULL
  zone.j.traning.id <- floor(0.80 * nrow(zone.j))
  set.seed(123)
  zone.j.traning.ids <- sample(seq_len(nrow(zone.j)), size = zone.j.traning.id)
  zone.j.traning <- zone.j[zone.j.traning.ids,]
  zone.j.test <- zone.j[-zone.j.traning.ids,]
  fit.zone.j = train(load~., data=zone.j.traning, method = "lm", trControl = control)
  predLoad = predict(fit.zone.j,zone.j.test)
  print(paste(j," ",fit.zone.j[4]$results[2][1,1]," ",
              RMSE(predLoad,zone.j.test$load))," ",fit.zone.j[4]$results[3][1,1]," ",
              R2(predLoad,zone.j.test$load))
}


# 2. categorizing time attributes
for (j in c(4,8,12,18)) {
  zone.j <- whole[which(whole$zone_id==j),]
  zone.j$zone_id<-NULL
  for (i in 1:4){
    zone.j[[i]] <- as.factor(zone.j[[i]])
  }
  zone.j.traning.id <- floor(0.80 * nrow(zone.j))
  set.seed(123)
  zone.j.traning.ids <- sample(seq_len(nrow(zone.j)), size = zone.j.traning.id)
  zone.j.traning <- zone.j[zone.j.traning.ids,]
  zone.j.test <- zone.j[-zone.j.traning.ids,]
  fit.zone.j = train(load~., data=zone.j.traning, method = "lm", trControl = control)
  predLoad = predict(fit.zone.j,zone.j.test)
  print(paste(j," ",fit.zone.j[4]$results[2][1,1]," ",
              RMSE(predLoad,zone.j.test$load))," ",fit.zone.j[4]$results[3][1,1]," ",
        R2(predLoad,zone.j.test$load))
}

# 3. nomalizing temperature
for (j in c(4,8,12,18)) {
  zone.j <- whole[which(whole$zone_id==j),]
  zone.j$zone_id<-NULL
  for (i in 1:4){
    zone.j[[i]] <- as.factor(zone.j[[i]])
  }
  for (i in 6:16){
    zone.j[[i]] <- (zone.j[[i]]-mean(zone.j[[i]]))/sd(zone.j[[i]])
  }
  zone.j.traning.id <- floor(0.80 * nrow(zone.j))
  set.seed(123)
  zone.j.traning.ids <- sample(seq_len(nrow(zone.j)), size = zone.j.traning.id)
  zone.j.traning <- zone.j[zone.j.traning.ids,]
  zone.j.test <- zone.j[-zone.j.traning.ids,]
  fit.zone.j = train(load~., data=zone.j.traning, method = "lm", trControl = control)
  predLoad = predict(fit.zone.j,zone.j.test)
  print(paste(j," ",fit.zone.j[4]$results[2][1,1]," ",
              RMSE(predLoad,zone.j.test$load))," ",fit.zone.j[4]$results[3][1,1]," ",
        R2(predLoad,zone.j.test$load))
}

# 4. adding T^3 as the new feature
for (j in c(4,8,12,18)) {
  zone.j <- whole[which(whole$zone_id==j),]
  zone.j$zone_id<-NULL
  for (i in 1:4){
    zone.j[[i]] <- as.factor(zone.j[[i]])
  }
  for(i in 1:11){
    TempHour <- zone.j[[i+5]]* zone.j[[i+5]]* zone.j[[i+5]]
    zone.j <- cbind(zone.j,TempHour)
    colnames(zone.j)[length(colnames(zone.j))] <-paste('TempCubic',i)
  }
  zone.j.traning.id <- floor(0.80 * nrow(zone.j))
  set.seed(123)
  zone.j.traning.ids <- sample(seq_len(nrow(zone.j)), size = zone.j.traning.id)
  zone.j.traning <- zone.j[zone.j.traning.ids,]
  zone.j.test <- zone.j[-zone.j.traning.ids,]
  fit.zone.j = train(load~., data=zone.j.traning, method = "lm", trControl = control)
  predLoad = predict(fit.zone.j,zone.j.test)
  print(paste(j," ",fit.zone.j[4]$results[2][1,1]," ",
              RMSE(predLoad,zone.j.test$load))," ",fit.zone.j[4]$results[3][1,1]," ",
        R2(predLoad,zone.j.test$load))
}

