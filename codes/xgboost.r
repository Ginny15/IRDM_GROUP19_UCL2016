memory.limit(32324)
library(data.table)  
library(Metrics)
library(forecast)
library(tseries)
library(plyr)
require(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(readr)
require(gbm)

# data preprocessing
source("data_preprocessing.r") 

for (i in 1:17){
  whole[[i]] <- as.numeric(whole[[i]])
}
for (i in 1:4){
  whole[[i]] <- as.factor(whole[[i]])
}

# @han yang
#stationarity test

#auto-correlation function
Acf(log_value)

#partial auto-correlation function
Pacf(log_value)

#The Augmented Dickey-Fuller(ADF) t-statistic test: small p-values suggest the data is stationary
#and does not need to be differenced stationarity
adf.test(log_value,"stationary")

#The Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test; here accepting the null hypothesis means that
#the series is stationarity, and small p-values suggest that the series is not stationary and a differencing is required.
kpss.test(log_value)

#use exponential smoothing forecasting without information of temprature
load_history_new <- as.ts(load_history)
load_history_new_components <- decompose(load_history_new)
load_history_pred_expo <-HoltWinters(load_history_new,beta=FALSE,gamma=FALSE)

#do experiment on neural network model
fit.nn.zone1 = train(load ~., data=xgb_train_zone1_with_time, method = "nnet", trControl = control)
fit.nn.zone4 = train(load ~., data=xgb_train_zone4_with_time, method = "nnet", trControl = control)
fit.nn.zone8 = train(load ~., data=xgb_train_zone8_with_time, method = "nnet", trControl = control)
fit.nn.zone12 = train(load ~., data=xgb_train_zone12_with_time, method = "nnet", trControl = control)
fit.nn.zone18 = train(load ~., data=xgb_train_zone18_with_time, method = "nnet", trControl = control)

# xgboost model
whole_with_time = whole
whole_with_time$load <- as.integer(whole_with_time$load)
whole_with_time$year<-as.factor(whole_with_time$year)
whole_with_time$month<-as.factor(whole_with_time$month)
whole_with_time$day<-as.factor(whole_with_time$day)
whole_with_time$hour<-as.factor(whole_with_time$hour)
whole_with_time$zone_id <- as.factor(whole_with_time$zone_id)

whole_zone4_with_time <- whole_with_time[whole_with_time$zone_id==4,]
whole_zone8_with_time <- whole_with_time[whole_with_time$zone_id==8,]
whole_zone12_with_time <- whole_with_time[whole_with_time$zone_id==12,]
whole_zone18_with_time <- whole_with_time[whole_with_time$zone_id==17,]

#Data set splitting for xgboost(80% for training and 20% for testing) with whole dataset
sample_size <- floor(0.80 * nrow(whole_with_time))
set.seed(123)
xgb_train_ind <- sample(seq_len(nrow(whole_with_time)), size = sample_size)

xgb_train <- whole_with_time[xgb_train_ind,]
xgb_test <- whole_with_time[-xgb_train_ind,]

#Data set splitting of the original dataset of zone4 for xgboost(80% for training and 20% for testing)
sample_size_zone4_with_time <- floor(0.80 * nrow(whole_zone4_with_time))
set.seed(123)
xgb_train_ind_zone4_with_time <- sample(seq_len(nrow(whole_zone4_with_time)), size = sample_size_zone4_with_time)
xgb_train_zone4_with_time <- whole_zone4_with_time[xgb_train_ind_zone4_with_time,]
xgb_test_zone4_with_time <- whole_zone4_with_time[-xgb_train_ind_zone4_with_time,]

fit.xgb.zone4 = train(load ~., data=xgb_train_zone4_with_time, method = "xgbTree", trControl = control)

#Testing on the test set
xgb_test_zone4_with_time_X <- xgb_test_zone4_with_time[,!(colnames(xgb_test_zone4_with_time) %in% c("load"))]
xgb_test_zone4_with_time_y <- xgb_test_zone4_with_time[,"load"]
pred_zone4_with_time <- predict(fit.xgb.zone4, xgb_test_zone4_with_time_X)
postResample(pred_zone4_with_time,xgb_test_zone4_with_time_y)

#get importance score from the optimal xgboost model and avoids the normalisation
#step by setting scale to FALSE 
xgb_importance_zone4 <- varImp(fit.xgb.zone4,scale=TRUE)
xgb_importance_zone4
plot(xgb_importance_zone4,10)

#Data set splitting of the original dataset of zone8 for xgboost(80% for training and 20% for testing)
sample_size_zone8_with_time <- floor(0.80 * nrow(whole_zone8_with_time))
set.seed(123)
xgb_train_ind_zone8_with_time <- sample(seq_len(nrow(whole_zone8_with_time)), size = sample_size_zone8_with_time)
xgb_train_zone8_with_time <- whole_zone8_with_time[xgb_train_ind_zone8_with_time,]
xgb_test_zone8_with_time <- whole_zone8_with_time[-xgb_train_ind_zone8_with_time,]

fit.xgb.zone8 = train(load ~., data=xgb_train_zone8_with_time, method = "xgbTree", trControl = control)


#Testing on the test set
xgb_test_zone8_with_time_X <- xgb_test_zone8_with_time[,!(colnames(xgb_test_zone8_with_time) %in% c("load"))]
xgb_test_zone8_with_time_y <- xgb_test_zone8_with_time[,"load"]
pred_zone8_with_time <- predict(fit.xgb.zone8, xgb_test_zone8_with_time_X)
postResample(pred_zone8_with_time,xgb_test_zone8_with_time_y)

#get importance score from the optimal xgboost model and avoids the normalisation
#step by setting scale to FALSE 
xgb_importance_zone8 <- varImp(fit.xgb.zone8,scale=TRUE)
xgb_importance_zone8
plot(xgb_importance_zone8,10)

#Data set splitting of the original dataset of zone12 for xgboost(80% for training and 20% for testing)
sample_size_zone12_with_time <- floor(0.80 * nrow(whole_zone12_with_time))
set.seed(123)
xgb_train_ind_zone12_with_time <- sample(seq_len(nrow(whole_zone12_with_time)), size = sample_size_zone12_with_time)
xgb_train_zone12_with_time <- whole_zone12_with_time[xgb_train_ind_zone12_with_time,]
xgb_test_zone12_with_time <- whole_zone12_with_time[-xgb_train_ind_zone12_with_time,]

fit.xgb.zone12 = train(load ~., data=xgb_train_zone12_with_time, method = "xgbTree", trControl = control)

#Testing on the test set
xgb_test_zone12_with_time_X <- xgb_test_zone12_with_time[,!(colnames(xgb_test_zone12_with_time) %in% c("load"))]
xgb_test_zone12_with_time_y <- xgb_test_zone12_with_time[,"load"]
pred_zone12_with_time <- predict(fit.xgb.zone12, xgb_test_zone12_with_time_X)
postResample(pred_zone12_with_time,xgb_test_zone12_with_time_y)

#get importance score from the optimal xgboost model and avoids the normalisation
#step by setting scale to FALSE 
xgb_importance_zone12 <- varImp(fit.xgb.zone12,scale=TRUE)
xgb_importance_zone12
plot(xgb_importance_zone12,10)

#Data set splitting of the original dataset of zone12 for xgboost(80% for training and 20% for testing)
sample_size_zone18_with_time <- floor(0.80 * nrow(whole_zone18_with_time))
set.seed(123)
xgb_train_ind_zone18_with_time <- sample(seq_len(nrow(whole_zone18_with_time)), size = sample_size_zone18_with_time)
xgb_train_zone18_with_time <- whole_zone18_with_time[xgb_train_ind_zone18_with_time,]
xgb_test_zone18_with_time <- whole_zone18_with_time[-xgb_train_ind_zone18_with_time,]

fit.xgb.zone18 = train(load ~., data=xgb_train_zone18_with_time, method = "xgbTree", trControl = control)

#Testing on the test set
xgb_test_zone18_with_time_X <- xgb_test_zone18_with_time[,!(colnames(xgb_test_zone18_with_time) %in% c("load"))]
xgb_test_zone18_with_time_y <- xgb_test_zone18_with_time[,"load"]
pred_zone18_with_time <- predict(fit.xgb.zone18, xgb_test_zone18_with_time_X)
postResample(pred_zone18_with_time,xgb_test_zone18_with_time_y)

#get importance score from the optimal xgboost model and avoids the normalisation
#step by setting scale to FALSE 
xgb_importance_zone18 <- varImp(fit.xgb.zone18,scale=TRUE)
xgb_importance_zone18
plot(xgb_importance_zone18,10)

# backcasting and forcasting

#make prediction based on xgboost model
#1.read the data that needs to predict
load_pred_all_zones <- read.csv("../csv/testRaw.csv")

#make prediction
load_pred_all_zones$year<-as.factor(load_pred_all_zones$year)
load_pred_all_zones$month<-as.factor(load_pred_all_zones$month)
load_pred_all_zones$day<-as.factor(load_pred_all_zones$day)
load_pred_all_zones$hour<-as.factor(load_pred_all_zones$hour)
load_pred_all_zones$zone_id <- as.factor(load_pred_all_zones$zone_id)

#prepare for the prediction input
xgb_pred_final_X <- load_pred_all_zones[,!(colnames(load_pred_all_zones) %in% c("load"))]
xgb_pred_final_y <- load_pred_all_zones[,"load"]

fit.xgb <- train(load~., data=load_pred_all_zones, method = "xgbTree", trControl = control)

pred_final <-predict(fit.xgb, xgb_pred_final_X)

load_pred_all_zones$load <- pred_final
load_pred_all_zones_new <- load_pred_all_zones
load_pred_all_zones_new <- load_pred_all_zones_new[,1:6]

load_pred_all_zones_new <- spread(load_pred_all_zones_new, key = hour, value = load)

load_pred_all_zones_new$year <- as.numeric(load_pred_all_zones_new$year)
load_pred_all_zones_new$month <- as.numeric(load_pred_all_zones_new$month)
load_pred_all_zones_new$day <- as.numeric(load_pred_all_zones_new$day)
load_pred_all_zones_new_group <- load_pred_all_zones_new[order(load_pred_all_zones_new$day,load_pred_all_zones_new$month,load_pred_all_zones_new$year),]
View(load_pred_all_zones_new_group)
write.csv(load_pred_all_zones,"../csv/IRDMGroupPrediction.csv")

