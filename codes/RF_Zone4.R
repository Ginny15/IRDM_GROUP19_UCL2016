## Read the data file (Cleansing and Munging have been finished)
source("data_preprocessing.r")

whole$load <- as.integer(whole$load)


## omit rows with NA values
whole <- data.frame(na.omit (whole))

## Change some numerical variables to categorical variables
whole$zone_id<- as.factor(whole$zone_id)
whole$year <- as.factor(whole$year)
whole$month <- as.factor(whole$month)
whole$day <- as.factor(whole$day)
whole$hour <- as.factor(whole$hour)
W
data.class(whole$year)


## Extracting 'zone == 4' // Extract a subset
zone4 <- subset(whole, select = c(year, month, day, hour, load, station_1, station_2, station_3, station_4,
                                    station_5, station_6, station_7, station_8, station_9,
                                    station_10, station_11), subset=(zone_id == 4))


## split into a training set (80%) and test set (20%)
idx <- runif(nrow(zone4)) <= .80
zone4.train <- zone4[idx,]
zone4.test <- zone4[(!idx),]
head(zone4.train)


## Implementation of Random Forest
zone4.rf <- randomForest(load ~ .-load, data=zone4.train, mtry=5, 
                           ntree= 300, importance=TRUE, na.action=na.omit)
zone4.rf
## Plot the results of Random Foreset
plot(zone4.rf)


## Show "importance scores" of predictor variables: higher value mean more important:
imp <- importance(zone4.rf, type = 1)
round(imp, 3)

## Plot a figure showing a ranked variables based on Importance scores
varImpPlot(zone4.rf)


#Prediction
dim(zone4.test)
pred.rf <- data.frame(predict(zone4.rf, newdata=zone4.test))
head(pred.rf)


# Compute test set RMSE of the RF model
rmse(zone4.test$load, pred.rf)

