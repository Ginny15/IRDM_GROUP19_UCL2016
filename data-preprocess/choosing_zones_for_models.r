library(Metrics)
library(forecast)
require(dplyr)
library(tidyr)

library(caret)
source("data_preprocessing.r")

#picking out the pieces with full data
whole <- whole[which(!is.na(whole$load)),]
for (i in 1:17){
  whole[[i]] <- as.numeric(whole[[i]])
}

# categorizing the time attributes
for (i in 1:4){
  whole[[i]] <- as.factor(whole[[i]])
}

# predicting with a simple MLR model to pick out the four most representative zones
for (j in 1:20) {
  zone.j <- whole[which(whole$zone_id==j),]
  zone.j$zone_id<-NULL
  control = trainControl(method="cv", number=5, repeats=1)
  fit.station1 = train(load~., data=zone.j, method = "lm", trControl = control)
  print(paste0("Linear regression for zone: ", j))
  print(fit.station1[4]$results[2])
}
