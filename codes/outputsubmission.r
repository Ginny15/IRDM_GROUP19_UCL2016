test <- read.csv("../csv/IRDMGroupPrediction.csv")
for (i in 1:11){
  test[[8]]<-NULL  
}
test$date <- paste(test$zone_id,test$year,test$month,test$day,sep = "-")
for (i in 1:5){
  test[[1]]<-NULL  
}
test <- spread(test, key = hour, value = load)
colnames(test)[2:25] <- lapply(colnames(test)[2:25],function(x){
  paste("h",x,sep = "")
})
test$zone_id <- sapply(test$date,function(x) as.numeric(strsplit(x,"-")[[1]][1]))
test$year <- sapply(test$date,function(x) as.numeric(strsplit(x,"-")[[1]][2]))
test$month <- sapply(test$date,function(x) as.numeric(strsplit(x,"-")[[1]][3]))
test$day <- sapply(test$date,function(x) as.numeric(strsplit(x,"-")[[1]][4]))
test$id <- 1:nrow(test)
submission <- test[,c(30,26:29,2:25)]
submission <- submission[order(submission$year,submission$month,submission$day,submission$zone_id),]
submission <- submission[which(!is.na(submission$h1)),]
row.sum <- 
submission.sum <- rbind(submission[1:20,],)
submission.sum  <- submission[0,]
nlist <-nrow(submission)/20
for (i in 1:63){
  submission.seg <- submission[((i-1)*20+1):(i*20),]
  row.sum <- submission[1,]
  row.sum$zone_id <- 21
  row.sum[,3:5] <- submission[(i*20),3:5]
  row.sum[,6:29] <- lapply(submission.seg[,6:29],sum)
  #submission.seg <- rbind(submission.seg,row.sum)
  submission.sum <- rbind(submission.sum,submission.seg,row.sum)
}
submission.sum$id <- 1:nrow(submission.sum)
write.csv(submission.sum,"../output/submission.csv",row.names=FALSE)
