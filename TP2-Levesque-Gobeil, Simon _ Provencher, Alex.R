require(ROCR)
require(rpart)

setwd("D:/Work/tpr")
GermanData  <- read.csv("GermanData.csv",header=F)
CarData     <- read.csv("CarData.csv",header=F)

### GermanData:
GermanData_training_idx <-sample(1:nrow(GermanData), nrow(GermanData) * 0.2, replace=F)
GermanData_test_idx <- setdiff(1:nrow(GermanData), GermanData_training_idx)

GermanDataTraining <- GermanData[GermanData_training_idx,]
GermanDatatest <- GermanData[GermanData_test_idx,]

ra <- rpart(V21 ~ .,data= GermanDataTraining, method = "class")
printcp(ra) #erreur sur lensemble d'antrainement
plotcp(ra)
summary(ra)

plot(ra, uniform=T, main="GermanData classification tree")
text(ra, use.n=T, pretty=T, all=T, cex=.8)

cp1 <- ra$cptable[which.min(ra$cptable[,"xerror"]),"CP"]
PruneRa <- prune(ra, cp1)
plot(PruneRa, uniform=T, main="GermanData pruned classification tree")
text(PruneRa, use.n=T, pretty=T, all=T, cex=.8)

gerPredict <- predict(PruneRa, GermanDatatest, type = "prob")
gerPred <- prediction(gerPredict[,2], GermanDatatest$V21)
gerPerf <- performance(gerPred, "tpr", "fpr")
plot(gerPerf, main=paste("GermanData courbe ROC (cp= ", cp1,")"))

### CarData:
names(CarData) <- c("buying","maint","doors","persons","lug_boot","safety","class")

CarData["predic"] <- NA
for (i in 1:nrow(CarData)){
  if(CarData$class[i] == "unacc") {
    CarData$predic[i] <-  0
  } else{
    CarData$predic[i] <- 1
  } 
}

#tree
#pred <- prediction(c(0.1,.5,.3,.8,.9,.4,.9,.5), c(0,0,0,1,1,1,1,1))
#perf <- performance(pred, "tpr", "fpr")
#plot(perf)

CarData_training_idx <-sample(1:nrow(CarData), nrow(CarData) * 0.2, replace=F)
CarData_test_idx <- setdiff(1:nrow(CarData), CarData_training_idx)

CarDataTraining <- CarData[CarData_training_idx,]
CarDataTest <- CarData[CarData_test_idx,]

rc <- rpart(predic ~ buying + maint + doors + persons + lug_boot + safety, data = CarDataTraining, method = "class")
printcp(rc) #erreur sur lensemble d'antrainement
plotcp(rc)
summary(rc)

plot(rc, uniform=T, main="Classification tree") #branch=.5
text(rc, use.n=T, pretty=T, all=T, cex=.8)

#cp initial value
cp2 <- rc$cptable[which.min(rc$cptable[,"xerror"]),"CP"]
#cp2 <- 0.01
PruneRc <- prune(rc, cp2)
plot(PruneRc, uniform=T, main="Pruned classification tree")
text(PruneRc, use.n=T, pretty=T, all=T, cex=.8)


#ROC
carPredict <- predict(PruneRc, CarDataTest, type = "prob")
pred <- prediction(carPredict[,2], CarDataTest$predic)
perf <- performance(pred, "tpr", "fpr")
plot(perf, main=paste("Courbe ROC (cp= ", cp2,")"))