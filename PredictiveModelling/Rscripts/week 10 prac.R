setwd("K:/USB/NEWpredMOD/Week 10")

mydata <- read.csv("deathrate.csv")

head(mydata)

mysample <- sample(nrow(mydata))
nTrain <- 0.7*nrow(mydata)
nVal <- 0.15*nrow(mydata)
nTest <- nrow(mydata) - nTrain - nVal

trainSet <- mydata[mysample[1:nTrain],2:17]
valSet <- mydata[mysample[(nTrain+1):(nTrain+nVal)],2,17]
testSet <- mydata[mysample[(nTrain+nVal+1):nrow(mydata)],2:17]
stddata <- subset(mydata, select = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))
mystd <- stddata[0,]
mystd[1,] <- sapply(stddata,mean)
mystd[2,] <- sapply(stddata,sd)
for (i in 2:ncol(stddata)){
  stddata[,i] <- (stddata[,i]-mystd[1,i])/mystd[2,i]
}
write.csv(stddata,file = "deathrate_train.csv", row.names = FALSE)
traindata <- read.csv("deathrate_train.csv") ## think i changed file name from deathrate.csv might be wrong now


fit01 <- lm(B~A1, data = trainSet)
fit02 <- lm(B~A2, data = trainSet)
fit03 <- lm(B~A3, data = trainSet)
fit04 <- lm(B~A4, data = trainSet)
fit05 <- lm(B~A5, data = trainSet)
fit06 <- lm(B~A6, data = trainSet)
fit07 <- lm(B~A7, data = trainSet)
fit08 <- lm(B~A8, data = trainSet)
fit09 <- lm(B~A9, data = trainSet)
fit10 <- lm(B~A10, data = trainSet)
fit11 <- lm(B~A11, data = trainSet)
fit12 <- lm(B~A12, data = trainSet)
fit13 <- lm(B~A13, data = trainSet)
fit14 <- lm(B~A14, data = trainSet)
fit15 <- lm(B~A15, data = trainSet)

rss <- c(sum(residuals(fit01)^2), sum(residuals(fit02)^2), sum(residuals(fit03)^2),  sum(residuals(fit04)^2), sum(residuals(fit05)^2), sum(residuals(fit06)^2), sum(residuals(fit07)^2), sum(residuals(fit08)^2), sum(residuals(fit09)^2), sum(residuals(fit10)^2), sum(residuals(fit11)^2), sum(residuals(fit12)^2), sum(residuals(fit13)^2), sum(residuals(fit14)^2), sum(residuals(fit15)^2))
which.min(rss)

head(traindata)
head(mydata)

install.packages("glmnet")
library(glmnet)


##mydata <- read.csv("trainSet.csv") ?? not sure about this line
mydata <- trainSet
mytest <- testSet
head(mydata)
x <- as.matrix(subset(mydata, select = 2:8))
y <- as.matrix(subset(mydata, select = 1))
fit <- glmnet(x,y,alpha=0,lambda = 1)

xtest <- as.matrix(mytest)
##pre1 <- predict(fit,newx = x,) ## incomplete from here


