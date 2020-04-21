setwd("K:/USB/NEWpredMOD/Week 11")

library(FNN)
## Load dataset and standardise columns
mydata <- read.csv("CommViolPredUnnormalizedData.csv")

for (i in 3:ncol(mydata)) {
  mydata[is.na(mydata[,i]), i] <- median(mydata[,i], na.rm = TRUE)
}
myx <- scale(subset(mydata, select = A1:A124))
myy <- as.matrix(subset(mydata, select = B2))

## Use the first 2000 rows in the data set, and kNN regression (with k = 3),
## to predict column B2 for the last 215 rows of the data set. Compute the
## mean square error. (FNN package) 

mytrainx <- myx[1:2000, ]
mytrainy <- myy[1:2000, ]
mytestx <- myx[2001:2215,]
mytesty <- myy[2001:2215,]
fit1 <- knn.reg(mytrainx, mytestx, mytrainy, k = 3)
pre1 <- fit1$pred
MSE1 <- sqrt(sum((pre1 - mytesty)^2)/215)

## Build a model using lasso regression, with the first 2000 rows as
## training data. Predict the column B2 for the last 215 rows, and compute
## the mean square error

library(glmnet)
lasso <- cv.glmnet(mytrainx, mytrainy, alpha = 1.0)
pre2 <- predict(lasso, newx = mytestx, s = "lambda.min")
MSE2 <- sqrt(sum((pre2 - mytesty)^2)/215)

MSE2


## Split the standardised CommViolPredUnnormalizedData.csv data into a
## training, validation and test set. 

library(FNN)
mydata <- read.csv("CommViolPredUnnormalizedData.csv")

for (i in 3:ncol(mydata)) {
  mydata[is.na(mydata[,i]), i] <- median(mydata[,i], na.rm = TRUE)
}
idx <- sample(1:nrow(mydata))
mydata_mixedup <- mydata[idx,]
myx <- scale(subset(mydata_mixedup, select = A1:A124))
myy <- as.matrix(subset(mydata_mixedup, select = B2))
N <- nrow(myx)
Ntrain <- round(0.7 * N)
Nval <- round(0.15 * N)
Ntest <- N - Ntrain - Nval
mytrainx <- myx[1:Ntrain, ]
mytrainy <- myy[1:Ntrain, ]
myvalx <- myx[(Ntrain+1):(Ntrain+Nval), ]
myvaly <- myy[(Ntrain+1):(Ntrain+Nval), ]
mytestx <- myx[(Ntrain+Nval+1):N, ]
mytesty <- myy[(Ntrain+Nval+1):N, ]

## Use the validation set to create plots for different k (e.g., k = 1, 3, 5, 7, 11):

fit1 <- knn.reg(mytrainx, myvalx, mytrainy, k = 1)
pre1 <- fit1$pred
MSE1 <- sqrt(sum((pre1 - myvaly)^2)/Nval)

fit3 <- knn.reg(mytrainx, myvalx, mytrainy, k = 3)
pre3 <- fit3$pred
MSE3 <- sqrt(sum((pre3 - myvaly)^2)/Nval)

fit5 <- knn.reg(mytrainx, myvalx, mytrainy, k = 5)
pre5 <- fit5$pred
MSE5 <- sqrt(sum((pre5 - myvaly)^2)/Nval)

fit7 <- knn.reg(mytrainx, myvalx, mytrainy, k = 7)
pre7 <- fit7$pred
MSE7 <- sqrt(sum((pre7 - myvaly)^2)/Nval)

fit9 <- knn.reg(mytrainx, myvalx, mytrainy, k = 9)
pre9 <- fit9$pred
MSE9 <- sqrt(sum((pre9 - myvaly)^2)/Nval)

fit11 <- knn.reg(mytrainx, myvalx, mytrainy, k = 11)
pre11 <- fit11$pred
MSE11 <- sqrt(sum((pre11 - myvaly)^2)/Nval)

MSEs <- c(MSE1,Inf,MSE3,Inf,MSE5,Inf,MSE7,Inf,MSE9,Inf,MSE11)

## Select the best k

bestk <- which.min(MSEs)

print(paste("Best k:", bestk))

## For the k you selected, compute the error on the test set.

fit <- knn.reg(mytrainx, mytestx, mytrainy, k = bestk)
pre <- fit$pred
MSE <- sqrt(sum((pre - mytesty)^2)/Ntest)

## for each point in the validation set, plot the predicted value and the
## actual value (index on the x-axis, value on the y). Use different colours
## for predicted and actual values. 

print(paste("Test error for best k: ", MSE))

plot(1:Nval, myvaly, pch = 16, col = "black", xlab =
       "Index", ylab = "MSE")
points(1:Nval, pre1, pch = 1, col = "red")














