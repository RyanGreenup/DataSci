## Practical 04a_______________________________________________________________________________________________________________


## Set working directory and load/view data set 
set_wd <- function() {
  
  #Install the RStudio API package
  
  if(require('rstudioapi')){
    library('rstudioapi')
  }else{
    install.packages('rstudioapi')
    library('rstudioapi')
  }
  
  #Use the Rstudio API to get the working directory
  
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
}

set_wd()
prac4a <- read.csv("practical04a.csv")

View(prac4a)

## Assign variables and apportion % for training/validation/testing
## Create random permutation of numbers 1:N
No <- nrow(prac4a)
NoTrain <- round(No * 0.8)
Noval <- round(No*0.1)
NoTest <- No-NoTrain - Noval
index <- sample(No)

prac4aTrain <- prac4a[index[1:NoTrain],]
prac4aVal <- prac4a[index[(NoTrain+1):(NoTrain+Noval)],]
prac4aTest <- prac4a[index[(NoTrain+Noval):No],]

## Plot the data sets
plot(prac4aTrain$input,prac4aTrain$output, pch=14, col = "blue", xlab = "Input", ylab = "Output" )
points(prac4aVal$input, prac4aVal$output, pch=15, col = "purple")
points(prac4aTest$input, prac4aTest$output, pch=16, col = "red")

## Create linear models
fit1 <- lm(output~input, data = prac4aTrain)
fit2 <- lm(output~input+I(input^2), data = prac4aTrain)
fit3 <- lm(output~input+I(input^2) + I(input^3), data = prac4aTrain)
fit4 <- lm(output~input+I(input^2) + I(input^3) + I(input^4), data = prac4aTrain)
fit5 <- lm(output~input+I(input^2) + I(input^3) + I(input^4) + I(input^5), data = prac4aTrain)
fit6 <- lm(output~input+I(input^2) + I(input^3) + I(input^4) + I(input^5) + I(input^6), data = prac4aTrain)
fit7 <- lm(output~input+I(input^2) + I(input^3) + I(input^4) + I(input^5) + I(input^6) + I(input^7), data = prac4aTrain)

## Compute the training errors
RMSE1train = sqrt(sum(residuals(fit1)^2)/NoTrain)
RMSE2train = sqrt(sum(residuals(fit2)^2)/NoTrain)
RMSE3train = sqrt(sum(residuals(fit3)^2)/NoTrain)
RMSE4train = sqrt(sum(residuals(fit4)^2)/NoTrain)
RMSE5train = sqrt(sum(residuals(fit5)^2)/NoTrain)
RMSE6train = sqrt(sum(residuals(fit6)^2)/NoTrain)
RMSE7train = sqrt(sum(residuals(fit7)^2)/NoTrain)

## Collect the training errors
RMSEtrain=c(RMSE1train,RMSE2train,RMSE3train,RMSE4train, RMSE5train, RMSE6train,RMSE7train)

## Predict and compute the validation errors
pre1 = predict(fit1,data.frame(input = prac4aVal$input))
pre2 = predict(fit2,data.frame(input = prac4aVal$input))
pre3 = predict(fit3,data.frame(input = prac4aVal$input))
pre4 = predict(fit4,data.frame(input = prac4aVal$input))
pre5 = predict(fit5,data.frame(input = prac4aVal$input))
pre6 = predict(fit6,data.frame(input = prac4aVal$input))
pre7 = predict(fit7,data.frame(input = prac4aVal$input))


RMSE1val = sqrt(sum((pre1-prac4aVal$output)^2)/Noval)
RMSE2val = sqrt(sum((pre2-prac4aVal$output)^2)/Noval)
RMSE3val = sqrt(sum((pre3-prac4aVal$output)^2)/Noval)
RMSE4val = sqrt(sum((pre4-prac4aVal$output)^2)/Noval)
RMSE5val = sqrt(sum((pre5-prac4aVal$output)^2)/Noval)
RMSE6val = sqrt(sum((pre6-prac4aVal$output)^2)/Noval)
RMSE7val = sqrt(sum((pre7-prac4aVal$output)^2)/Noval)

##Collect the validation errors
RMSEval = c(RMSE1val,RMSE2val,RMSE3val,RMSE4val,RMSE5val,RMSE6val,RMSE7val)

## Plot training versus validation errors
plot(1:7, RMSEtrain, pch=15, col="red", type="b", xlab="Model Complexity", ylab="RMSE")

points(1:7, RMSEval, pch=16, col="blue", type="b")






##Practical 04b________________________________________________________________________________________________________________

prac4b <- read.csv("practical04b.csv")

View(prac4b)

## Assign variables and apportion % for training/validation/testing
## Create random permutation of numbers 1:N
No <- nrow(prac4b)
NoTrain <- round(No * 0.8)
Noval <- round(No*0.1)
NoTest <- No-NoTrain - Noval
index <- sample(No)

prac4bTrain <- prac4b[index[1:NoTrain],]
prac4bVal <- prac4b[index[(NoTrain+1):(NoTrain+Noval)],]
prac4bTest <- prac4b[index[(NoTrain+Noval):No],]

## Plot the data sets
plot(prac4bTrain$input,prac4bTrain$output, pch=14, col = "blue", xlab = "Input", ylab = "Output" )
points(prac4bVal$input, prac4bVal$output, pch=15, col = "purple")
points(prac4bTest$input, prac4bTest$output, pch=16, col = "red")

## Create linear models
fit1 <- lm(output~input, data = prac4bTrain)
fit2 <- lm(output~input+I(input^2), data = prac4bTrain)
fit3 <- lm(output~input+I(input^2) + I(input^3), data = prac4bTrain)
fit4 <- lm(output~input+I(input^2) + I(input^3) + I(input^4), data = prac4bTrain)
fit5 <- lm(output~input+I(input^2) + I(input^3) + I(input^4) + I(input^5), data = prac4bTrain)
fit6 <- lm(output~input+I(input^2) + I(input^3) + I(input^4) + I(input^5) + I(input^6), data = prac4bTrain)
fit7 <- lm(output~input+I(input^2) + I(input^3) + I(input^4) + I(input^5) + I(input^6) + I(input^7), data = prac4bTrain)

## Compute the training errors
RMSE1train = sqrt(sum(residuals(fit1)^2)/NoTrain)
RMSE2train = sqrt(sum(residuals(fit2)^2)/NoTrain)
RMSE3train = sqrt(sum(residuals(fit3)^2)/NoTrain)
RMSE4train = sqrt(sum(residuals(fit4)^2)/NoTrain)
RMSE5train = sqrt(sum(residuals(fit5)^2)/NoTrain)
RMSE6train = sqrt(sum(residuals(fit6)^2)/NoTrain)
RMSE7train = sqrt(sum(residuals(fit7)^2)/NoTrain)

## Collect the training errors
RMSEtrain=c(RMSE1train, RMSE2train, RMSE3train, RMSE4train, RMSE5train, RMSE6train, RMSE7train)

## Predict and compute the validation errors
pre1 = predict(fit1,data.frame(input = prac4bVal$input))
pre2 = predict(fit2,data.frame(input = prac4bVal$input))
pre3 = predict(fit3,data.frame(input = prac4bVal$input))
pre4 = predict(fit4,data.frame(input = prac4bVal$input))
pre5 = predict(fit5,data.frame(input = prac4bVal$input))
pre6 = predict(fit6,data.frame(input = prac4bVal$input))
pre7 = predict(fit7,data.frame(input = prac4bVal$input))


RMSE1val = sqrt(sum((pre1-prac4bVal$output)^2)/Noval)
RMSE2val = sqrt(sum((pre2-prac4bVal$output)^2)/Noval)
RMSE3val = sqrt(sum((pre3-prac4bVal$output)^2)/Noval)
RMSE4val = sqrt(sum((pre4-prac4bVal$output)^2)/Noval)
RMSE5val = sqrt(sum((pre5-prac4bVal$output)^2)/Noval)
RMSE6val = sqrt(sum((pre6-prac4bVal$output)^2)/Noval)
RMSE7val = sqrt(sum((pre7-prac4bVal$output)^2)/Noval)

##Collect the validation errors
RMSEval = c(RMSE1val,RMSE2val,RMSE3val,RMSE4val,RMSE5val,RMSE6val,RMSE7val)

## Plot training versus validation errors
plot(1:7, RMSEtrain, pch=15, col="red", type="b", xlab="Model Complexity", ylab="RMSE")

points(1:7, RMSEval, pch=16, col="blue", type="b")

