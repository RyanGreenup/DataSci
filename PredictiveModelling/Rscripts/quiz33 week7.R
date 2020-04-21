##Set working directory
# Set the Working Directory -----------------------------------------------

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




## Load data
mydata <- read.csv("q3-1.csv")



##plot all
#mycolours = c("red", "blue")
#plot(mydata$Pclass,mydata$Age,col=mycolours[1 + mydata$Survived], xlab = "Cabin Class", ylab = "Age")

##Clean up data
#mydata$x1[is.na(mydata$x1)]<-median(mydata$x1,na.rm=TRUE)
#mydata$x2[is.na(mydata$x2)]<-median(mydata$x2,na.rm=TRUE)

##Build models

fit <- glm(y ~ x1 + x2, family = binomial(link = 'logit'), data = mydata)
fit1 <- glm(y ~ I(x1^2) + I(x2^2), family = binomial(link='logit'),data=mydata)
fit2 <- glm(y ~ x1 + x2 + I(x1^2) + I(x2^2), family = binomial(link='logit'),data=mydata)
fit3 <- glm(y ~ I(x1^3) + I(x2^3), family = binomial(link = 'logit'), data = mydata)
##Select best model
aics <- c(summary(fit)$aic, summary(fit2)$aic, summary(fit2)$aic, summary(fit3)$aic)

bestfit <- switch(EXPR = which.min(aics), fit,fit1,fit2,fit3)
print("Best fit: ")
print(bestfit$call)

##Create prediction using best model
pre <- predict(bestfit, type = "response")

##Function to compute sensitivity for arbitrary threshold
sensitivity <- function(prob, ground_truth, threshold){
  preX <- ifelse(prob < threshold,0,1)
  
  ##sensitivity -> (#true positives)/(#condition positive)
  num_true_pos <- sum((preX == 1)*(ground_truth == 1)) ## true data believed to be true
  num_cond_pos <- sum(ground_truth == 1)               ## Actual negative data
  return(num_true_pos/num_cond_pos)
}

##Function to compute specificity for arbitrary threshold
specificity <- function(prob, ground_truth, threshold){
  preX <- ifelse(prob < threshold,0,1)
  
  ##specificity -> (#true negatives)/(#condition negative)
  num_true_neg <- sum((preX == 0)*(ground_truth == 0)) ## true data believed to be true
  num_cond_neg <- sum(ground_truth == 0)               ## Actual negative data
  return(num_true_neg/num_cond_neg)
}

##Compute ROC values
steps <- 101
thresholds <- seq(0,1, length.out = steps)
roc <- data.frame(thresh = 0, fpr = 0, tpr = 0, sum = 0)
for(i in 1:steps){
  roc[i,"thresh"] <- thresholds[i]
  roc[i, "fpr"] <- 1 - specificity(pre, mydata$Survived, thresholds[i])
  roc[i, "tpr"] <- sensitivity(pre, mydata$y, thresholds[i])
  roc[i, "sum"] <- specificity(pre, mydata$y, thresholds[i]) + sensitivity(pre, mydata$y, thresholds[i])
}

##Plot ROC Curve
plot(roc$fpr, roc$tpr, type='l', xlab = "1 - Specificity", ylab = "Sensitivity", main = "ROC Curve")

#Find best value
i <- which.max(roc$sum)
points(roc$fpr[i], roc$tpr[i], pch=8)
text(roc$fpr[i], roc$tpr[i], paste("threshold = ", signif(roc$thresh[i],2)), pos = 1)


