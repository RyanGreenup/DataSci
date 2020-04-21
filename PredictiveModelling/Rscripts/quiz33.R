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


## practical 06a
## Load data
mydata <- read.csv("q3-1.csv")

## Create model
fit <- glm(y ~ x1 + x2, family = binomial(link = 'logit'), data = mydata)
fit$coefficients

## Decision boundary
intercept <- -fit$coefficients[1]/fit$coefficients[3]
slope <- -fit$coefficients[2]/fit$coefficients[3]

## Plot all
mycolours = c("red", "blue")
plot(mydata$x1, mydata$x2, col = mycolours[1+mydata$y], xlab = "x1", ylab = "x2")
abline(intercept, slope, lwd = 2)

## Create predictions for the training data
pre <- predict(fit, type = "response")
pre <- ifelse(pre < 0.5, 0, 1)

## Misclassification data
misclassified = (pre != mydata$y)
misclassificationError = mean(misclassified)
print(paste("Accuracy", 1-misclassificationError))

