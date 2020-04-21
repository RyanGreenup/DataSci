# Preamble ----------------------------------------------------------------
##Clear latent variables
rm(list = ls())

##Set working directory and load packages

setwd.loadpac <- function() {
  
  if(require('pacman')){
    library('pacman')
  }else{
    install.packages('pacman')
    library('pacman')
  }
  
  pacman::p_load(glmnet, ggmap, plotly, EnvStats, FNN, ggplot2, GGally, corrplot, dplyr, tidyr, stringr, reshape2, cowplot, ggpubr, reshape2, ggplot2, rmarkdown, dplyr, plotly, rstudioapi, wesanderson, RColorBrewer)
  
  #Use the Rstudio API to get the working directory
  
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
  
}

setwd.loadpac()


trainx <- data.frame(x0=c(1,1,1,1),x1=c(0,0,1,1),x2=c(0,1,0,1))
trainy <- data.frame(y=c(0,0,0,1))

w <- data.frame(w0=0,w1=0,w2=0)
pred <- data.frame(y=0,yhat=0)
step <- 1

alpha <- 0.1
small <- 0.001

repeat {
  for (j in 1:nrow(trainx)) {
  perceptron <- sum(w[step,]*trainx[j,])>0
  pred[step,] <- c(trainy[j,1], as.integer(perceptron))
  for(i in 1:3) {
    w[step+1,i] <- w[step,i]+alpha*(pred$y[step]-pred$yhat[step])*trainx[j,i]
  }
  step <- step+1  
}
myerror <- 0
for(j in (step-nrow(trainx)):(step-1)) {
  myerror <- myerror + abs(pred$y[j]-pred$yhat[j]) / nrow(trainx)
}
if (myerror < small) {break}
}


## pred$y
## pred$yhat


  
## Code from last year
trainx <- data.frame(x1=c(0,0,1,1),x2=c(0,1,0,1))
trainy <- data.frame(y=c(0,0,0,1))

w <- data.frame(w0=-30,w1=20,w2=20)
pred <- data.frame(y=0,yhat=0)
step <- 1

alpha <- 0.1
small <- 0.001

repeat {
  for (j in 1:nrow(trainx)) {
    perceptron <- sum(w[step,]*trainx[j,])>0
    pred[step,] <- c(trainy[j,1], as.integer(perceptron))
    for(i in 1:3) {
      w[step+1,i] <- w[step,i]+alpha*(pred$y[step]-pred$yhat[step])*trainx[j,i]
    }
    step <- step+1  
  }
  myerror <- 0
  for(j in (step-nrow(trainx)):(step-1)) {
    myerror <- myerror + abs(pred$y[j]-pred$yhat[j]) / nrow(trainx)
  }
  if (myerror < small) {break}
}

## 
##       

## Plot activation function g(z) between -10 and 10 
g <- function(x) {
  return (1/(1+exp(-x)))
}
plot(g,-10,10)


## Implement f as a function in R, with the weights stored in a
## matrix W, and the inputs in a matrix x.

## f below had just (x,w) and (x%%w) but i used w above so i changed it
f <- function(myx,myw) {
  return(g(myx %*% myw))
}

myx = matrix(nrow = 4, ncol =2)
myw = matrix(nrow =3, ncol = 1)




n = (c(-30,20,20))
df=data.frame(n)

myw <- df
myw

myx <- trainx
myx

## fx 

fx <- (myw[1,]+(myw[2,]*myx[1,1])+(myw[3,]*myx[1,2]))



