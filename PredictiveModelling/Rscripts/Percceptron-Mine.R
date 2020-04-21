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


trainx <- data.frame(x0 = c(1,1,1,1), x1 = c(0,0,1,1), x2 = c(0,1,0,1))
trainy <- data.frame(y = c(0,0,0,1))

w <- data.frame(w0 = 0, w1 = 0, w2 = 0)
pred <- data.frame(y = 0, yhat = 0)
step <- 1

alpha <- 0.1
small <- 0.001

repeat {
  
  for (j in 1:nrow(trainx)) {
    perceptron <- sum(w[step,] * trainx[j,]) > 0
    pred[step,] <- c(trainy[j,1],  as.integer(perceptron) )
    for (i in 1:3) {
      w[step+1,i] <- w[step,i] + alpha * (pred$y[step] - pred$yhat[step]) * trainx[j,i]
    }
    step <- step + 1
  }
  myerror <- 0
  for (j in (step-nrow(trainx)):(step-1)) {
    myerror <- myerror + abs(pred$y[j] - pred$yhat[j]) / nrow(trainx)
    print(paste(j, myerror))
  }
  if (myerror < small) { break }
}


# Example 1 - Activation Function -----------------------------------------

g <- function(x1, x2){
  return (-30+20*x1+20*x2)
}
plot(g,-10,10)

act.df <- data.frame(z )