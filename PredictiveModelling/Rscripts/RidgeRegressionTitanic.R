
# Preamble ----------------------------------------------------------------
  ##Load Packages
if(require('pacman')){
  library('pacman')
}else{
  install.packages('pacman')
  library('pacman')
}

pacman::p_load(xts, sp, gstat, ggplot2, rmarkdown, reshape2, ggmap,
               parallel, dplyr, plotly, glmnet)


  ##Set Working Directory
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

  ##Load Dataset
trainStd.df <- read.csv("titanictrainSTD.csv", TRUE, ",")
testStd.df <- read.csv("titanictestSTD.csv", TRUE, ",")



# Fit a GLM Model ---------------------------------------------------------
 ##For this glmnet will need to be loaded as a package
  library(glmnet)
  
  ##Create Matrix Subsets
  x.train <- trainStd.df[,!names(trainStd.df) %in% c("Survived", "Name", "Ticket", "Cabin")]
  x.train <- as.matrix(x.train)
  y.train <- trainStd.df[,names(trainStd.df) %in% c("Survived")]
  y.train <- as.matrix(y.train)
  
  x.test <- testStd.df[,!names(testStd.df) %in% c("Survived", "Name", "Ticket", "Cabin")]
  x.test <- as.matrix(x.test)
  
 ##Fit the Model 
  mod <- glmnet(x = x.train, y = y.train, family = "binomial", alpha = 0, lambda = 1)
  mod.mult <- glmnet(x = x.train, y = y.train, family = "binomial", alpha = 0)
  
  
  ##Apply Prediction to Test Set
  prob <- predict(object = mod, newx = x.test, type = 'response')
  prob.mult <- predict(object = mod.mult, newx = x.test, type = 'response')
  
    #So maybe we would use a validation set to decide which one
      #of the multiple models to use?
  
  ##Compute correct predictions on Training Set
    prob.train <- predict(object = mod, newx = x.train, type = 'response')
    prob.train.mult <- predict(object = mod.mult, newx = x.train, type = 'response')
      
      ###Decide on a probability trheshold
        thresh <- 0.5
            #so we could use a ROC curve to decide on a better performing
              #probability threshold
        ###Create Predictions
        pred.train <- ifelse(test = prob.train<thresh,yes = 0, no = 1 )
        
        no.error <- sum(y.train != pred.train)
        rate.error <- mean(y.train != pred.train)
        i
          #So with a validation set and a few loops I think the idea is we would favour
            #a model with predictions that perform the best.
        
        paste("This model misclassified", no.error, "/", nrow(pred.train), "which is an error rate of", round(rate.error*100), "%")
