
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
  
  pacman::p_load(ggmap, plotly, EnvStats, ggplot2, glmnet, GGally, corrplot, dplyr, tidyr, stringr, reshape2, cowplot, ggpubr, reshape2, ggplot2, rmarkdown, dplyr, plotly, rstudioapi, wesanderson, RColorBrewer)
  
  #Use the Rstudio API to get the working directory
  
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
  
}

setwd.loadpac()


# Load Dataset ------------------------------------------------------------
all.df <- read.csv(file = "deathrate.csv", header = TRUE, sep = ",")


# Standardise the Data ----------------------------------------------------

  ##subset out the features
  all.df.feat <- all.df[,-1]
  
  #Create a vector with mean and standard deviation values corresponding
    #to the feature column numbers
  MeanCol.vec <- sapply(all.df.feat, mean)
  SDCol.vec <- sapply(all.df.feat, sd)
  
  #Create data frames for the standard feature data
  allSTD.df.feat  <- all.df.feat
  
  for (i in 1:ncol(allSTD.df.feat)){
   x   <- all.df.feat[,i] 
   
   mu  <- MeanCol.vec[i]
   sig <- SDCol.vec[i]
   
    allSTD.df.feat[,i] <- (x-mu)/sig
  }
  
  #Combine to create Std DataFrames
  
  allSTD.df <- cbind(I = all.df$I, allSTD.df.feat) 
  allSTD.df <- arrange(.data = allSTD.df, by.group = I) 
  
  ##Write The Standardised data to a CSV
   write.csv(x = allSTD.df, file = "deathrateSTD.csv", row.names = FALSE)
   

# Train a Ridge Regression and Lasso Regression Model using Cross  --------
   
##Create the feature and output Matrix
    #Theres only 30 values in the training data, so training a model from that
      #data is probably not a good idea, instead we will use cross validation
      # on all the data
x <- as.matrix(allSTD.df[,!(names(all.df) %in% c("I", "B"))])
y <- as.matrix(allSTD.df[,(names(all.df) %in% c("B"))]) 

##Use Cross Validation to create a model
  ridge.mod <- cv.glmnet(x, y, family = "gaussian", alpha = 0)
  lasso.mod <- cv.glmnet(x, y, family = "gaussian", alpha = 1)
  
  lambda.min.ridge <- ridge.mod$lambda.min
  lambda.min.lasso <- lasso.mod$lambda.min
  
  
  pred.ridgeSTD <- predict(ridge.mod, newx = x, s = lambda.min.ridge)
  pred.lassoSTD <- predict(lasso.mod, newx = x, s = lambda.min.lasso)
  
pred.ridge <-  pred.ridgeSTD*SDCol.vec["B"] + MeanCol.vec["B"]
pred.lasso<-  pred.lassoSTD*SDCol.vec["B"] + MeanCol.vec["B"]
      #Base Plot
      plot(x = all.df$I, y = all.df$B, lty = 1, type = "b",
           ylab = "Predicted Death Rate",
           xlab = "Observation Number",
           main = "Observed and Predicted Death Rate")
      lines(x = all.df$I, y = pred.lasso, col = "red")
      

# Predictors for the best Lasso Model -------------------------------------
  #The Best lasso model is the model returned by the cross validation,
      #it is stored as lasso.mod
      
      ##The coefficients can be returned with
      lasso.mod.coef <- coefficients(lasso.mod)
      lasso.mod.coef
      
      LassoPredictors <- lasso.mod.coef[lasso.mod.coef[,1] > 0,]
      LassoPredictors <- sort(LassoPredictors)

      paste("The Predictors for the Best Lasso Model, in order of significance, are:",
            paste("",names(LassoPredictors[-1]),sep="", collapse=", "))     
      