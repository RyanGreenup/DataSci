
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
  
  pacman::p_load(ggmap, plotly, EnvStats, ggplot2, GGally, corrplot, dplyr, tidyr, stringr, reshape2, cowplot, ggpubr, reshape2, ggplot2, rmarkdown, dplyr, plotly, rstudioapi, wesanderson, RColorBrewer)
  
  #Use the Rstudio API to get the working directory
  
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
  
}

setwd.loadpac()


# Load Dataset ------------------------------------------------------------
all.df <- read.csv(file = "deathrate.csv", header = TRUE, sep = ",")


# Split the Data into Training, Validation and Test Data ------------------

n      <- nrow(all.df) #Number of Observations
idvals <- sample(n)    #Random shuffle of the numbers from 1:60
  
nTrain <- 0.5*n
nVal   <- 0.25*n
nTest  <- nrow(all.df) - nTrain - nVal

train.df <- all.df[idvals[1:nTrain],]
val.df   <- all.df[idvals[(nTrain+1):(nTrain+nVal)],]
test.df  <- all.df[idvals[(nTrain+nVal+1):n],]


# Standardise the Data ----------------------------------------------------

  ##subset out the features
  train.df.feat <- train.df[,-1]
  val.df.feat   <- val.df[,-1]
  test.df.feat  <- test.df[,-1]
  
  #Create a vector with mean and standard deviation values corresponding
    #to the feature column numbers
  TrainMeanCol.vec <- sapply(train.df.feat, mean)
  TrainSDCol.vec <- sapply(train.df.feat, sd)
  
  #Create data frames for the standard feature data
  trainSTD.df.feat <- train.df.feat
  valSTD.df.feat   <- val.df.feat
  testSTD.df.feat  <- test.df.feat
  
  for (i in 1:ncol(trainSTD.df.feat)){
   x.train   <- train.df.feat[,i] 
   x.val     <- val.df.feat[,i] 
   x.test    <- test.df.feat[,i] 
   
   mu  <- TrainMeanCol.vec[i]
   sig <- TrainSDCol.vec[i]
   
    trainSTD.df.feat[,i] <- (x.train-mu)/sig
    valSTD.df.feat[,i]   <- (x.val-mu)/sig
    testSTD.df.feat[,i]  <- (x.test-mu)/sig
  }
  
  #Combine to create Std DataFrames
  
  trainSTD.df <- cbind(I = train.df$I, trainSTD.df.feat) 
  valSTD.df   <- cbind(I = val.df$I, valSTD.df.feat) 
  testSTD.df  <- cbind(I = test.df$I, testSTD.df.feat) 
  
  allSTD.df <- rbind(trainSTD.df, valSTD.df, testSTD.df)
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
  
pred.ridge <-  pred.ridgeSTD*TrainSDCol.vec.vec["B"] + MeanCol.vec["B"]
pred.lasso<-  pred.lassoSTD*TrainSDCol.vec["B"] + MeanCol.vec["B"]
      #Base Plot
      plot(x = all.df$I, y = all.df$B, lty = 1, type = "b",
           ylab = "Predicted Death Rate",
           xlab = "Observation Number",
           main = "Observed and Predicted Death Rate")
      lines(x = all.df$I, y = pred.lasso, col = "red")
      
      





