
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


# Set the Seed ------------------------------------------------------------
  #everything changes far too much without setting the seed
set.seed(seed = 31415)


# Load Dataset ------------------------------------------------------------
all.df <- read.csv(file = "CommViolPredUnnormalizedData.csv", header = TRUE, sep = ",")


# Clean Data --------------------------------------------------------------
  ##This can also be done with sapply
  #    as.data.frame(sapply(all.df, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x)))

  ##Using a for loop
  for (i in 1:ncol(all.df)) {
    if(sum(is.numeric(all.df[,i]))){
  
      all.df[is.na(all.df[,i]), i] <- median(all.df[,i], na.rm = TRUE)     
      
        if(sum(is.na(all.df[,i]))){
          print(paste("Could be an error on column no. ", "i"))
        }
      
    }else(
      
      print(paste("skip row no.", i))
      
    )
    
  }


# Standardise the Data ----------------------------------------------------
  #We only have to standardise the data for the sake of the 
    #Lasoo Regression

##Subset the Data
allStd.df <- subset(x = all.df, select = A1:A124)

##Standardise the data
allStd.df <- scale(allStd.df) #This coerces the data into a matrix

# Create Matrix Input/Output Assignments ----------------------------------
x.all <- as.matrix(allStd.df)
y.all <- subset(x = all.df, select = B2)
y.all <- as.matrix(y.all)

# Create a Training and Test Split ----------------------------------------
n      <- nrow(all.df) #Number of Observations
idvals <- sample(n)    #Random shuffle of the numbers from 1:60

nTrain <- 0.7*n
nVal   <- 0.15*n
nTest  <- nrow(all.df) - nTrain - nVal

x.train <- x.all[idvals[1:nTrain],]
y.train <- y.all[idvals[1:nTrain],]

x.val   <- x.all[idvals[(nTrain+1):(nTrain+nVal)],]
y.val   <- y.all[idvals[(nTrain+1):(nTrain+nVal)],]

x.test  <- x.all[idvals[(nTrain+nVal+1):n],]
y.test  <- y.all[idvals[(nTrain+nVal+1):n],]


# Create a KNN Model ------------------------------------------------------
library(FNN)

# knn.mod  <- knn.reg(train = x.train, test = x.test, y = y.train, k = 3)
# knn.pred <- knn.mod$pred
# MSE.knn  <- sqrt(sum((knn.pred - y.val)^2)/nrow(x.val)) #Calculate the MSE from Test Data

# Create Many KNN Models --------------------------------------------------

##First we'll create a function that gives us a list of these values
  #Use the Validation Data for the MSE
  mod.make <- function(xtrain, xtest, ytrain, ytest, k){
    
  knn.mod  <- knn.reg(train = xtrain, test = xtest, y = ytrain, k = k)
  knn.pred <- knn.mod$pred
  MSE.knn.val <- sqrt(sum((knn.pred - y.val)^2)/nrow(x.val)) #Calculate the MSE from Test Data
  
    return(list("Model" = knn.mod, "Predictions" = knn.pred, "MSE" = MSE.knn.val))
    
  }
  
##Now we'll use a loop to create a bunch of models
k.n <- 200
  
  knn.list <- list()
  for (i in 1:k.n) {
    
  knn.list[[i]] <- mod.make(x.train, x.test, y.train, y.test, k = i) 
    
  }

##Use another loop to compile the MSE Values
  MSE.train.vec <- vector(length = length(knn.list))
  MSE.val.vec <- vector(length = length(knn.list))
  
  for (i in 1:length(knn.list)) {
  MSE.train.vec[i] <- knn.list[[i]]$MSE
  MSE.val.vec[i] <- knn.list[[i]]$MSE
  
  }
  
##Use this to decide on the best K-value
  k.best <- which.min(MSE.val.vec)
  print(paste("The best k-value is", k.best))  
  
  ###Visualise this in a plot
  error.df <-  data.frame(k = (1:k.n), TrainingError = MSE.train.vec, ValError = MSE.val.vec)
  plot(x = error.df$k, y = error.df$ValError, type = 'b', lty = 1)
  lines(x = error.df$k, y = error.df$TrainingError, col = "red")

# Make a KNN with the best K-value ----------------------------------------
#mod.make(x.train, x.test, y.train, y.test, k = k.best)
  

