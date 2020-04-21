
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
  # The data is already random so there's no need to shuffle it

n       <- nrow(all.df)
test.n  <- 200
train.n <- n-test.n
n
train.n
train.n+test.n

x.train <- x.all[1:train.n, ]
y.train <- y.all[1:train.n, ]

x.test <- x.all[((train.n+1):n),]
y.test <- y.all[((train.n+1):n),]


# Create a KNN Model ------------------------------------------------------
library(FNN)

knn.mod  <- knn.reg(train = x.train, test = x.test, y = y.train, k = 3)
knn.pred <- knn.mod$pred
MSE.knn  <- sqrt(sum((knn.pred - y.test)^2)/test.n) #Calculate the MSE from Test Data


# Create a Lasso Regression Model -----------------------------------------
library(glmnet)

lasso.mod  <- cv.glmnet(x = x.train, y = y.train, alpha = 1)
lasso.pred <- predict(object = lasso.mod, newx = x.test, s = 'lambda.min' )
MSE.lasso  <- sqrt(sum((lasso.pred - y.test)^2)/test.n) #Calculate the MSE from Test Data 


