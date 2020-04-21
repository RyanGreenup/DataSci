
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

nTrain <- round(0.7*n)
nVal   <- round(0.15*n)
nTest  <- nrow(all.df) - nTrain - nVal

x.train <- x.all[idvals[1:nTrain],]
y.train <- y.all[idvals[1:nTrain],]

x.val   <- x.all[idvals[(nTrain+1):(nTrain+nVal)],]
y.val   <- y.all[idvals[(nTrain+1):(nTrain+nVal)],]

x.test  <- x.all[idvals[(nTrain+nVal+1):n],]
y.test  <- y.all[idvals[(nTrain+nVal+1):n],]


# Create a KNN Model ------------------------------------------------------


mod.make.trainvalidate <- function(k){
  knn.mod   <- knn.reg(train = x.train, test = x.val, y = y.train, k = k)
  knn.pred  <- knn.mod$pred
  MSE.val   <- sqrt(sum((knn.pred - y.val)^2)/nVal)
  
  return(list("Model" = knn.mod, "PredictedValues" = knn.pred, "ValidationError" = MSE.val))
  
}

##Now we'll use a loop to create a bunch of models
k.n <- 11
  
  knn.list <- list()
  for (i in 1:k.n) {
    
  knn.list[[i]] <- mod.make.trainvalidate(k = i) 
    
  }
  
##Use another loop to compile the MSE Values
  MSE.val.vec <- vector(length = length(knn.list))
  
  for (i in 1:length(knn.list)) {
  MSE.val.vec[i] <- knn.list[[i]]$ValidationError
  
  }
  
##Use this to decide on the best K-value
  k.best <- which.min(MSE.val.vec)
  print(paste("The best k-value is", k.best))  
    #Theres gotta be a better way to automatically select this,
      #maybe using diff() and some trick to select the vertex...
  
  ###Visualise this in a plot
  error.df <-  data.frame(k = (1:k.n), ValError = MSE.val.vec)
  plot(x = error.df$k, y = error.df$ValError, type = 'b', lty = 1)
  ggplot(error.df, aes(x = k, y = ValError)) + 
    geom_line(col = "indianred", lwd = 2) +
    geom_point(size = 6, col = "royalblue", alpha = 0.4) +
    labs(x = "K-Value", y = "Validation Error", title = "Model Error")
  
  
  #If I had time, it could be worth comparing the validation and
    #Training Error but, I simply don't have the time to 
    #figure that out now, the validation should rise for an overparameterised
    #model, even if it happens a little after the vertex
  

# Assess the Model With the Test Data -------------------------------------

  knn.mod   <- knn.reg(train = x.train, test = x.test, y = y.train, k = k.best)
  knn.pred  <- knn.mod$pred
  MSE.test<- sqrt(sum((knn.pred - y.test)^2)/nTest)
  
  paste("The Test error for the model, using the best k-value (", k.best, ") is ", MSE.test)

# Plot the Predictions ----------------------------------------------------
    ##Create a dataframe of values
    assess.df <- data.frame(Index = 1:nTest, Observed = y.test, Modelled = knn.pred, Loss = abs(y.test-knn.pred))
    
    #Subset to make it readable
    dot.n <- 30
    assess.df <- assess.df[runif(n = 60, min = 1, max = nrow(assess.df)),]
  
    #Base Plot
    plot(Observed ~ Index, data = assess.df, pch = 16, col = "Purple", xlab = "Index", ylab = "Observed Values")
    points(Modelled ~ Index, data = assess.df, pch = 1, col = "red")

    ##Use ggplot2
      #It refuses to plot a melted data frame, no error, just won't plot this data, maybe there's
        #A missing row, quite literally nothing I can do about it, there's just not enough
        # time to debug it.
    