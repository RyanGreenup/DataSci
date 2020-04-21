
# Preamble ----------------------------------------------------------------
  ##Load Packages
if(require('pacman')){
  library('pacman')
}else{
  install.packages('pacman')
  library('pacman')
}

pacman::p_load(xts, sp, gstat, ggplot2, rmarkdown, reshape2, ggmap,
               parallel, dplyr, plotly)


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
train.df <- read.csv("titanictrain.csv", TRUE, ",")


# Clean Data --------------------------------------------------------------
head(train.df)
  #What needs to be done:
    # ------------------------------------------
    #Replace Male/Female
    #Replace NAs in Age column with Median
    #Replace Embark point with the most common one (isn't that just the median?)
      #Replace Embark point with numeric values

#Replace Male/Female values with numeric values
train.df$Sex <- as.numeric(train.df$Sex)

#Replace Nas in Age Column with Median Value
train.df$Age[is.na(train.df$Age)] <- median(train.df$Age, na.rm = TRUE)

#Make Embark point numeric and replace with most common value
  #You've gotta add the most common value in first because
  #Otherwise there are four levels not three


  ## replace empty (unknown) embarkation point with most frequent one 
  most_embarked <- levels(train.df$Embarked)[which.max(tabulate(train.df$Embarked))] 
  train.df$Embarked[train.df$Embarked == ""] <- most_embarked

  # replace C, Q & S with numeric values 
  train.df$Embarked <- as.numeric(factor(train.df$Embarked))


# Standardise Data --------------------------------------------------------

  # If columns have different scales the penalty terms may be biased/unfair, hence 
  # before performing ridge regression it is necessary to scale all columns to 
  # have unit variance

    # This is done with the familiar z=(obs. - mean)/std. dev

head(train.df)
  
  
  trainStd.df <- train.df[!(names(train.df) %in% c("Survived", "Name", "Cabin", "Ticket"))]
  trainStd.df$Embarked <- as.numeric(trainStd.df$Embarked)
  head(trainStd.df)
  
    for(j in 1:ncol(trainStd.df)){
     x <- trainStd.df[,j]
     mu <- mean(trainStd.df[,j])
     sig <- sd(trainStd.df[,j])
      
     trainStd.df[,j] <- (x-mu)/sig
    }
  
  head(trainStd.df)
  

# Export the Standardised Training Set ------------------------------------

  trainStdfin.df <- cbind(train.df[(names(train.df) %in% c("Survived", "Name", "Cabin", "Ticket"))], trainStd.df)
  trainStdfin.df <- trainStdfin.df[,c(4,5,6,1,7,8,9,10,2,11,3,12)]

  write.csv(x = trainStdfin.df, file = "TitanictrainSTD.csv")

# Test Set ----------------------------------------------------------------
  #It is important to use the medians/means/sd from the training data
    #to prevent from introducing data leakage.

  ## Load the Data Set ----------------------------------------------------
  test.df <- read.csv("titanictest.csv", TRUE, ",")
  
  ## Clean the Test Set using Training Medians ----------------------------

    #What needs to be done:
      # ------------------------------------------
      #Replace Male/Female
      #Replace NAs in Age column with Median
      #Replace Embark point with the most common one (isn't that just the median?)
        #Replace Embark point with numeric values
  
  #Replace Male/Female values with numeric values
  test.df$Sex <- as.numeric(test.df$Sex)
  
  #Replace Nas in Test Data Age Column with Median Values from Training Data
  test.df$Age[is.na(test.df$Age)] <- median(train.df$Age, na.rm = TRUE)
  
  #Make Embark point numeric and replace with most common value
    #You've gotta add the most common value in first because
    #Otherwise there are four levels not three
  
    ## replace empty points of embarkment in the test data with most frequent 
      #one from the training data 
    test.df$Embarked[test.df$Embarked == ""] <- most_embarked
  
    # replace C, Q & S with numeric values 
    test.df$Embarked <- as.numeric(factor(test.df$Embarked))
  

  ## Standardise the Test Set using Training Medians ----------------------
   
    # If columns have different scales the penalty terms may be biased/unfair, hence 
    # before performing ridge regression it is necessary to scale all columns to 
    # have unit variance
  
      # This is done with the familiar z=(obs. - mean)/std. dev
    
    ##To prevent Data leakage use the standard deviation and mean value from the 
      #training data in the testing data
  
    testStd.df <- test.df[!(names(test.df) %in% c("Name", "Cabin", "Ticket"))]
    testStd.df$Embarked <- as.numeric(testStd.df$Embarked)
    head(testStd.df)
    
      for(j in 1:ncol(testStd.df)){
       x <- testStd.df[,j]          #Testing value
       mu <- mean(trainStd.df[,j])  #Training value
       sig <- sd(trainStd.df[,j])   #Training value
        
       testStd.df[,j] <- (x-mu)/sig
      }
    
    head(testStd.df)
    
    mean(tabulate(as.numeric((is.na(testStd.df)))))
    
# Export the Standardised Training Set ------------------------------------

  testStdfin.df <- cbind(test.df[(names(test.df) %in% c("Name", "Cabin", "Ticket"))], testStd.df)
  testStdfin.df <- testStdfin.df[,c(4,5,1,6,7,8,9,2,10,3,11)]
  
  write.csv(x = testStdfin.df, file = "TitanictestSTD.csv")
  