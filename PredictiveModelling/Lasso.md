---
title: "10 Predicting Deat Rates with lasso and Ridge Regression"
tags: [Notebooks/Predictive Modelling]
date: 2018-June-3
# output:
   rmarkdown::pdf_document:
#     theme: lumen
#toc: true
#output:
#  prettydoc::html_pretty:
#    theme: hpstr
    highlight: github
    #Possible themes are cayman, tactile, architect, leonids, hpstr
---



#Preamble
##Clear Latent Variables
First clear latent variables that could be leftover from past work

```r
rm(list = ls())
```
##Load Packages
Now load all the necessary packages:

```r
if(require('pacman')){
    library('pacman')
  }else{
    install.packages('pacman')
    library('pacman')
  }
  
  pacman::p_load(ggmap, plotly, EnvStats, ggplot2, GGally, corrplot, dplyr, tidyr, stringr, reshape2, cowplot, ggpubr, reshape2, ggplot2, rmarkdown, dplyr, plotly, rstudioapi, wesanderson, RColorBrewer, colorspace, 
                 prettydoc, glmnet)
```

##Load the Dataset
Load in the deathrate dataset, as the working directory is the file location a
relative pathname will work.

```r
all.df <- read.csv(file = "deathrate.csv", header = TRUE, sep = ",")
```

```
## Warning in file(file, "rt"): cannot open file 'deathrate.csv': No such
## file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```
 

#Standardise the Data
So before being able to implement lasso or ridge regression, it is necessary
to standardise the model scales by making all the data relative to a zero-mean
and unit standard deviation.

##subset out the features
The observation index values obviously do not need to be standardised because they are not
predictor variables (i.e. features).

```r
  all.df.feat <- all.df[,-1]
```

```
## Error in eval(expr, envir, enclos): object 'all.df' not found
```

Now create a vector with and standard deviation values corresponding to the feature column numbers.
It's better practice to store vectors rather than implementing this within a loop because
when dealing with validation and training splits the training data needs to be reused anyway
to prevent data leakage.


```r
  MeanCol.vec <- sapply(all.df.feat, mean)
```

```
## Error in lapply(X = X, FUN = FUN, ...): object 'all.df.feat' not found
```

```r
  SDCol.vec <- sapply(all.df.feat, sd)
```

```
## Error in lapply(X = X, FUN = FUN, ...): object 'all.df.feat' not found
```

Create a data frame for the standardised feature data. this can just be a replica of 
the original data because the entries will be re-written later.

```r
  #Create data frames for the standard feature data
  allSTD.df.feat  <- all.df.feat
```

```
## Error in eval(expr, envir, enclos): object 'all.df.feat' not found
```

Now implement a loop to replace each column with a vector of standardised values:

```r
  for (i in 1:ncol(allSTD.df.feat)){
   x   <- all.df.feat[,i] 
   
   mu  <- MeanCol.vec[i]
   sig <- SDCol.vec[i]
   
    allSTD.df.feat[,i] <- (x-mu)/sig
  }
```

```
## Error in ncol(allSTD.df.feat): object 'allSTD.df.feat' not found
```

Take these standardised values and stitch them back together with the index values.
It's better to seperate -> loop -> restitch, because otherwise you'll spend the rest
of your life trying to debug incorrect column numbers within the for loop.
  

```r
  #Combine to create Std DataFrames
  
  allSTD.df <- cbind(I = all.df$I, allSTD.df.feat) 
```

```
## Error in eval(quote(list(...)), env): object 'all.df' not found
```

```r
  allSTD.df <- arrange(.data = allSTD.df, by.group = I) 
```

```
## Error in arrange(.data = allSTD.df, by.group = I): object 'allSTD.df' not found
```
  
Finally, export the standardised values to a csv

```r
  ##Write The Standardised data to a CSV
   write.csv(x = allSTD.df, file = "deathrateSTD.csv", row.names = FALSE)
```

```
## Error in is.data.frame(x): object 'allSTD.df' not found
```

# Train a Ridge Regression and Lasso Regression Model using Cross  --------
   
##Create the feature and output Matrix

Theres only 30 values in the training data, so training a model from that
      data is probably not a good idea, instead we will use cross validation
       on all the data
      
So we'll create a matrix of predictor variables and a matrix of output variables:


```r
x <- as.matrix(allSTD.df[,!(names(all.df) %in% c("I", "B"))])
```

```
## Error in as.matrix(allSTD.df[, !(names(all.df) %in% c("I", "B"))]): object 'allSTD.df' not found
```

```r
y <- as.matrix(allSTD.df[,(names(all.df) %in% c("B"))]) 
```

```
## Error in as.matrix(allSTD.df[, (names(all.df) %in% c("B"))]): object 'allSTD.df' not found
```


##Use Cross Validation to create a model
Cross validation can be implemented right in the `glmnet` function, this will
automatically:

1. use ridge/lasso regression to decide on coefficients and/or values
2. use cross validation to determine which lambda value performs best


```r
  ridge.mod <- cv.glmnet(x, y, family = "gaussian", alpha = 0)
```

```
## Error in nrow(x): object 'x' not found
```

```r
  lasso.mod <- cv.glmnet(x, y, family = "gaussian", alpha = 1)
```

```
## Error in nrow(x): object 'x' not found
```

The lambda value determined by the cross-validation is stored within the model and 
can be assigned to a variable as such:


```r
  lambda.min.ridge <- ridge.mod$lambda.min
```

```
## Error in eval(expr, envir, enclos): object 'ridge.mod' not found
```

```r
  lambda.min.lasso <- lasso.mod$lambda.min
```

```
## Error in eval(expr, envir, enclos): object 'lasso.mod' not found
```

In order to model the data we are going to need to make some predictions of the data
with our model, so:

+ specify the new input data (`newx =`) as the original model input data (`x`)
+ specify the lambda value (`s =`) as the lambda value that performed best in the cross validation, this is the lambda value that was stored above.


```r
  pred.ridgeSTD <- predict(ridge.mod, newx = x, s = lambda.min.ridge)
```

```
## Error in predict(ridge.mod, newx = x, s = lambda.min.ridge): object 'ridge.mod' not found
```

```r
  pred.lassoSTD <- predict(lasso.mod, newx = x, s = lambda.min.lasso)
```

```
## Error in predict(lasso.mod, newx = x, s = lambda.min.lasso): object 'lasso.mod' not found
```
  
Now these predictions are standardised so we need to make them relative to there original scales by using the formula:

$$X_i = \sigma_i \times X_{STD, i} + \bar{X}$$

```r
pred.ridge <-  pred.ridgeSTD*SDCol.vec["B"] + MeanCol.vec["B"]
```

```
## Error in eval(expr, envir, enclos): object 'pred.ridgeSTD' not found
```

```r
pred.lasso<-  pred.lassoSTD*SDCol.vec["B"] + MeanCol.vec["B"]
```

```
## Error in eval(expr, envir, enclos): object 'pred.lassoSTD' not found
```
  
Now a Base Plot can be made quite readily:


```r
      #Base Plot
      plot(x = all.df$I, y = all.df$B, lty = 1, type = "b",
           ylab = "Predicted Death Rate",
           xlab = "Observation Number",
           main = "Observed and Predicted Death Rate")
```

```
## Error in plot(x = all.df$I, y = all.df$B, lty = 1, type = "b", ylab = "Predicted Death Rate", : object 'all.df' not found
```

```r
      lines(x = all.df$I, y = pred.lasso, col = "red")
```

```
## Error in lines(x = all.df$I, y = pred.lasso, col = "red"): object 'all.df' not found
```

# Predictors for the best Lasso Model 

The Best lasso model is the model returned by the cross validation, it is stored as lasso.mod.

The Coefficient values from the model can be returned with:


```r
      lasso.mod.coef <- coefficients(lasso.mod)
```

```
## Error in coefficients(lasso.mod): object 'lasso.mod' not found
```

```r
      lasso.mod.coef
```

```
## Error in eval(expr, envir, enclos): object 'lasso.mod.coef' not found
```

The coefficient values that are not zeroed-out by the regression algorithm can be found:
      
      

```r
      LassoPredictors <- lasso.mod.coef[lasso.mod.coef[,1] > 0,]
```

```
## Error in eval(expr, envir, enclos): object 'lasso.mod.coef' not found
```

```r
      LassoPredictors <- sort(LassoPredictors)
```

```
## Error in sort(LassoPredictors): object 'LassoPredictors' not found
```

And this can be compiled into a statment with the `paste()` function


```r
      paste("The Predictors for the Best Lasso Model, in order of significance, are:",
            paste("",names(LassoPredictors[-1]),sep="", collapse=", "))     
```

```
## Error in paste("", names(LassoPredictors[-1]), sep = "", collapse = ", "): object 'LassoPredictors' not found
```

Thus the predictors for the Best Lasso Model are A8, A1, A14, A9 which correspond to:

+ `A8`;  Population per Square Mile
+ `A1`;  The Average Annual Precipitation
+ `A14`; The Sulphur Dioxide Polution Index
+ `A9`;  The size of the nonwhite population

      
      
  
      
      
