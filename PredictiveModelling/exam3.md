---
title: "Final Exam-Question 3"
tags: [Notebooks/Predictive Modelling]
date: 2018-June-4
author: "Ryan Greenup (1780 5315)"
# output:
#   rmarkdown::html_document:
#     theme: lumen
output:
  prettydoc::html_pretty:
    theme: hpstr
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
  
  pacman::p_load(ggmap, plotly, EnvStats, ggplot2, GGally, corrplot, dplyr, tidyr, stringr, reshape2, cowplot, ggpubr, reshape2, ggplot2, rmarkdown, dplyr, plotly, rstudioapi, wesanderson, RColorBrewer, colorspace, prettydoc, glmnet, FNN)
```

##Set Working Directory
It is not necessary to set the working directory because this is a markdown file, the working directory is automatically set to the location of file

##Load the data
the data can be loaded using a relative file path, because the working directory is automatically set to
the file location

```r
all.df <- read.csv(file = "fuel.csv", header = TRUE, sep = ",")
```

```
## Warning in file(file, "rt"): cannot open file 'fuel.csv': No such file or
## directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
all.df[72,]
```

```
## Error in eval(expr, envir, enclos): object 'all.df' not found
```

#(a) Investigate the Data
First inspect the first few lines:


```r
head(all.df)
```

```
## Error in head(all.df): object 'all.df' not found
```

Consider calling `summary` and `str` over the data


```r
summary(all.df)
```

```
## Error in summary(all.df): object 'all.df' not found
```

```r
str(all.df)
```

```
## Error in str(all.df): object 'all.df' not found
```

##Plot the data
A pretty plot can be made using `ggplot2`


```r
all.df.plot <- all.df
```

```
## Error in eval(expr, envir, enclos): object 'all.df' not found
```

```r
all.df.plot$dow <- as.factor(all.df.plot$dow)
```

```
## Error in is.factor(x): object 'all.df.plot' not found
```

```r
ggplot(data = all.df.plot, aes(x = day, y = price, col = dow)) +
  geom_point(size = 7, alpha = 0.6) +
  labs(x = "Time", y = "Price (cents, AUD)", col = "Day of Week",
       main = "Fuel Prices") +
  theme_classic()
```

```
## Error in ggplot(data = all.df.plot, aes(x = day, y = price, col = dow)): object 'all.df.plot' not found
```

#(b) Create a simple linear regression Model
A simple linear regression can be created thusly


```r
fuel.lm <- lm(price ~ day, data = all.df)
```

```
## Error in is.data.frame(data): object 'all.df' not found
```

```r
pred.lm.val <- predict(fuel.lm, newdata = data.frame(day = 73))
```

```
## Error in predict(fuel.lm, newdata = data.frame(day = 73)): object 'fuel.lm' not found
```

```r
paste("On day 73 the expected price given the linear model would be", round(pred.lm.val, 2)) %>% 
  print()
```

```
## Error in paste("On day 73 the expected price given the linear model would be", : object 'pred.lm.val' not found
```

#(C) Detrending the data to create a better model

*I'm running out of time, I'll do the KNN stuff and come back to this*

#(d) Create a KNN Model

 ##Standardise the data
 First it is necessary to standardise the data before it can be
 used in a KNN regression Model:
 

```r
# Standardise the Data ----------------------------------------------------

  ##Subset the Data
  allStd.df <- subset(x = all.df, select = c("day", "dow"))
```

```
## Error in subset(x = all.df, select = c("day", "dow")): object 'all.df' not found
```

```r
  ##Standardise the data
  allStd.df <- scale(allStd.df) #This coerces the data into a matrix
```

```
## Error in scale(allStd.df): object 'allStd.df' not found
```

  ##Matrix input/output assignments
  It is necessary to use matrix input and output when training a KNN model
  using the `knn.reg` command
  

```r
x.all <- as.matrix(allStd.df)
```

```
## Error in as.matrix(allStd.df): object 'allStd.df' not found
```

```r
y.all <- subset(x = all.df, select = "price" )
```

```
## Error in subset(x = all.df, select = "price"): object 'all.df' not found
```

```r
y.all <- as.matrix(y.all)
```

```
## Error in as.matrix(y.all): object 'y.all' not found
```


  ##Create a testing and training split
  Split the data up so that the knn model can be trained
  

```r
# Create a Training and Test Split ----------------------------------------

#Shuffle the data
n       <- nrow(all.df)
```

```
## Error in nrow(all.df): object 'all.df' not found
```

```r
all.df.s <- all.df[sample(n),]
```

```
## Error in eval(expr, envir, enclos): object 'all.df' not found
```

```r
x.all.s <- x.all[sample(n),]
```

```
## Error in eval(expr, envir, enclos): object 'x.all' not found
```

```r
y.all.s <- y.all[sample(n),]
```

```
## Error in eval(expr, envir, enclos): object 'y.all' not found
```

```r
test.n  <- round(0.25*n)
```

```
## Error in 0.25 * n: non-numeric argument to binary operator
```

```r
train.n <- n-test.n
```

```
## Error in eval(expr, envir, enclos): object 'test.n' not found
```

```r
train.n+test.n
```

```
## Error in eval(expr, envir, enclos): object 'train.n' not found
```

```r
x.train <- x.all.s[1:train.n, ]
```

```
## Error in eval(expr, envir, enclos): object 'x.all.s' not found
```

```r
y.train <- y.all.s[1:train.n]
```

```
## Error in eval(expr, envir, enclos): object 'y.all.s' not found
```

```r
x.test <- x.all.s[((train.n+1):n),]
```

```
## Error in eval(expr, envir, enclos): object 'x.all.s' not found
```

```r
y.test <- y.all.s[((train.n+1):n)]
```

```
## Error in eval(expr, envir, enclos): object 'y.all.s' not found
```
  
  ##Create the KNN Model
  

```r
# Create a KNN Model ------------------------------------------------------
library(FNN)

knn.mod  <- knn.reg(train = x.all, test = x.test, y = y.train, k = 3)
```

```
## Error in as.matrix(train): object 'x.all' not found
```

```r
knn.pred <- knn.mod$pred
```

```
## Error in eval(expr, envir, enclos): object 'knn.mod' not found
```

```r
MSE.knn  <- sqrt(sum((knn.pred - y.test)^2)/test.n) #Calculate the MSE from Test Data
```

```
## Error in eval(expr, envir, enclos): object 'knn.pred' not found
```

  ##Predict with the KNN Model
  
  What will the price be on day 73 with the KNN Model (dow will be 2)
  

```r
#predict(object = knn.mod, newdata = data.frame("day" = 73, "dow" = 2))
```
  
  

