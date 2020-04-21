---
title: "03 HousesCrime"
tags: [Notebooks/Predictive Modelling]
author: "Ryan Greenup"
date: "25 March 2018"
output: 
  html_document: 
    toc: yes
---



#Preamble
First Set the working directory, load the packages and import the data set:


```r
# Load Packages -----------------------------------------------------------

if(require('pacman')){
  library('pacman')
}else{
  install.packages('pacman')
  library('pacman')
}

pacman::p_load(xts, sp, gstat, ggplot2, rmarkdown, reshape2, ggmap,
               parallel, dplyr, plotly)


# Set the Working Directory -----------------------------------------------

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

#set_wd()


# Import Data Set ---------------------------------------------------------

housescrime.df <- read.csv("housescrime.csv", header = TRUE, sep = ",")
```

```
## Warning in file(file, "rt"): cannot open file 'housescrime.csv': No such
## file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
head(housescrime.df)
```

```
## Error in head(housescrime.df): object 'housescrime.df' not found
```

```r
##Create Assignments
price   <- housescrime.df$HousePrice
```

```
## Error in eval(expr, envir, enclos): object 'housescrime.df' not found
```

```r
crime   <- housescrime.df$CrimeRate
```

```
## Error in eval(expr, envir, enclos): object 'housescrime.df' not found
```

```r
density <- housescrime.df$HsPrc10k
```

```
## Error in eval(expr, envir, enclos): object 'housescrime.df' not found
```

```r
milesp  <- housescrime.df$MilesPhila
```

```
## Error in eval(expr, envir, enclos): object 'housescrime.df' not found
```

```r
popchg  <- housescrime.df$PopChg
```

```
## Error in eval(expr, envir, enclos): object 'housescrime.df' not found
```


#How to find the relationship between Crime and House Prices
First we will take a plot of the data and inspect what possible relationships may exist:


```r
plot(y = crime, x = price, main = "Crime Rates", 
     ylab = "Crime Rate", xlab = "Property Prices")
```

```
## Error in plot(y = crime, x = price, main = "Crime Rates", ylab = "Crime Rate", : object 'price' not found
```

There appears to be a slight negative but insignificant trend in crime for 
more expensive properties, this is made more difficult because there are not 
many observations for expensive houses, because such expensive houses are 
less common.

There is an outlier perhaps there is an underlying
socieconomic factor to justify removing it, e.g. riots or natural disasters?

It is probable, looking at the data, that there are more
variables influencing the crimer rate than just price.

#Can a Linear Regression be applied
Observe that the residuals for a linear model are not normally distributed:


```r
housescrime.lm <- lm(crime ~ price)
```

```
## Error in eval(predvars, data, env): object 'price' not found
```

```r
plot(y = crime, x = price, main = "Crime Rates", 
     ylab = "Crime Rate", xlab = "Property Prices")
```

```
## Error in plot(y = crime, x = price, main = "Crime Rates", ylab = "Crime Rate", : object 'price' not found
```

```r
abline(housescrime.lm)
```

```
## Error in abline(housescrime.lm): object 'housescrime.lm' not found
```

```r
layout(matrix(nrow = 2, 1:4))
plot(housescrime.lm)
```

```
## Error in plot(housescrime.lm): object 'housescrime.lm' not found
```

The residuals are not sufficiently normally distributed from the model to 
justify applying a linear model to this data, moreover, observe that
the price is less significant in a multiple linear regression than
the price:


```r
crime.mlm <- lm(crime ~ price + density + milesp +popchg)
```

```
## Error in eval(predvars, data, env): object 'price' not found
```

```r
summary(crime.mlm)
```

```
## Error in summary(crime.mlm): object 'crime.mlm' not found
```


This is good evidence to suggest that whilst price may be a variable
that effects the crime rate, there are other more significant variables
and that a linear model may not be the appropriate model.

#Conclusion
A simple linear regression between property prices and the crime rate is not
an appropriate model for the crime rate, because the residuals are not
sufficiently normally distributed, the data suggests that there may not be
a significant relationship between property prices and crime and there
is likely to be more variables affecting the crime rate than just the price
of property in the surrounding area.
Observe that the residuals are non-normal, thus a linear regression cannot be applied here, because the residuals from the model are not sufficiently normally distributed.
