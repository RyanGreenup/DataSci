---
title: "02 Creating a Milk Model"
tags: [Notebooks/Predictive Modelling]
author: "Ryan Greenup"
date: "25 March 2018"
output: html_document
---



#Preamble
First as a matter of housekeeping, we need to load in pertinent packages and 
set the working directory


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

# Import the Data Set -----------------------------------------------------
milk.df <- read.csv("milk.csv", header = TRUE, sep = ",")
```

```
## Warning in file(file, "rt"): cannot open file 'milk.csv': No such file or
## directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
head(milk.df)
```

```
## Error in head(milk.df): object 'milk.df' not found
```

```r
    ##Create Assignments
    month <- milk.df$Month
```

```
## Error in eval(expr, envir, enclos): object 'milk.df' not found
```

```r
    prod  <- milk.df$Production
```

```
## Error in eval(expr, envir, enclos): object 'milk.df' not found
```

```r
    milk.lm <- lm(prod ~ month)
```

```
## Error in eval(predvars, data, env): object 'month' not found
```

#Inspect the Data Set
So first we'll look at what our data consists of:


```r
head(milk.df)
```

```
## Error in head(milk.df): object 'milk.df' not found
```

Now we will make a quick plot of the data


```r
plot(x = month, y = prod, lty = 1, type = "b")
```

```
## Error in plot(x = month, y = prod, lty = 1, type = "b"): object 'month' not found
```

```r
    ggplot(data = milk.df, aes(x = Month, y = Production)) +
      geom_line(col = "Royalblue", lwd = 1.25, alpha = 0.6) +
      theme_classic() +
      ggtitle("Milk Production") +
      geom_point(col = "purple", lwd = 2)
```

```
## Error in ggplot(data = milk.df, aes(x = Month, y = Production)): object 'milk.df' not found
```

#Create a Model
This model has a positive linear trend and appears to be seasonal,
perhaps an ARIMA model would be appropriate, but for the time being,
we will fit a linear trigonometric plot:


```r
# Create Linear Model -----------------------------------------------------

milk.lmtrig <- lm(prod ~ month + I(sin(2*pi*month/12)) + I(cos(2*pi*month/12)))
```

```
## Error in eval(predvars, data, env): object 'month' not found
```
Consider the resudiuals of the model:


```r
layout(matrix(nrow = 2, 1:4))
plot(milk.lmtrig)
```

```
## Error in plot(milk.lmtrig): object 'milk.lmtrig' not found
```

These residuals are normally distributed, thus their is no cause to 
discard the model.


```r
summary(milk.lmtrig)
```

```
## Error in summary(milk.lmtrig): object 'milk.lmtrig' not found
```


Thus our model becomes: 


$$PRODUCTION = 1.7 \times MONTH + 33.544 \bullet  
\sin(\frac{2\pi}{12}\times MONTH) + -67.94 \bullet  
\cos(\frac{2\pi}{12}\times MONTH)$$

#Plot the Model
In order to plot this model we will have to predict for it's values 
over a data frame


```r
  #In order to plot this model, we need to use it to predict
  #over a data frame
    x.pred <- 1:length(month)      
```

```
## Error in eval(expr, envir, enclos): object 'month' not found
```

```r
    y.pred <- predict(milk.lmtrig, data.frame(month = x.pred))
```

```
## Error in predict(milk.lmtrig, data.frame(month = x.pred)): object 'milk.lmtrig' not found
```

```r
    milk.mod <- data.frame("Month" = x.pred, "Production" = y.pred)    
```

```
## Error in data.frame(Month = x.pred, Production = y.pred): object 'x.pred' not found
```

Now we can make plots for this model to contrast it to the original:


```r
#Plot the Prediction over the initial plot
    
      #Base Plot
      plot(x = month, y = prod, lty = 1, type = "b")
```

```
## Error in plot(x = month, y = prod, lty = 1, type = "b"): object 'month' not found
```

```r
      lines(x = milk.mod$Month, y = milk.mod$Production, col = "red")
```

```
## Error in lines(x = milk.mod$Month, y = milk.mod$Production, col = "red"): object 'milk.mod' not found
```

```r
      #ggplot2
      ggplot(data = milk.df, aes(x = Month, y = Production)) +
        geom_line(col = "Royalblue", lwd = 3, alpha = 0.6) +
        theme_classic() +
        ggtitle("Milk Production and Model") +
        geom_point(col = "purple", lwd = 2, alpha = 0.6) +
        #add in the model
        geom_line(col = "Indianred", lwd = 1) 
```

```
## Error in ggplot(data = milk.df, aes(x = Month, y = Production)): object 'milk.df' not found
```

```r
        ##ggplot2 melted
        milk.df.fin <- data.frame("Month" = month,
                                  "Observed" = prod,
                                  "Modelled" = milk.mod$Production)  
```

```
## Error in data.frame(Month = month, Observed = prod, Modelled = milk.mod$Production): object 'month' not found
```

```r
        milk.df.fin.melt <- melt(milk.df.fin, id = "Month") 
```

```
## Error in melt(milk.df.fin, id = "Month"): object 'milk.df.fin' not found
```

```r
        ggplot(data = milk.df.fin.melt, aes(x = Month,
                                            y = value,
                                            col = variable)) +
          geom_line(lwd = 1.2, alpha = 0.9) +
          theme_classic() +
          ggtitle("Monthly Milk Production") +
          xlab("Milk Production") +
          scale_color_manual(values = c('Observed' = 'grey',
                                        'Modelled' = 'indianred'))
```

```
## Error in ggplot(data = milk.df.fin.melt, aes(x = Month, y = value, col = variable)): object 'milk.df.fin.melt' not found
```


#Conclusion
A trigonometric linear model appears to fit the seasonal data reasonably well 
both by looking at the plots and the residual distribution
