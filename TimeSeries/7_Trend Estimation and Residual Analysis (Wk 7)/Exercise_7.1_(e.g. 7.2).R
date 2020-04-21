
# Preamble ----------------------------------------------------------------
  rm(list=ls())
  layout(matrix(1))
  #Import Data Set
   this.dir <- dirname(parent.frame(2)$ofile)
   setwd(this.dir)
  avmonthtemp <- read.csv("AverageMonthlyTemperature.csv")
  
  #Assign Variables
  head(avmonthtemp)
  tail(avmonthtemp)
  
  temp <- avmonthtemp$AvTemperature
  month <- factor(avmonthtemp$Months)
  monthno. <- 1:length(temp)
  avmonthtemp.ts <- ts(temp, start = 1, end=144)
  
  
# Generate plot of Data
  
  avmonthtemp_plot <- function(ltype="b"){

    plot(avmonthtemp.ts, col="royalblue2", lwd=3, type=ltype, 
         ylab="Average Monthly Temperature", 
         xlab="Consecutive Month No.", 
         main="Average Monthly Temperature over time")      
                                  }
avmonthtemp_plot()



# Create a Simple Linear Regression ---------------------------------------

avmonthtemp.slr <- lm(temp~month)
fittedtemp.slr <- avmonthtemp.slr$fitted.values

  #Plot the Simple Linear Regression
    avmonthtemp_plot("l")
    points(x=monthno., y=fittedtemp.slr, col="red", lwd=4, lty=9, type="l")
    legend(0,-10, legend = c("SLR, Discrete Month Values", "Observed Values"),
            fill = TRUE, col=c("royalblue2", "Red"),lty=(1), lwd=c(2,4))

# Create a Multiple Linear Regression -------------------------------------

avmonthtemp.mlr <- lm(temp~monthno.+month)
fittedtemp.mlr <- avmonthtemp.mlr$fitted.values
avmonthtempsum <- summary(avmonthtemp.mlr)

  #Plot the Multiple Linear Regression
    avmonthtemp_plot("l")
    points(x=monthno., y=fittedtemp.mlr, col="red", lwd=4, lty=9, type="l")
    legend(0,-10, legend = c("MLR, Discrete Month Values", "Observed Values"),
            fill = TRUE, col=c("royalblue2", "Red"),lty=(1), lwd=c(2,4))
   
    head(avmonthtempsum$coefficients, n=3)
    
    
     
    

# Exercise 7.1 (e.g. 7.5) -------------------------------------------------
    avmonthtemp.resid <- avmonthtemp.mlr$residuals
    

# 1. Residual Diagnostics -------------------------------------------------
    layout(matrix(nrow=2, data=1:4))
    plot(avmonthtemp.mlr, lwd=2, cex=1.25, col="magenta3")
    layout(matrix(1))
# 2. Residual ACF Plot ----------------------------------------------------
    acf(avmonthtemp.resid, lwd=13, col="mediumorchid", main="ACF values of Resdiuals flowing from MLR model")

# 3. Residual Diagnostics using Sarima() ----------------------------------
  require(astsa)
    sarima(avmonthtemp.resid, 0,0,0)
    
    
    
    summary(avmonthtemp.slr)
    