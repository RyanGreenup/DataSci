
# Preamble ----------------------------------------------------------------
  rm(list=ls())
  layout(matrix(1))
  # Import Data Set
   this.dir <- dirname(parent.frame(2)$ofile)
   setwd(this.dir)
  co2_df <- read.csv("MaunaLoaCO2.csv")
  
  #Assign Variables
  head(co2, 3)
  
  co2 <- co2_df$co2.14
  month <- factor(co2_df$month.14)
  monthno. <- 1:length(co2)
  co2.ts <- ts(co2, start = 1, end=length(co2))
  
  
  # Generate plot of Data
  
  co2_plot<- function(){
    
    plot(co2.ts, col="chocolate4",lty=1, cex=0.3,lwd=3, type="l", 
         ylab="Average Monthly Temperature", 
         xlab="Consecutive Month No.", 
         main="Average Monthly Temperature over time")      
                                  }
co2_plot()



# Create a Multiple Linear Regression -------------------------------------

co2.mlr <- lm(co2~monthno.+month)
fittedco2.mlr <- co2.mlr$fitted.values
co2sum <- summary(co2.mlr)

  #Plot the Multiple Linear Regression
    co2_plot()
    points(x=monthno., y=fittedco2.mlr, col="grey40", lwd=4, lty=9, type="l")
    legend(0,375, legend = c("MLR, Discrete Month Values", "Observed Values"),
            fill = TRUE, col=c("chocolate4", "grey40"),lty=(1), lwd=c(2,4))
   
    head(co2sum$coefficients, n=3)
    
    
    
    
# Time Series Plot of the Residuals ---------------------------------------

co2.resid <- co2.mlr$residuals
co2.resid.ts <- ts(co2.resid, start = 1, end=length(co2.resid))
ts.plot(co2.resid.ts, xlab="Residual Value from Model", 
        ylab="Sequential Month Number", 
        main="CO2 values over time", 
        lwd=5, col="royalblue3")



# Standard Diagnostics ----------------------------------------------------

layout(matrix(nrow=2, data=1:4))
plot(co2.mlr, col="lightsteelblue4", cex=1.3, lwd=3)
layout(matrix(1))

# acf of residual values --------------------------------------------------
acf(co2.resid, col='tan', lwd=10, main="Autocorrelation of Residual resulting from MLR")


# samira plots ------------------------------------------------------------

library(astsa)
sarima(co2.resid, p=0, d=0, q=0)


