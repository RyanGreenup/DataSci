
# Preamble ----------------------------------------------------------------
# this.dir <- dirname(parent.frame(2)$ofile)
# setwd(this.dir)
ts.series.df <- read.csv("ts.series.csv")
require(lmtest)

#Select Time Series
ts.series <- ts.series.df[10]

#Residual Plotting Function
arima_resid <- function(resid, p=0, q=0){
  
  
  #Residual diagnostics
  layout( matrix(nrow=3, ncol=2, byrow=1, data=c(1,1,2,3,4,4)))
  
  ts.plot(resid, xlab="Residuals", 
          main="Residuals over Time", col="sienna", lwd=2)
  abline(0,0, col="sienna", lwd=3)
  
  x.acf <- acf(resid, plot = 0)
  #Remove Lag 0
  x.acf$acf[1] <- NA
  plot(x.acf,lwd=10, col="sienna", ylim=c(-1,1), main="Residuals")
  
  qqplot(resid, rnorm(2000,0, sd(resid)), col="sienna", cex=2)
  abline(0,1, lwd=3, col="sienna")
  
  require(FitAR)
  # LJBT <- LjungBoxTest(resid, k=1)
  # plot(LJBT[1:20,3], ylim=c(0,1))
  LBQPlot(resid, k=p+q)
  
  layout(matrix(1)) 
}


# ARIMA Model Fitting (use one Template for all 10) -----------------------
    #plot the series
      ts.plot(ts.series, ylab="Time Series Value", col="violetred")
    
    #Difference the data
       # ts.series <- diff(as.ts(ts.series))
       # ts.plot(ts.series, ylab="Time Series Value", col="violetred")
       d <-0
    
      #Plot the ACF/PACF  
      layout(matrix(1:2, ncol=2))
      acf(ts.series, lag.max = 60, col="indianred",lwd=4 )
      pacf(ts.series, lag.max = 199, col="indianred3",lwd=4 )
      layout(matrix(1))
      #this is a close one, we'll say PACF approaches zero, ACF cuts of at 1
      #This model suggests another AR parametr, it will require overfitting
    p <- 0 
    q <- 1
    #Fit the Model
      ts.series_model <- arima(ts.series,order = c(p,d,q))
      coeftest(ts.series_model)
      
    #Residual Diagnostics
      arima_resid(ts.series_model$residuals, p=p, q=q)
      
    #Simulated Overfitting
      p <- p
      q <-3
      ts.series_model <- arima(ts.series,order = c(p,d,q))
      coeftest(ts.series_model)
     
      print("The overfit model has more normally distributed residuals and all coefficients are significant, hence the overfit model is chosen") 