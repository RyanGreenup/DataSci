#Preamble
 # this.dir <- dirname(parent.frame(2)$ofile)
 # setwd(this.dir)
co2_df <- read.csv("MaunaLoaCO2.csv")
require(astsa)
require(forecast)

#Create Assignments
obs <- tail(co2_df$co2.14, 12)
my_colours <- c("purple", "red", "pink", "seagreen")
co2.14 <- co2_df$co2.14
month.14 <- co2_df$month.14
time <- co2_df$time

#Create the Linear Model
co2.lm <- lm(co2.14~time+factor(month.14))

#Create a Residual Analysis Function
arima_resid <- function(resid, p=0, q=0){
  
  
  #residual diagnostics
  layout( matrix(nrow=3, ncol=2, byrow=1, data=c(1,1,2,3,4,4)))
  
  ts.plot(resid, xlab="residuals", 
          main="residuals over time", col=my_colours[3], lwd=2)
  abline(0,0, col=my_colours[2], lwd=3)
  
  x.acf <- acf(resid, plot = 0)
  #remove lag 0
  x.acf$acf[1] <- NA
  plot(x.acf,lwd=10, col=my_colours[1], ylim=c(-1,1), main="Residuals")
  
  qqplot(resid, rnorm(2000,0, sd(resid)), col=my_colours[2], cex=2)
  abline(0,1, lwd=3, col=my_colours[3])
  
  require(FitAR)
  # LJBT <- LjungBoxTest(resid, k=1)
  # plot(LJBT[1:20,3], ylim=c(0,1))
  LBQPlot(resid, k=p+q)
  
  layout(matrix(1)) 
}

#Plot ACF/PACF Values
layout(matrix(1:2, ncol=2))
acf(co2.lm$residuals, main="CO2 Values", col="Royalblue1", lwd=5)
pacf(co2.lm$residuals, main="CO2 Values", col="Royalblue1", lwd=5)
layout(matrix(1))

#Hence fit an ARMA(2,0)
co2.fit_correct <- arima(co2.lm$residuals, order = c(2,0,0))
require(lmtest)
coeftest(co2.fit_correct)


         
           
#Plot the ARMA (2,0) Residuals
arima_resid(co2.fit_correct$residuals, p=2)


# Demonstrate Model Misspecification --------------------------------------
co2.fit_incorrect <- arima(co2.lm$residuals, order = c(1,0,0))
require(lmtest)
coeftest(co2.fit_incorrect)
  #Plot the Residuals
    arima_resid(co2.fit_incorrect$residuals) 
    