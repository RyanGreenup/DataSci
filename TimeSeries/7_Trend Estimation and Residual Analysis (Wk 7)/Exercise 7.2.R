# Preamble ----------------------------------------------------------------
require(astsa)
require(xts)

#Import Data Frame
 this.dir <- dirname(parent.frame(2)$ofile)
 setwd(this.dir)
airpol_df <- read.csv("airpol.csv")

#Assign Variables
head(airpol_df)

o3 <- airpol_df$O3

no2 <- airpol_df$NO2
no <- airpol_df$NO
so2 <- airpol_df$SO2
pm10 <- airpol_df$PM10

# 1. Create the multiple Linear Regression --------------------------------

airpol.lm <- lm(o3~no2+no+so2+pm10)
airpolsumlm <- summary(airpol.lm)

airpolsumlm


# 2. Sample ACF's for Residuals -------------------------------------------



# 3. Residual Analysis Plots ----------------------------------------------

#0. Compare model to observed data:

  fittedo3 <- airpol.lm$fitted.values
  ts.plot(matrix(ncol=1, data=c(o3)),
          col="purple", lwd=2, 
          ylab="Ozone Level", xlab="Day of Observation", 
          main="Comparison of Observed Ozone with MLR Model")
  points(matrix(ncol=1, data=c(fittedo3)), type = "l", lty=9, col="red", lwd=3)
   legend(10,65, legend = c("MLR Model", "Observed Values"),
          fill = TRUE, col=c("purple", "Red"),lty=(1), lwd=c(2,4))
  

  #1. Standard Diagnostics
  layout_matrix <- matrix(nrow = 1, data = 1:4)
  layout(layout_matrix)
  plot(airpol.lm, col="indianred1", cex=2, lwd=2.5)

  #2. Time Series plot of Residuals
  layout_matrix <- matrix(nrow = 1, data = 1:1)
  layout(layout_matrix)
  airpol_resid <- o3-airpol.lm$fitted
  ts.plot(airpol_resid, xlab="Days", ylab="model Residual", lwd=3, type="b", col="seagreen")
  abline(0,0, lwd=5, lty=3, col="royalblue2")
  
  #3. ACF of Time-Series Residuals
  layout_matrix <- matrix(nrow = 2, data = 1:2)
  layout(layout_matrix)
  acf(airpol_resid, lag.max =100, main="Residuals", col="dodgerblue", lwd=5)
  acf(rnorm(length(airpol_resid)), lag.max = 100, main="Simulated White Noise", col="indianred3", lwd=5)
   
  #4. Compare ACF/PACF of Residuals
  require(astsa)
  acf2(airpol_resid, max.lag=100)
  acf2(rnorm(300), max.lag=250)
  
  #5. Comparing the residuals to a White Noise Model
  airpol_resid <- o3-airpol.lm$fitted
  sarima(airpol_resid, p=0, d=0, q=0) 





 