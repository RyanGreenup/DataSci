
# Preamble ----------------------------------------------------------------

#Import Data Set
 # this.dir <- dirname(parent.frame(2)$ofile)
 # setwd(this.dir)
co2_df <- read.csv("MaunaLoaCO2.csv")

#Create the Assignments
co2.14 <- co2_df$co2.14
month.14 <- co2_df$month.14
time <- co2_df$time



# Fit a Linear Model ------------------------------------------------------

co2.lm <- lm(co2.14~time+factor(month.14))



# Fit an ARIMA model ------------------------------------------------------

#Plot the data
plot.ts(co2.lm$residuals, col="royalblue", lwd=3, 
        ylab="Residual Values", 
        main="Residuals plotted over time")

#Plot the aCF/PACF 
layout(matrix(1:2, ncol=2))
acf(co2.lm$residuals, col="deepskyblue", lwd=5, 
    main="Residuals from CO2 Linear Model")
pacf(co2.lm$residuals, col="deepskyblue", lwd=5, 
     main="Residuals from CO2 Linear Model")
layout(matrix(1))

#Choose ARIMA(2,0,0)

#Fit that model
z.fit <- arima(co2.lm$residuals, 
               order=c(2,0,0), 
               include.mean = FALSE)
require(lmtest) 
coeftest(z.fit)

#Alternative Method to fit model
# z.fit <- sarima(z, 2,0,0,0)


# Plot the ARMA Residuals -------------------------------------------------
layout(matrix(c(3,1,3,2), 2))
acf(z.fit$residuals, main="Residuals over time", lwd=4, col="darkorchid2")
qqnorm(z.fit$residuals, 
       main="Quantile plot of residuals against Normal data", 
       col="darkorchid2", 
       cex=1.5)
abline(1,0)
plot.ts(z.fit$residuals, ylab="ARMA Residual", 
        main="ARMA Residuals over time", 
        lwd=2, col="darkorchid4")

    #Do it with astsa package
    require(astsa)
    sarima(z.fit$residuals, 2,0,0)














