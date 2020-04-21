
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


# Forecast 24 observations ahead ------------------------------------------
layout(matrix(1))
    #forecast package
      require(forecast)
      forecast(z.fit, 4)
      plot(forecast(z.fit, 24), col="navy", lwd=2, 
           main="24 Observation Forecast")

      #astsa package
      sarima.for(co2.lm$residuals, 24, 2,0,0)


# Plot the expected values of CO2 -----------------------------------------

pred_df <- data.frame(time=c(169:192), month.14=factor(c(1:12, 1:12)))
lm_pred <- predict(co2.lm, pred_df, interval='confidence' )
error_fc <- forecast(z.fit,24)

m <- lm_pred[,1]+error_fc$mean
l <- lm_pred[,1]+error_fc$lower[,2]
u <- lm_pred[,1]+error_fc$upper[,2]

plot(c(169:192), lm_pred[,1], type = "b", ylim=c(372, 383), 
     col='steelblue', lwd=2, xlab="time", ylab="CO2 Value")
lines(c(169:192), m, type="b", col="violet", lwd=2)
lines(c(169:192), l, col="red", lwd=3)
lines(c(169:192), u, col="red", lwd=3)

legend(170,383, 
       legend = c("Predicted LM CO2 Value + Mean Seasonal Forecast Error", 
                  "LM Precicted CO2 Value", 
                  "95% forecast interval for expected error"),
       fill = TRUE, 
       col=c("violet", "steelblue", "red"),
       lty=(1), 
       lwd=c(2,4)

)




# Compare the Forecasts of a Model with and Without the ARMA(2,0)  --------

#Only the Linear Model
y <- co2_df[c(1:156),]
y.lm <- lm(co2.14~time+factor(month.14))
pred_df <- data.frame(time=c(157:168), month.14=factor(c(1:12)))
y_pred <- predict(y.lm, pred_df, interval='confidence')

plot_dst <- c(157:168)

plot(plot_dst, y_pred[,1], type = "b", 
     ylim= c(368, 380), xlab="Time", 
     ylab="CO2 Values", col="darkmagenta", lwd=2)
lines(plot_dst, tail(co2_df$co2.14,12), type = "b", col="springgreen3", lwd=2)

lines(plot_dst, y_pred[,3], col="red")
lines(plot_dst, y_pred[,2], col="red")

legend(158,370, 
       legend = c("Observed CO2 Values", 
                  "LM fitted CO2 Value", 
                  "95% confidence interval LM only"),
       fill = TRUE, 
       col=c("springgreen3", "darkmagenta", "red"),
       lty=(1), 
       lwd=c(2,4)
       
)

#Inclusive of the ARIMA prediction
z <- y.lm$residuals
z.fit <- arima(z, order=c(2,0,0), include.mean = FALSE)
z.for <- forecast(z.fit, 12)

m <- y_pred[,1]+z.for$mean
l <- y_pred[,1]+z.for$lower[,2]
u <- y_pred[,1]+z.for$upper[,2]

plot_dist <- c(157:168)
obs <- tail(co2_df$co2.14,12)

#Plot the fitted/expected CO2 Values
plot(plot_dist, m, type = "b", ylim = c(368, 380), 
     ylab="CO2 Levels", xlab="time", col="springgreen3", lwd=2)
#Add the Observed CO2 Levels over that interval
lines(plot_dist, obs, type = "b", col="mediumpurple", lwd=2)

#add the Confidence Intervals
lines(plot_dist, l, col="red", lwd=2)
lines(plot_dist, u, col="red", lwd=2)

#add the Legend
legend(158,370, 
       legend = c("Fitted CO2 Values", 
                  "Observed CO2 VAlues", 
                  "95% Forecast using LM+ARIMA"),
       fill = TRUE, 
       col=c("springgreen3", "mediumpurple", "red"),
       lty=(1), 
       lwd=c(2,4)
       
)




