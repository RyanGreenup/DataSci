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


# Linear Model ------------------------------------------------------------

#Create the Linear Model and Prediction Interval
y<-co2_df[c(1:156),]
y.fit<-lm(co2_df$co2.14~0+time+factor(month.14))
yf<-predict(y.fit, data.frame(time=c(157:168), 
                              month.14=factor(c(1:12))),
                              interval='confidence')

#Plot the prediction Interval and draw the 
plot(c(157:168),yf[,1],type='b',ylim=c(368,380),
     ylab='obs=red', 
     xlab='regression only', 
     col=my_colours[1], lwd=3)
lines(c(157:168),
      obs,type='b',
      col=my_colours[2], 
      lwd=4)
lines(c(157:168),
      yf[,2], 
      col=my_colours[3], 
      lwd=4)
lines(c(157:168),
      yf[,3], 
      col=my_colours[4], 
      lwd=3)


legend(158,380, 
       legend = c("Observed CO2 Values", 
                  "Upper Prediction Interval for CO2 Level", 
                  "Fitted Value by Linear Model", 
                  "Lower Prediction Interval for CO2 level"),
       fill = TRUE, 
       col=c(my_colours[2], my_colours[4], 
             my_colours[1], my_colours[3]),lty=(1), 
       lwd=c(2,4))


# ARIMA Model -------------------------------------------------------------

  #Create the assignments
  z <- y.fit$residuals
  z.fit <- arima(z, order=c(2,0,0), include.mean = FALSE)
  z.fit
  z_for <- forecast(z.fit, 12)
  
  #Create the upper/lower lines
  midval <- yf[,1]+z_for$mean
  low <- yf[,1]+z_for$lower[,2]
  upp <- yf[,1]+z_for$upper[,2]  

  #Plot the lines
   plot(c(157:168), midval, type ="b", 
        ylim = c(368, 380), 
        xlab="regression wirh AR(2)", col=my_colours[1])
  lines(c(157:168),  obs, type = "b", col=my_colours[2])
  lines(c(157:168),  low, col=my_colours[3])
  lines(c(157:168),  upp, col=my_colours[4])

  #Create a Legend


legend(157,370, 
       legend = c("Upper Prediction Interval for CO2 Level",
                  "Linear fit + ARIMA Forecast", 
                  "observed CO2 Levels", 
                  "Lower Prediction Interval for CO2 level"),
       fill = TRUE, 
       col=c(my_colours[4], my_colours[1], 
             my_colours[2], my_colours[3]),lty=(1), 
       lwd=c(2,4))

round(cbind(obs, midval, low, upp), 1)








