
# Preamble ----------------------------------------------------------------
#Import Data Set
# library(readr)
# Ozone_1_ <- read_csv("C:...Ozone(1).csv")
# View(Ozone_1_)
#Assign Variables

ozone_response_y <- Ozone_1_$airoz
solar_predict_x1 <- Ozone_1_$solar
wind_predict_x2 <- Ozone_1_$wind
temp_predict_x3 <- Ozone_1_$temp
month_predict_x4 <- Ozone_1_$month
factor_month_predict_x4 <- factor(month_predict_x4)



# Create the Linear Model -------------------------------------------------
ozone_mult.lm <- lm(ozone_response_y~solar_predict_x1+wind_predict_x2
                    +temp_predict_x3+factor_month_predict_x4)
#Summarise the Linear Model
summary(ozone_mult.lm)
sum_ozonelm <- summary(ozone_mult.lm)

predict(ozone_mult.lm, data.frame(temp_predict_x3=30,
                                  wind_predict_x2=10,
                                  solar_predict_x1=92,
                                  factor_month_predict_x4="8"),
        interval='predict')

# Variable Selection -------------
  #Create a Vector of P-Values
  sum_ozonelm$coefficients ##The ozone coefficients
  sum_ozonelm$coefficients[25:32] ##The ozone coefficient p-values

    pvalcoef <- sum_ozonelm$coefficients[25:32]
    coefnames <- c("Intercept", "Solar", "Wind", "Temp", "June", "July", "August", "September")
      
    names(pvalcoef) <- coefnames
    
# Remove First Variable ---------------------------------------------------
sum_ozonelm$coefficients ##The ozone coefficients
sum_ozonelm$coefficients[25:32] ##The ozone coefficient p-values

monthpval <- pvalcoef[5:8]; print(monthpval)
remainingpval <- pvalcoef[2:4]; print(remainingpval)


if(
  min(monthpval)>max(remainingpval)
){
  print("Remove the Months variable") 
}else(
  print("Do Not Remove the Months Variable")
)
#step(ozone_mult.lm, direction = 'backward', trace = TRUE)

max(remainingpval)  #This corresponds to solar_predict_x1


# Recreate the Linear Regression ------------------------------------------
ozone_mult.lm <- lm(ozone_response_y~wind_predict_x2
                    +temp_predict_x3+factor_month_predict_x4)
sum_ozonelm <- summary(ozone_mult.lm)

  #Re-Create the p-value vector
  sum_ozonelm$coefficients ##The ozone coefficients
  sum_ozonelm$coefficients[22:28] ##The ozone coefficient p-values
  
  pvalcoef <- sum_ozonelm$coefficients[22:28]; print(pvalcoef)
  coefnames <- c("Intercept", "Wind", "Temp", "June", "July", "August", "September")
  
  names(pvalcoef) <- coefnames; print(pvalcoef)
  
  monthpval <- pvalcoef[4:7]; print(monthpval)
  remainingpval <- pvalcoef[2:3]; print(remainingpval)


# Remove Second Variable ---------------------------------------------------


  if(
    min(monthpval)>max(remainingpval)
  ){
    print("Remove the Months variable") 
  }else(
    print("Do Not Remove the Months Variable")
  )

#Given that the lowest variable in the months is still larger than
#all other variables, the next variable to remove is the months variable


# Recreate the Linear Regression ------------------------------------------
ozone_mult.lm <- lm( (ozone_response_y) ~ (wind_predict_x2) + (temp_predict_x3) )
sum_ozonelm <- summary(ozone_mult.lm); print(sum_ozonelm)


# Forecast the Data -------------------------------------------------------
#Values to forecast
#REMEMBER THAT MONTH MUST BE A FACTOR!!!!
fcastvals <- data.frame(
  wind_predict_x2=12,
  temp_predict_x3=78
  
)

# Confidence Interval -----------------------------------------------------
predict(ozone_mult.lm, fcastvals, interval = 'confidence')

#Prediction Interval -----------------------------------------------------
predict(ozone_mult.lm, fcastvals, interval = 'predict')





# Create the Confidence  Plot ---------------------------------------------------------
alphaint <- 0.95

upr_conf <- predict(ozone_mult.lm, fcastvals, level=alphaint, interval='confidence')[3]
lwr_conf <- predict(ozone_mult.lm, fcastvals, level=alphaint, interval='confidence')[2]

# Plot the Confidence Interval --------------------------------------------
#The histogram of possible values, corresponds to a normal distribution along the y-axis
# This normal distribution will have a mean value of the fitted y-value
# The standard deviation will correspond to a normal distribution where the Z-value occurs at 41.9 rather than 1.96

mean_conf_y <- predict(ozone_mult.lm, fcastvals, level=alphaint, interval='confidence')[1]
sd_conf_y <- -(upr_conf-mean_conf_y)/qnorm(0.025, mean=0, sd=1)

possibleyvals_conf <- rnorm(n=100000, mean=mean_conf_y, sd=sd_conf_y)
hist(possibleyvals_conf, prob=TRUE, lwd=2, main = "Confidence Interval",
     xlab = "Ozone Level", border = "blue" ) #Prob chooses probability of frequency for the histogram.
mtext(" There is a 95% probability that the mean value of the ozone level (y-value)is contained therein", font = 2)
curve(dnorm(x, mean=mean(mean_conf_y), sd=sd(possibleyvals_conf)), add=TRUE, col="pink", lwd=2) #Draws the actual density function
#lines(density(possibleyvals_conf), col='purple', lwd=2) #Draws the observed density function
abline(v=upr_conf, col='purple', lwd=3)  
abline(v=lwr_conf, col='purple', lwd=3)
abline(v=mean_conf_y, lwd=2, lty='dotted')  
mtext(print(round(mean_conf_y),2), 1:0, font = 1 )






# Create the  Prediction Plot ---------------------------------------------------------
alphaint <- 0.95

upr_pred <- predict(ozone_mult.lm,
                    fcastvals,
                    level=alphaint,
                    interval='predict')[3]
lwr_pred <- predict(ozone_mult.lm,
                    fcastvals,
                    level=alphaint,
                    interval='predict')[2]

# Plot the Predidence Interval --------------------------------------------
#The histogram of possible values, corresponds to a
#normal distribution along the y-axis
# This normal distribution will have a mean value of 
#the fitted y-value The standard deviation will 
#correspond to a normal distribution where the Z-value occurs at 41.9 rather than 1.96

mean_pred_y <- predict(ozone_mult.lm,
                       fcastvals,
                       level=alphaint,
                       interval='predict')[1]
sd_pred_y <- -(upr_pred-mean_pred_y)/qnorm(0.025, mean=0, sd=1)

possibleyvals_pred <- rnorm(n=100000, mean=mean_pred_y, sd=sd_pred_y)
hist(possibleyvals_pred, prob=TRUE, lwd=2,
     main = "Prediction Interval",
     xlab = "Ozone Level", border = "cyan" 
) #Prob chooses probability of frequency for the histogram.
mtext(" There is a 95% probability that the ozone level (y-value)is contained therein", font = 2)
curve(dnorm(x, mean=mean(mean_pred_y),
            sd=sd(possibleyvals_pred)),
      add=TRUE, col="cyan", lwd=2
) #Draws the actual density function
#lines(density(possibleyvals_pred), col='purple', lwd=2) #Draws the observed density function
abline(v=upr_pred, col='blue', lwd=3)  
abline(v=lwr_pred, col='blue', lwd=3)
abline(v=mean_pred_y, lwd=2, lty='dotted')  
mtext(print(round(mean_pred_y),2), 1:0, font = 1 )



















