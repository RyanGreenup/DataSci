
# Preamble ----------------------------------------------------------------

# Test and install pacman package
if (require('pacman')) {
  library('pacman')
} else{
  install.packages('pacman')
  library('pacman')
}

# Use Pacman to install other useful packages
pacman::p_load(tidyverse, rmarkdown, dplyr, plotly, EnvStats, mise, readr)

mise()




#Import Data Set
# library(readr)
# Ozone_1_ <- read_csv("C:...Ozone(1).csv")
Ozone_1_ <- read_csv("../../0DataSets/Ozone(1).csv")

# View(Ozone_1_)
#Assign Variables

ozone_response_y <- Ozone_1_$airoz
solar_predict_x1 <- Ozone_1_$solar
wind_predict_x2 <- Ozone_1_$wind
temp_predict_x3 <- Ozone_1_$temp
month_predict_x4 <- Ozone_1_$month
factor_month_predict_x4 <- factor(month_predict_x4)
length(ozone_response_y)


hist(ozone_response_y, breaks = 10, main = "Histogram of Ozone", xlab = "Ozone", freq = FALSE, border = "#3AC74C", col = "#7DF58D", lwd=3)
curve(dnorm(x, mean=mean(ozone_response_y), sd=sd(ozone_response_y)), add=TRUE, col="#0BAB21", lwd=5) #Draws the actual density function
qqnorm(ozone_response_y)

ozone_response_y <- log(ozone_response_y)

hist(ozone_response_y, breaks = 18, main = "Histogram of Log(Ozone)", xlab = "Log(Ozone)", freq = FALSE, border = "#3A7ACE", col = "#78A7E4", lwd=3)
curve(dnorm(x, mean=mean(ozone_response_y), sd=sd(ozone_response_y)), add=TRUE, col="#094695", lwd=5) #Draws the actual density function
qqnorm(ozone_response_y)




# Create the Linear Model -------------------------------------------------
ozone_mult.lm <- lm(ozone_response_y~solar_predict_x1+wind_predict_x2
                    +temp_predict_x3+factor_month_predict_x4)
#Summarise the Linear Model
summary(ozone_mult.lm)
sum_ozonelm <- summary(ozone_mult.lm); print(sum_ozonelm)















# Forecast the Data -------------------------------------------------------
#Values to forecast
#REMEMBER THAT MONTH MUST BE A FACTOR!!!!
fcastvals <- data.frame(solar_predict_x1=180,
                        wind_predict_x2=12,
                        temp_predict_x3=78,
                        factor_month_predict_x4="6")

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
     xlab = "log(ozone)", border = "#ff86d3", col = "#a6bbff", breaks = 15 ) #Prob chooses probability of frequency for the histogram.
mtext(" There is a 95% probability that the mean value of the log(ozone) (y-value)is contained therein", font = 2)
curve(dnorm(x, mean=mean(mean_conf_y), sd=sd(possibleyvals_conf)), add=TRUE, col="#cf70ff", lwd=5) #Draws the actual density function
#lines(density(possibleyvals_conf), col='purple', lwd=2) #Draws the observed density function
abline(v=upr_conf, col='#cf70ff', lwd=5)  
abline(v=lwr_conf, col='#cf70ff', lwd=5)
abline(v=mean_conf_y, lwd=2, lty='dotted')  
mtext(print(round(mean_conf_y),2), 1:0, font = 2 )








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

    # Plot the Prediction Interval --------------------------------------------
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
     xlab = "Log(Ozone)", border = "#ff9c80", col = "#fff9ac", breaks = 15
) #Prob chooses probability of frequency for the histogram.
mtext(" There is a 95% probability that the log(ozone) (y-value)is contained therein", font = 2)
curve(dnorm(x, mean=mean(mean_pred_y),
            sd=sd(possibleyvals_pred)),
      add=TRUE, col="#ff9c80", lwd=5
) #Draws the actual density function
#lines(density(possibleyvals_pred), col='purple', lwd=2) #Draws the observed density function
abline(v=upr_pred, col='#ffd296', lwd=5)  
abline(v=lwr_pred, col='#ffd296', lwd=5)
abline(v=mean_pred_y, lwd=2, lty='dotted')  
mtext(print(round(mean_pred_y),2), 1:0, font = 1 )





# Model Diagnostics -------------------------------------------------------
plot(ozone_mult.lm)
    #Is the trend of the sqrt(residuals) significant?
        rootstdresid <- sqrt(abs(rstandard(ozone_mult.lm)))
        fittedozone <- fitted(ozone_mult.lm)
        rootstdresid.lm <- lm( (  rootstdresid ) ~ (  fittedozone  )  )
        sumrootresidlm <- summary(rootstdresid.lm); print(sumrootresidlm)
        plot(y = rootstdresid, x = fittedozone, xlab = "sqrt(| residual  |)")
        abline(rootstdresid.lm)
        
        if(sumrootresidlm$coefficients[8]<0.01
        ){
          print("Their is evidence for a linear correlation at a 99% significance level")
          }else(print("Their is not enough evidence to rule out a linear correlation at a 99% confidence level"))

# Variable Selection -------------
  #Create a Vector of P-Values
  sum_ozonelm$coefficients ##The ozone coefficients
  sum_ozonelm$coefficients[25:32] ##The ozone coefficient p-values
  
  pvalcoef <- sum_ozonelm$coefficients[25:32]
  coefnames <- c("Intercept", "Solar", "Wind", "Temp", "June", "July", "August", "September")
  
  names(pvalcoef) <- coefnames; print(pvalcoef)

    # Remove First Variable ---------------------------------------------------
    remainingpval <- pvalcoef[2:4]; print(remainingpval)  
    monthpval <- pvalcoef[5:8]; print(monthpval)
    
 
    if(
      min(monthpval)>max(remainingpval)
    ){
      print("Remove the Months variable") 
    }else(
      print("Do Not Remove the Months Variable")
    )
        #step(ozone_mult.lm, direction = 'backward', trace = TRUE)
    
           #USe an ANOVA to determine whether to remove the months variable------------------
              #H0: The Linear Regressions are the same, there's no difference, (i.e. feel free to pull months out)
              #Ha: The linear Regressions are different, don't remove the variable
              
              #p-value
              #TO reject the Null hypothesis we would need a p-value less than 0.05
              #A low confidence level, say 70% would mean 
              #we are ready to say that the variable is important when it probably isn't
              #A strict confidence level, say 99.99% would mean we would be unlikely to determine a variable valuable.
              
              #This is the same as in the backwards elimination method
              #When we use the typical backwards elimination, a low confidence level (e.g. 70%)
              #Means we won't remove many variables (even if they are meaningless)
              #A high confidence level means we are very ready to remove variables (i.e. if there p-value is too big). 
              
              # Test Statistic
              g1 <- lm(ozone_response_y~solar_predict_x1
                       + wind_predict_x2
                       + temp_predict_x3
                       + factor_month_predict_x4)
              g2 <- lm(ozone_response_y~solar_predict_x1
                       + wind_predict_x2
                       + temp_predict_x3)
              anova_month <- anova(g1,g2)
              anova_month$`Pr(>F)`[2]
              
              #Conclusion
              if(
                anova_month$`Pr(>F)`[2]>0.05
              ){
                print("The p-value is too big, remove the variable")
              }else(
                print("The p-value is small, keep the variable")
              )
              
                    #Remove Months VAriable

      # Recreate the Linear Regression ------------------------------------------
        ozone_mult.lm <- lm(ozone_response_y~solar_predict_x1+wind_predict_x2+temp_predict_x3)
        sum_ozonelm <- summary(ozone_mult.lm)
        
        #Re-Create the p-value vector
        sum_ozonelm$coefficients ##The ozone coefficients
        sum_ozonelm$coefficients[13:16] ##The ozone coefficient p-values
        
        pvalcoef <- sum_ozonelm$coefficients[13:16]; print(pvalcoef)
        coefnames <- c("Intercept", "Wind", "Temp", "Solar")
        
        names(pvalcoef) <- coefnames; print(sum_ozonelm); print(pvalcoef)
        
        


    # Remove Second Variable ---------------------------------------------------

   
      
      
      
      format(pvalcoef, scientific = FALSE)
      max(remainingpval)  #This corresponds to wind
    # Recreate the Linear Regression ------------------------------------------
ozone_mult.lm <- lm( (ozone_response_y) ~ (wind_predict_x2) + (temp_predict_x3) )
sum_ozonelm <- summary(ozone_mult.lm); print(sum_ozonelm)





















# Forecast the 'best group' Data -------------------------------------------------------
#Values to forecast
#REMEMBER THAT MONTH MUST BE A FACTOR!!!!
fcastvals <- data.frame(solar_predict_x1=180,
                        wind_predict_x2=12,
                        temp_predict_x3=78)

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
         xlab = "log(ozone)", border = "#9da4ff", col = "#9dfff8", breaks = 15 ) #Prob chooses probability of frequency for the histogram.
    mtext(" There is a 95% probability that the mean value of the log(ozone) (y-value)is contained therein", font = 2)
    curve(dnorm(x, mean=mean(mean_conf_y), sd=sd(possibleyvals_conf)), add=TRUE, col="#6dc979", lwd=3) #Draws the actual density function
    #lines(density(possibleyvals_conf), col='purple', lwd=2) #Draws the observed density function
    
    abline(v=upr_conf, col='#9da4ff', lwd=5)  
    abline(v=lwr_conf, col='#9da4ff', lwd=5)
    abline(v=mean_conf_y, lwd=2, lty='dotted')  
    mtext(print(round(mean_conf_y),2), 1:0, font = 2 )
    
    ##Plot the old Prediction graphics
      curve(
        dnorm(
          x, mean=mean(3.22), sd=0.1729315
          ), add=TRUE,
        col="#cf70ff",
        lwd=3, lty=5) #Draws the actual density function
      #(from the old regression prediction)
      #abline(v=2.8, col='#cf70ff', lwd=3, lty=5)  
      #abline(v=3.4, col='#cf70ff', lwd=3, lty=5)
    
    
    
    
    
    
    
    
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
    
    # Plot the Prediction Interval --------------------------------------------
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
         xlab = "Log(Ozone)", border = "#cc6287", col = "#ffad8f", breaks = 15
    ) #Prob chooses probability of frequency for the histogram.
    mtext(" There is a 95% probability that the log(ozone) (y-value)is contained therein", font = 2)
    curve(dnorm(x, mean=mean(mean_pred_y),
                sd=sd(possibleyvals_pred)),
          add=TRUE, col="#cc6287", lwd=5
    ) #Draws the actual density function
    #lines(density(possibleyvals_pred), col='purple', lwd=2) #Draws the observed density function
    abline(v=upr_pred, col='#ffad8f', lwd=5)  
    abline(v=lwr_pred, col='#ffad8f', lwd=5)
    abline(v=mean_pred_y, lwd=2, lty='dotted')  
    mtext(print(round(mean_pred_y),2), 1:0, font = 1 )
    
    ##Plot the old Prediction graphics
    curve(
      dnorm(
        x, mean=mean(3.22), sd=0.5466998
      ), add=TRUE,
      col="#ff9c80",
      lwd=3, lty=5) #Draws the actual density function
    #(from the old regression prediction)
    abline(v=2.154003, col='#ffd296', lwd=3, lty=5)  
    abline(v=4.297027, col='#ffd296', lwd=3, lty=5)
    
    




