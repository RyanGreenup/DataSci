# Preamble ----------------------------------------------------------------

  # set working directory here (file must be sourced, i.e. run with ctr+shift+enter NOT line-by-line)
  this.dir <- dirname(parent.frame(2)$ofile) # frame(3) also works.
  setwd(this.dir)
  
  #Import dataset and assign variables
  require(readr)
  ozone.csv <- read_csv("Ozone(1).csv")
  response_ozone_y <- ozone.csv$airoz
  predict_solar_x1 <- ozone.csv$solar
  predict_wind_x2 <- ozone.csv$wind
  predict_temp_x3 <- ozone.csv$temp
  

# Relative CSV Import fallback --------------------------------------------

  
  # # Import relevant Packages
  #   #install.packages("EnvStats")
  # library(EnvStats)
  # 
  #   #install.packages("plotly")
  # library(plotly)
  # 
  # #Import relvant Data Sets
  #   #So we don't have to bother importing data sets, just create the vector in the script.
  # response_ozone_y <- c(41,  36,  12,  18,  23,  19,   8,  16,  11,  14,  18,  14,  34,   6,  30,  11,   1,  11,   4,  32,  23,  45, 115,  37,  29,  71,  39,  23,  21,  37,  20,  12,  13, 135,  49,  32,  64,  40,  77,  97,
  #                       97,  85,  10,  27,   7,  48,  35,  61,  79,  63,  16,  80, 108,  20,  52,  82,  50,  64,  59,  39,   9,  16, 122,  89, 110,  44,  28,  65,  22,  59,  23,  31,  44,  21,   9,  45, 168,  73,  76, 118,
  #                       84,  85,  96,  78,  73,  91,  47,  32,  20,  23,  21,  24,  44,  21,  28,   9,  13,  46,  18,  13,  24,  16,  13,  23,  36,   7,  14,  30,  14,  18,  20)
  # predict_solar_x1 <- c(190, 118, 149, 313, 299,  99,  19, 256, 290, 274,  65, 334, 307,  78, 322,  44,   8, 320,  25,  92,  13, 252, 223, 279, 127, 291, 323, 148, 191, 284,  37, 120, 137, 269, 248, 236, 175, 314, 276, 267,
  #                     272, 175, 264, 175,  48, 260, 274, 285, 187, 220,   7, 294, 223,  81,  82, 213, 275, 253, 254,  83,  24,  77, 255, 229, 207, 192, 273, 157,  71,  51, 115, 244, 190, 259,  36, 212, 238, 215, 203, 225,
  #                     237, 188, 167, 197, 183, 189,  95,  92, 252, 220, 230, 259, 236, 259, 238,  24, 112, 237, 224,  27, 238, 201, 238,  14, 139,  49,  20, 193, 191, 131, 223)
  # predict_wind_x2 <- c(7.4,  8.0, 12.6, 11.5,  8.6, 13.8, 20.1,  9.7,  9.2, 10.9, 13.2, 11.5, 12.0, 18.4, 11.5,  9.7,  9.7, 16.6,  9.7, 12.0, 12.0, 14.9,  5.7,  7.4,  9.7, 13.8, 11.5,  8.0, 14.9, 20.7,  9.2, 11.5,
  #                     10.3,  4.1,  9.2,  9.2,  4.6, 10.9,  5.1,  6.3,  5.7,  7.4, 14.3, 14.9, 14.3,  6.9, 10.3,  6.3,  5.1, 11.5,  6.9,  8.6,  8.0,  8.6, 12.0,  7.4,  7.4,  7.4,  9.2,  6.9, 13.8,  7.4,  4.0, 10.3,
  #                     8.0, 11.5, 11.5,  9.7, 10.3,  6.3,  7.4, 10.9, 10.3, 15.5, 14.3,  9.7,  3.4,  8.0,  9.7,  2.3,  6.3,  6.3,  6.9,  5.1,  2.8,  4.6,  7.4, 15.5, 10.9, 10.3, 10.9,  9.7, 14.9, 15.5,  6.3, 10.9,
  #                     11.5,  6.9, 13.8, 10.3, 10.3,  8.0, 12.6,  9.2, 10.3, 10.3, 16.6,  6.9, 14.3,  8.0, 11.5)
  # predict_temp_x3 <- c(67, 72, 74, 62, 65, 59, 61, 69, 66, 68, 58, 64, 66, 57, 68, 62, 59, 73, 61, 61, 67, 81, 79, 76, 82, 90, 87, 82, 77, 72, 65, 73, 76, 84, 85, 81, 83, 83, 88, 92, 92, 89, 73, 81, 80, 81, 82, 84, 87, 85, 74, 86, 85,
  #                     82, 86, 88, 86, 83, 81, 81, 81, 82, 89, 90, 90, 86, 82, 80, 77, 79, 76, 78, 78, 77, 72, 79, 81, 86, 97, 94, 96, 94, 91, 92, 93, 93, 87, 84, 80, 78, 75, 73, 81, 76, 77, 71, 71, 78, 67, 76, 68, 82, 64, 71, 81, 69,
  #                     63, 70, 75, 76, 68)
  # 
  # ozone.csv <- data.frame("Observation Number"=seq(1:111),"Solar"=predict_solar_x1, "Wind"=predict_wind_x2, "Temperature"=predict_temp_x3, "Ozone"=response_ozone_y )





#Written by Ryan G. - 17805315 - 7 August 2017 (Wk. 4)


# Create the Linear Model (With Multiple Predictors) ----------------------

Ozone_multiple.lm <- lm(response_ozone_y~predict_solar_x1+predict_wind_x2+predict_temp_x3)


  # Summarise the Linear Model ----------------------------------------------
  summary(Ozone_multiple.lm)

  
  # Print the F-statistic ---------------------------------------------------
    summary_ozone.lm <- summary(Ozone_multiple.lm)
    print(summary_ozone.lm$fstatistic[1])

  
  
  
  
  
  

# Forecast Data from the Linear Model -------------------------------------
  #Forecast Values
  fcastval <- data.frame(predict_solar_x1=184, predict_temp_x3=78, predict_wind_x2=12)
  alphaint <- 0.95
  
  # Confidence Interval -----------------------------------------------------
    predict(Ozone_multiple.lm, fcastval, level=alphaint, interval='confidence')
  
    upr_conf <- predict(Ozone_multiple.lm, fcastval, level=alphaint, interval='confidence')[3]
    lwr_conf <- predict(Ozone_multiple.lm, fcastval, level=alphaint, interval='confidence')[2]
    (upr_conf-lwr_conf)/2
      
        # Plot the Confidence Interval --------------------------------------------
                #The histogram of possible values, corresponds to a normal distribution along the y-axis
                # This normal distribution will have a mean value of the fitted y-value
                # The standard deviation will correspond to a normal distribution where the Z-value occurs at 41.9 rather than 1.96
              
                mean_conf_y <- predict(Ozone_multiple.lm, fcastval, level=alphaint, interval='confidence')[1]
                sd_conf_y <- -(upr_conf-mean_conf_y)/qnorm(0.025, mean=0, sd=1)
                
                possibleyvals_conf <- rnorm(n=1000000, mean=mean_conf_y, sd=sd_conf_y)
                
                tb <- c(seq(10:50))
                range <- c(0, lwr_conf, upr_conf, mean_conf_y*4*sd_conf_y )
                col <- findInterval(tb, range, all.inside=TRUE)
                
                col[which(col==1)] <- "firebrick1"
                
                col[which(col==2)] <- "gold"
                col[which(col==3)] <- "darkolivegreen1"
                
                
                # h_confint <- hist(possibleyvals_conf, prob = TRUE, breaks = 200, plot = FALSE)
                # cuts_confit <- cut(h_confint$breaks, c(-(1/0), lwr_conf, upr_conf, (1/0)))
                # plot(h_confint, lwd=2,
                #      main = "95% Confidence Interval of mean value of fitted y value",
                #      xlab = "Mean value of expected y-value", border = "blue", col = c("blue", "white", "blue")[cuts_confit] ) #Prob chooses probability of frequency for the histogram."
                  hist(possibleyvals_conf, breaks = 30, prob=TRUE, lwd=2,
                     main = "Confidence Interval",
                    xlab = "Mean value of expected y-value", border = "purple") #Prob chooses probability of frequency for the histogram."
                curve(dnorm(x, mean=mean_conf_y, sd=sd(possibleyvals_conf)), add=TRUE, col="purple", lwd=2) #Draws the actual density function
                #lines(density(possibleyvals_conf), col='purple', lwd=2) #Draws the observed density function
                abline(v=upr_conf, col='pink', lwd=3)  
                abline(v=lwr_conf, col='pink', lwd=3)     
                abline(v=mean_conf_y, lwd=2, lty='dotted')  
                mtext(" There is a 95% probability that the mean-value for the ozone level (y-value)is contained therein", font = 2)
                
             
                
                
                
                
        
  
  # Prediction Interval -----------------------------------------------------
    predict(Ozone_multiple.lm, fcastval, level=alphaint, interval='predict') 
    
                
                
                upr_pred <- predict(Ozone_multiple.lm, fcastval, level=alphaint, interval='predict')[3]
                lwr_pred <- predict(Ozone_multiple.lm, fcastval, level=alphaint, interval='predict')[2]
                
                # Plot the Predidence Interval --------------------------------------------
                #The histogram of possible values, corresponds to a normal distribution along the y-axis
                # This normal distribution will have a mean value of the fitted y-value
                # The standard deviation will correspond to a normal distribution where the Z-value occurs at 41.9 rather than 1.96
                
                mean_pred_y <- predict(Ozone_multiple.lm, fcastval, level=alphaint, interval='predict')[1]
                sd_pred_y <- -(upr_pred-mean_pred_y)/qnorm(0.025, mean=0, sd=1)
                
                possibleyvals_pred <- rnorm(n=100000, mean=mean_pred_y, sd=sd_pred_y)
                hist(possibleyvals_pred, prob=TRUE, lwd=2, main = "Prediction Interval",
                     xlab = "Ozone Level", border = "blue" ) #Prob chooses probability of frequency for the histogram.
                mtext(" There is a 95% probability that the ozone level (y-value)is contained therein", font = 2)
                curve(dnorm(x, mean=mean(mean_pred_y), sd=sd(possibleyvals_pred)), add=TRUE, col="blue", lwd=2) #Draws the actual density function
                #lines(density(possibleyvals_pred), col='purple', lwd=2) #Draws the observed density function
                abline(v=upr_pred, col='lightblue', lwd=3)  
                abline(v=lwr_pred, col='lightblue', lwd=3)
                abline(v=mean_pred_y, lwd=2, lty='dotted')  
                mtext(print(round(mean_pred_y),2), 1:0, font = 1 )
                
                

                

# Play with plotting the data in 3-Dimensions (NEED GPU) ------------------

  #B = matrix(response_ozone_y, predict_solar_x, predict_temp_x)
  #p <- plot_ly(z = ~B) %>% add_surface()
  #p

                
                
                

# How I was originally going to plot them, but Shadint was hard -----------
# 
#                 # Prediction Interval -----------------------------------------------------
#                 predict(Ozone_multiple.lm, fcastval, level=alphaint, interval='predict') 
#                 
#                 
#                 
#                 upr_pred <- predict(Ozone_multiple.lm, fcastval, level=alphaint, interval='predict')[3]
#                 lwr_pred <- predict(Ozone_multiple.lm, fcastval, level=alphaint, interval='predict')[2]
#                 
#                 # Plot the Predidence Interval --------------------------------------------
#                 #The histogram of possible values, corresponds to a normal distribution along the y-axis
#                 # This normal distribution will have a mean value of the fitted y-value
#                 # The standard deviation will correspond to a normal distribution where the Z-value occurs at 41.9 rather than 1.96
#                 
#                 mean_pred_y <- predict(Ozone_multiple.lm, fcastval, level=alphaint, interval='predict')[1]
#                 sd_pred_y <- -(upr_pred-mean_pred_y)/qnorm(0.025, mean=0, sd=1)
#                 
#                 possibleyvals_pred <- rnorm(n=100000, mean=mean_pred_y, sd=sd_pred_y)
#                 hist(possibleyvals_pred, prob=TRUE, lwd=2, main = "Prediction Interval", xlab = "Ozone Level" ) #Prob chooses probability of frequency for the histogram.
#                 mtext(" There is a 95% probability that the ozone level (y-value)is contained therein", font = 2)
#                 curve(dnorm(x, mean=mean(mean_pred_y), sd=sd(possibleyvals_pred)), add=TRUE, col="purple", lwd=2) #Draws the actual density function
#                 #lines(density(possibleyvals_pred), col='purple', lwd=2) #Draws the observed density function
#                 abline(v=upr_pred, col='red', lwd=3)  
#                 abline(v=lwr_pred, col='red', lwd=3)
#                 abline(v=mean_pred_y, lwd=2, lty='dotted')  
#                 mtext(print(round(mean_pred_y),2), 1:0, font = 1 )









