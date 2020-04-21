#install.packages(EnvStats)
library(EnvStats)

#Include the data frame
#library(readr)
#Ozone <- read_csv("ozone.csv")



# Assign Vectorsand Create Data Frame ----------------------------------------------------------
  #I'll just recreate the data frame within R, that way the code will run on any PC without having to worry about importing a dataset.

response_ozone_y <- c(41,  36,  12,  18,  23,  19,   8,  16,  11,  14,  18,  14,  34,   6,  30,  11,   1,  11,   4,  32,  23,  45, 115,  37,  29,  71,  39,  23,  21,  37,  20,  12,  13, 135,  49,  32,  64,  40,  77,  97,
                    97,  85,  10,  27,   7,  48,  35,  61,  79,  63,  16,  80, 108,  20,  52,  82,  50,  64,  59,  39,   9,  16, 122,  89, 110,  44,  28,  65,  22,  59,  23,  31,  44,  21,   9,  45, 168,  73,  76, 118,
                    84,  85,  96,  78,  73,  91,  47,  32,  20,  23,  21,  24,  44,  21,  28,   9,  13,  46,  18,  13,  24,  16,  13,  23,  36,   7,  14,  30,  14,  18,  20)
predict_solar_x<- c(190, 118, 149, 313, 299,  99,  19, 256, 290, 274,  65, 334, 307,  78, 322,  44,   8, 320,  25,  92,  13, 252, 223, 279, 127, 291, 323, 148, 191, 284,  37, 120, 137, 269, 248, 236, 175, 314, 276, 267,
                    272, 175, 264, 175,  48, 260, 274, 285, 187, 220,   7, 294, 223,  81,  82, 213, 275, 253, 254,  83,  24,  77, 255, 229, 207, 192, 273, 157,  71,  51, 115, 244, 190, 259,  36, 212, 238, 215, 203, 225,
                    237, 188, 167, 197, 183, 189,  95,  92, 252, 220, 230, 259, 236, 259, 238,  24, 112, 237, 224,  27, 238, 201, 238,  14, 139,  49,  20, 193, 191, 131, 223)
predict_wind_x <- c(7.4,  8.0, 12.6, 11.5,  8.6, 13.8, 20.1,  9.7,  9.2, 10.9, 13.2, 11.5, 12.0, 18.4, 11.5,  9.7,  9.7, 16.6,  9.7, 12.0, 12.0, 14.9,  5.7,  7.4,  9.7, 13.8, 11.5,  8.0, 14.9, 20.7,  9.2, 11.5,
                    10.3,  4.1,  9.2,  9.2,  4.6, 10.9,  5.1,  6.3,  5.7,  7.4, 14.3, 14.9, 14.3,  6.9, 10.3,  6.3,  5.1, 11.5,  6.9,  8.6,  8.0,  8.6, 12.0,  7.4,  7.4,  7.4,  9.2,  6.9, 13.8,  7.4,  4.0, 10.3,
                    8.0, 11.5, 11.5,  9.7, 10.3,  6.3,  7.4, 10.9, 10.3, 15.5, 14.3,  9.7,  3.4,  8.0,  9.7,  2.3,  6.3,  6.3,  6.9,  5.1,  2.8,  4.6,  7.4, 15.5, 10.9, 10.3, 10.9,  9.7, 14.9, 15.5,  6.3, 10.9,
                    11.5,  6.9, 13.8, 10.3, 10.3,  8.0, 12.6,  9.2, 10.3, 10.3, 16.6,  6.9, 14.3,  8.0, 11.5)
predict_temp_x <- c(67, 72, 74, 62, 65, 59, 61, 69, 66, 68, 58, 64, 66, 57, 68, 62, 59, 73, 61, 61, 67, 81, 79, 76, 82, 90, 87, 82, 77, 72, 65, 73, 76, 84, 85, 81, 83, 83, 88, 92, 92, 89, 73, 81, 80, 81, 82, 84, 87, 85, 74, 86, 85,
                    82, 86, 88, 86, 83, 81, 81, 81, 82, 89, 90, 90, 86, 82, 80, 77, 79, 76, 78, 78, 77, 72, 79, 81, 86, 97, 94, 96, 94, 91, 92, 93, 93, 87, 84, 80, 78, 75, 73, 81, 76, 77, 71, 71, 78, 67, 76, 68, 82, 64, 71, 81, 69,
                    63, 70, 75, 76, 68)

ozone.csv <- data.frame("Observation Number"=seq(1:111),"Solar"=predict_solar_x, "Wind"=predict_wind_x, "Temperature"=predict_temp_x, "Ozone"=response_ozone_y )


# Create and Plot Linear Regression Models --------------------------------

  # Ozone Level vs Solar Radiation ------------------------------------------
  ozonevsolar.lm <- lm(response_ozone_y~predict_solar_x)
  plot(predict_solar_x, response_ozone_y, xlab ="Solar Radiation", ylab = "Ozone Level", main = "Ozone against Solar Radiation", col="red")
  abline(ozonevsolar.lm)
  
       # Print the Equation and r^2 ----------------------------------------------
  ozonevsolar_intercept <-  round(ozonevsolar.lm$coefficients[1], 2)
  ozonevsolar_slope <-  round(ozonevsolar.lm$coefficients[2], 2)
  ozonevsolar_r2 <- round((cor(predict_solar_x, response_ozone_y))^2, 2)
  
  eqozvsol <- paste0("Ozone = ", ozonevsolar_intercept, if(ozonevsolar_slope > 0){" + "}else(" ") , ozonevsolar_slope,  " Solar")
  r2ozvsol <- paste0("r^2 = ", ozonevsolar_r2 )

  mtext(eqozvsol, 3, line=-2)
  mtext(r2ozvsol, 3, line=-3)
  
  

  
  

  






  
  
  # Ozone Level vs Wind Speed -----------------------------------------------  
  ozonevwind.lm <- lm(response_ozone_y ~ predict_wind_x)
  plot(predict_wind_x, response_ozone_y, xlab="Wind Speed", ylab = "Ozone Level", main = "Ozone against Wind Speed")
  abline(ozonevwind.lm)
      # Print the Equation and r^2 ------------------------------------------
  ozonevwind_intercept <-  round(ozonevwind.lm$coefficients[1], 2)
  ozonevwind_slope <-  round(ozonevwind.lm$coefficients[2], 2)
  ozonevwind_r2 <- round((cor(predict_wind_x, response_ozone_y))^2, 2)
  
  eqozvwin <- paste0("Ozone = ", ozonevwind_intercept, if(ozonevwind_slope > 0){" + "}else(" ") , ozonevwind_slope,  " Wind")
  r2ozvwin <- paste0("r^2 = ", ozonevwind_r2 )
  
  mtext(eqozvwin, 3, line=-2)
  mtext(r2ozvwin, 3, line=-3)
  
  # Ozone Level vs Temperature Speed ----------------------------------------
  ozonevtemp.lm <- lm(response_ozone_y~predict_temp_x)
  plot(predict_temp_x, response_ozone_y, xlab="Temperature", ylab = "Ozone Level", main = "Ozone against Temperature")
  abline(ozonevtemp.lm)
      # Print the Equation and r^2 ------------------------------------------
  ozonevtemp_intercept <-  round(ozonevtemp.lm$coefficients[1], 2)
  ozonevtemp_slope <-  round(ozonevtemp.lm$coefficients[2], 2)
  ozonevtemp_r2 <- round((cor(predict_temp_x, response_ozone_y))^2, 2)
  
  eqozvwin <- paste0("Ozone = ", ozonevtemp_intercept, if(ozonevtemp_slope > 0){" + "}else(" ") , ozonevtemp_slope,  " temp")
  r2ozvwin <- paste0("r^2 = ", ozonevtemp_r2 )
  
  mtext(eqozvwin, 3, line=-3)
  mtext(r2ozvwin, 3, line=-4)
  
  
  
  
  
  
  
  
# Perform Residual Model Diagnostics --------------------------------------
  # Ozone level vs Solar Radiation ----------------------------------------
  plot(lm(response_ozone_y~predict_solar_x))
  # Ozone level vs Wind Speed ---------------------------------------------
  plot(lm(response_ozone_y~predict_wind_x))
  # Ozone level vs Temperature---------------------------------------------
  plot(lm(response_ozone_y~predict_temp_x))



















  
  
  
  
  
  

  