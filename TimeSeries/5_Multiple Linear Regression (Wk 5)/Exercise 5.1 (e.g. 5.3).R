
# Preamble ----------------------------------------------------------------
  #Import Data Set
      # library(readr)
      # Ozone_1_ <- read_csv("C:/...Ozone(1).csv")
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
  
  #Plot the Model Diagnostics
  plot( ozone_mult.lm, lwd=2, col="purple", lty="twodash" )
  
  
  
  