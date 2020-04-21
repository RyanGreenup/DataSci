# Load Packages -----------------------------------------------------------
library(scales)


# Create Variables --------------------------------------------------------
n <- 144  #no. of observations
mean_temp <- 10 #assume mean value is 10 deg celsus



time_month <- seq(1:144)
Random_error <- set.seed(654);rnorm(n = 144, mean = 0, sd = 3) #The points jump around at about 3sd eyeballing it
Long_trend <- (3/100/12)*time_month+mean_temp #Assume 3 degrees a century (due to various effects)
Observed_seasonal_change <- c(12, 10, 6, 0, -7, -11, -20, -15, -8, -5, 2, 10)




# Find Seasonal Variation -------------------------------------------------
  #By running and stopping the code, closer approximations can be found for these

A <- 11
B <- 36
C <- 2
D <- 0

sin_model_change <- A*sin(B*sample_year+C)+D
diff <- sum((sin_model_change-Observed_seasonal_change)*100/sin_model_change)/length(Observed_seasonal_change) #Average Percent Error



inc <- 0.01
max_val <- 100 #period < 12, magnitude < 25, phase < period,

#This was done with 'while' loops, but apparently "if(){}else()' is much faster, confirm this


  # A Loop ------------------------------------------------------------------
  while(diff>5|A<max_val){
    A <- A+0.01
        # B Loop ------------------------------------------------------------------
        while(B<max_val){
          B <- B+inc
            # C Loop ------------------------------------------------------------------
                while(C<6){
                  C <- C+0.01
                # D Loop ------------------------------------------------------------------
                  while(D<12){
                    D <-0# D+0.1
                      #Test
                      sin_model_change <- A*sin(B*sample_year+C)+D
                      diff <- sum((sin_model_change-Observed_seasonal_change)*100/sin_model_change)/
                        length(Observed_seasonal_change) #Average Percent Error
                      
                      
                  }
                
                
                
                
                  D <- 9}
          
          
          
          
             C <- 2}
  
    
    
    
    
      B <- 0
     print(percent(A/max_val));print(diff)
      }

print(A);print(B);print(C);print(D)

#By playing around with this wildly inefficient code, you will come to find that the seasonal variation is
#approximately something like 11*sin(36*sample_year+2)+9

#Now by tweaking the variables in response to the plot:

  seasonal_trend <- 11*sin(0.5*sample_year-80)+0
  plot.ts(seasonal_trend, ylab="Temp Difference")
  plot.ts(Observed_seasonal_change, ylab="Observed Temp Difference")

  #Thus the Seasonal Trend is:
  seasonal_trend <- 11*sin(0.5*time_month-80)+0
  
  
#Now calculate the Temperature (Without the Cyclical Trend, because time).
  
  Temp_Yt <- Long_trend+seasonal_trend
  plot.ts(Temp_Yt, ylab="Temperature", xlab="Consecutive Months")  
  








