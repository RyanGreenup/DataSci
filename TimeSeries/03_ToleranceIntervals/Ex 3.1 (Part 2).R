#install.packages(EnvStats)
library(EnvStats)


#Create the Background Data frame

salmon_back <- rnorm(90, mean = 73*1, sd = 3)
basa_back <- rnorm(90, mean = 120*1, sd = 10)
cod_back <- rnorm(90, mean = 90*1, sd = 30)

fish_back <-  data.frame(salmon_back, basa_back, cod_back)


#Create the testing Data frame
ratio <- 0.75 # for adjusting the new data

salmon <- rnorm(30, mean = 73*ratio, sd = 3)
basa <- rnorm(30, mean = 120*ratio, sd = 10)
cod <- rnorm(30, mean = 90*ratio, sd = 30)

fish <-  data.frame(salmon, basa, cod)




#Use an if test to make sure the data is normal before running the code
alpha_normalitytest <- 0.05 #alpha level just for the test of normality


if (
  alpha_normalitytest < shapiro.test(fish$salmon)[2] & 
  alpha_normalitytest < shapiro.test(fish$basa)[2] & 
  alpha_normalitytest < shapiro.test(fish$cod)[2] &
  alpha_normalitytest < shapiro.test(fish_back$salmon)[2] & 
  alpha_normalitytest < shapiro.test(fish_back$basa)[2] & 
  alpha_normalitytest < shapiro.test(fish_back$cod)[2]  
    )  {print("FYI, There may be evidence to suggest that the data is non-normal pursuant to the Shapiro test")}
  

  #############Begin the Tolerance Interval work
  
      ####### Create the Background Tolerance Interval
  
  
  
      LTL_salmon <- tolIntNorm(fish_back$salmon, coverage = 0.95, cov.type = "content", ti.type = "lower", conf.level = 0.95, method = "exact")$interval$limits[1]
      LTL_cod <- tolIntNorm(fish_back$cod, coverage = 0.95, cov.type = "content", ti.type = "lower", conf.level = 0.95, method = "exact")$interval$limits[1]
      LTL_basa <- tolIntNorm(fish_back$basa, coverage = 0.95, cov.type = "content", ti.type = "lower", conf.level = 0.95, method = "exact")$interval$limits[1]
  
      ####### Compare each value to the data in the tolerance Interval
  
      if(
          all(fish$salmon < LTL_salmon) == FALSE &
          all(fish$cod < LTL_cod) == FALSE       &
          all(fish$basa < LTL_basa) == FALSE)
        {
    
                    print("There is no evidence to suggest, at a confidence level of 95%, that the lengths of fish are shorter")
                        
                  }else{
                          
                          print("The fish are shorter")
                                    
                                          }      
      



      



