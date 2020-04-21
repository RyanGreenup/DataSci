# Load Packages -----------------------------------------------------------
library(scales)
library(EnvStats)

# Create Variables --------------------------------------------------------


    # Purely Random Process ---------------------------------------------------
    #Any process with an autocorrelation of 0 for all values (i.e. all lag-1, lag-2, lag-3 is 0)
    # and an autocovariance of 0 for all values (i.e. all lag-1, lag-2, lag-3 is 0)
    #Additionally a white noise function has mean value of 0.
    
      # Normal Distribution -----------------------------------------------------
        Z_norm <- rnorm(30000, mean=0, sd=4) #If z is the random error, it should be normally distributed about 0.
        acf(Z)
        acf(Z, type="covariance")
        
        # Uniform Distribution -----------------------------------------------------
        Z_unif <- runif(300, max = 1, min = -1) #If z is the random error, it should be normally distributed about 0.
        acf(Z_unif)
        acf(Z_unif, type="covariance")
        mean(Z_unif)
                
                hist(Z_unif)
                
                #For fun; Observe the Normality of the sampling distribution folliwng the Central Limit Theorem
                n <- 10
                unif_mean_vector <- c()
                while(length(unif_mean_vector)<n){
                  
                  unif_mean_vector <- c(unif_mean_vector,mean(runif(n,min = -6, max = 6)))
                  print(percent(length(unif_mean_vector)/n))
                }
                
                mean(unif_mean_vector)
                hist(unif_mean_vector)
                

                
#Moving Average Process 
#######################################
n <- 800
white_noise_vector <- rnorm(100+n, mean = 0, sd=8)
Z <- 0
i <- 1
zt <- c()

while(length(zt)<n){

i <- i+1
zt <- c(zt, white_noise_vector[i]-(1/4)*white_noise_vector[i-1])
}
plot.ts(zt, ylab="zt=at-1/4at")

acf(zt, type="correlation", na.actio=na.omit, plot=TRUE)

var(zt)
    
