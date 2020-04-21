library(EnvStats)


# Create the Variables ----------------------------------------------------

n <- 100 #Sample Size
r <- 500 #Value Range
R <- 5000 #Repetitions, no. of observations in data frame
sm <- 30  #size of time series mean value sampling distribution

Z <- rnorm(n, mean=0, sd=4.58) #Zero-mean stationary series
Z <- runif(n)
t <- seq(1:n+100) #Time values



# Start the Loop ----------------------------------------------------------
observed_mean_vector <- c()
calculated_mean_vector<- c()

while(length(observed_mean_vector)<R){


Z <- runif(n, min = -r, max = r)
a <- runif(1, min = -r, max = r)
b <- runif(1, min = -r, max = r)

calcmux <- a+b*(n+1)/n
calculated_mean_vector <- c(calculated_mean_vector, calcmux)
      
      xmeanvector <- c()
      while(length(xmeanvector)<sm){
        Z <- runif(n, min = -r, max = r)
        X <- a+b*t+Z
        xmeanvector <- c(xmeanvector, mean(X))
        
      }

observed_mean_vector <- c(observed_mean_vector, mean(xmeanvector))    
}


observedvscalculatedmean <- data.frame("Observed Time Series Mean"=observed_mean_vector, "Mean Value Function"=calculated_mean_vector)

plot(y = observed_mean_vector, x = calculated_mean_vector, xlab = "Mean Value Function", ylab="Observed Time Series Mean Value", col = "tan")
abline(lm(observed_mean_vector~calculated_mean_vector), col="blue", lwd=3)
plot(lm(observed_mean_vector~calculated_mean_vector))

