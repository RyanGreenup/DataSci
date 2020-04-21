
# Exercise 6.4 - Revisited ------------------------------------------------
# Ryan G. 17805315 26 August 2017


# Preamble ----------------------------------------------------------------

    # Load Packages -----------------------------------------------------------
    library(scales)
    library(EnvStats)
    library(fpp)
    library(forecast)
    
    # Create Variables --------------------------------------------------------
    n <- 1200 #Sample Size
    Z <- rnorm(n, mean=0, sd=4.58) #Zero-mean stationary series
      Z <- arima.sim(model=list(order=c(0,0,0)), n) #WN Process is zero-mean and stationary
    t <- seq(1:n) #Time values
    
    #Create the Time Series, 
    X <- 5+2*t+Z #This appears to be, simply, a linear regression
    
    
    
    

# Find the Mean Value for the Time Series ---------------------------------
    
    meanvec <- c()
    nvec <- c()
    maxsize <- 1000
    for(n in 1:maxsize){
      
      
      Z <- arima.sim(model=list(order=c(0,0,0)), n) #WN Process is zero-mean and stationary
      t <- seq(1:n) #Time values
      
      #Create the Time Series, 
      X <- 5+2*t+Z #This appears to be, simply, a linear regression
      mean(X)
      meanvec <- c(meanvec, mean(X))
      print(paste(length(meanvec)*100/maxsize, "% Complete"))
      nvec <- c(nvec, n)
    }
    
    plot(nvec,meanvec)
    
    #So the Value of the mean is heavily dependent on the sample size,
    #This isn't suprising as the mean value will be a horizontal line mid-yaxis.
    
    
    
    
    
    
    # Find the Auto-covariance ------------------------------------------------
    
    
    meanvec <- c()
    nvec <- c()
    acvfvec <- c()
    kvec <- c()
    maxlag <- 100
    maxsize <- 1000+maxlag
    for(n in maxlag+1:maxsize){
      k <- 1
      Z <- arima.sim(model=list(order=c(0,0,0)), n) #WN Process is zero-mean and stationary
      t <- seq(1:n) #Time values
      
      #Create the Time Series, 
      X <- 5+2*t+Z #This appears to be, simply, a linear regression
      
      acvfvec <- c(acvfvec, cov(X[-k], X[-n]))
      nvec <- c(nvec, n)
      kvec <- c(kvec,k)
      
      for(k in 1:maxlag){
        acvfvec <- c(acvfvec, cov(X[-k], X[-n]))
        nvec <- c(nvec, n)
        kvec <- c(kvec,k)
        
        
            }
      
      
      print(paste(round(length(acvfvec)*100/maxsize/maxlag/1.03, 2), "% Complete"))
    }
    
    
    
    lm(acvfvec~nvec+kvec)
     summary(lm(acvfvec~nvec+kvec))
     
     #This suggests that the ACVF is Mostly dependent on sample size
     
   lm(acvfvec~nvec)
   summary(   lm(acvfvec~nvec) )
    plot(nvec, acvfvec, cex=0.1,xlab="Sample Size", ylab="Autocovariance for y=5+2t+Z")
    mtext("90, 000 auto-variance values for sample-size between 0 and 1000", padj = 2)
    
    
    yvec <- c()
    xvec <- c()
    for(x in 1:10000){
      y <- (x^3-4*x-3)/3  
      yvec <- c(yvec, y)
      xvec <- c(xvec, x)
      
    }  
    points(x = yvec, type = "l", col = 2, lty = 2)
    legend(x = 2, y=3)
    
    
    
    
    
    

# Stationarity ------------------------------------------------------------

#The process is stationary if the mean and covariance do not change over time
   
diff(rollmean(X, k = 9))
    hist(diff(rollmean(X, k = 1)))
    
    # e is the vector of normal observations e.g. rnorm(n=300)
    #p Is the percentage p-value, e.g. 95%
    #T <- "X-Axis label"
    #t <- "values"#X-Axis Data-name for subltitle
    
    n <- 1200 #Sample Size
    Z <- rnorm(n, mean=0, sd=4.58) #Zero-mean stationary series
    Z <- arima.sim(model=list(order=c(0,0,0)), n, sd=0.01) #WN Process is zero-mean and stationary
    t <- seq(1:n) #Time values
    
    #Create the Time Series, 
    X <- 5+2*t+Z #This appears to be, simply, a linear regression
    
    roll_mean_diff_vector <- as.vector(diff(rollmean(X, k=1)))
    
    predplot(roll_mean_diff_vector, 99.9, T="Difference between Roling Mean Values", t="Difference between Rolling mean") 
    
    
    
    
    
    
    
plot(rollmean(X, k=1), t)    
    
adf.test(X) #Small p-values suggests data is stationary
kpss.test(X) #Small p-values suggests the data is NOT stationary
Box.test(X) #Small p-values suggest the series is stationary
    
    
    
    
    
    
    