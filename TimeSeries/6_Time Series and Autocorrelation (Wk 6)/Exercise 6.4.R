# Load Packages -----------------------------------------------------------
library(scales)
library(EnvStats)

# Create Variables --------------------------------------------------------
n <- 1200 #Sample Size
Z <- rnorm(n, mean=0, sd=4.58) #Zero-mean stationary series
  Z <- runif(n)
t <- seq(1:n+100) #Time values

#Create the Time Series
X <- 5+2*t+Z


#Test acf function
X_xts <- as.xts(x = X, order.by = seq(from=as.Date("2014-01-01"), length.out = length(X), by=1))
acf(X_xts, type = "covariance")$acf[1]
cov(merge(x = X_xts, lag(X_xts, k = 11), join="inner"))[1,1]
#observe that the acf function uses a different method to calculate the covariance than the cov function as well.


#Find the Mean value
mean(X)  #So the Mean Value of the Time series appears to be around the sample size, 
#but lets get a better idea with a prediction interval.

#Test Loop
mean_X_vector <- c()
while(length(mean_X_vector)<n){
  
  Z <- rnorm(n, mean=0, sd=4.5) #Zero-mean stationary series 
  X <- 5+2*t+Z
  mean_X_vector <- c(mean_X_vector, mean(X))
  
}
hist(mean_X_vector)
predIntNorm(mean_X_vector, conf.level = 0.00000001)

X <- 5+2*t


#So after running this, we can be 99.999999% certain that the mean of the time series is n+1+5


# Find the Variance -------------------------------------------------------    

var(X)  

#Test Loop
var_X_vector <- c()
while(length(var_X_vector)<n){
  
  Z <- rnorm(n, mean=0, sd=4.5) #Zero-mean stationary series 
  X <- 5+2*t+Z
  var_X_vector <- c(var_X_vector, var(X))
  
}
hist(var_X_vector)
predIntNorm(var_X_vector, conf.level = 0.00000001)

#So after the  running this, we can atleast see that the variation of X is dependent on n

#Find the Covariance
k <- 1 #Set the Lag
n <- 10000 #samplesize
R <- 30 #Number of tests for sampling distribution
t <- seq(1:n+100)

xmean <- mean(X)

#Test Loop
covar_X_vector <- c()
calc_covar_Txtbook <- sum((X-xmean)*(X-xmean))/n; calc_covar_Txtbook
calc_covar_simplify <- (1/3/(n-1))*(n^3-4*n-3) # I beleive this is what the formula simplifies to, If not, just remove this
covar_X_txtbookcalc_vector <- c()
covar_X_simplifycalc_vector <- c()
actualzmean_vector <- c()











while(length(covar_X_vector)<R){
  
  Z <- rnorm(n, mean=0, sd=2) #Zero-mean stationary series 
  X <- 5+2*t+Z
  
  covar_X_vector <- c(covar_X_vector, acf(X, type = "covariance")$acf[2])
  print(percent(round(length(covar_X_vector)/R,2)))
  
}
hist(covar_X_vector)
covarXpred <- predIntNorm(covar_X_vector, conf.level = 0.1)








xmean <- 6+n
xtkmean <- 6+n+k
xt <- X
xtk <- 5+2*(t+k)+Z

covarXpred$parameters[1]
calc_covar_Txtbook <- sum((xt-xmean)*(xtk-xmean))/n; calc_covar_Txtbook
calc_covar_Txtbook <- sum((xt-xmean)*(xtk-xmean))/n; calc_covar_Txtbook




calc_covar_simplify <- (1/3/(n-1))*(n^3-4*n-3) # I beleive this is what the formula simplifies to, If not, just remove this



acf(xt, type = "covariance")$acf[2]
acf(X, type = "covariance")$acf[2]
    
    
    
    calc_covar_Txtbook
    calc_covar_simplify
    covarXpred$interval$limits[1]
    
    
    
    
    #Compare R formula and textbook Formula
    if((calc_covar_Txtbook-covarXpred$interval$limits[1])*100/covarXpred$interval$limits[1]<5){
      
      print("The Textbook formula for Autocovariance, is the same as the Autocovariance observed by the acf() function, the formula is correct")
    }else(
      
      print("The formula is wrong")
    )
    
    
    
    
    #Compare R formula and Simplified
    
    if((calc_covar_simplify-covarXpred$interval$limits[1])*100/covarXpred$interval$limits[1]<5){
      
      print("The simplified formula for Autocovariance, is the same as the Autocovariance observed by the acf() function, the formula is correct")
    }else(
      
      print("The formula is wrong")
    )
    
    
    
    #Compare Textbook Formula and simplification
    
    if(abs((calc_covar_Txtbook-calc_covar_simplify)*100/calc_covar_Txtbook)<5){
      
      print("The Simplification seems to be correct")
    }else(
      
      print("The simplification is different to the textbook formula")
    )
    
    
    plot.ts(X, col="purple", lwd=3, main="Possible Time series Plot")
    abline(a = 5, b=2, col="red", lwd=2)
    mtext("X=5+5t+z; z~N(0,2), n=20",padj = 5)
    X
    
    
    
    
    
    # Abandoned T-Test for delta mean value -----------------------------------
    
    # delta_calctxt_obs_vector <- (covar_X_vector-covar_X_txtbookcalc_vector)/covar_X_vector
    # delta_calcsimp_obs_vector <- (covar_X_vector-covar_X_simplifycalc_vector)/covar_X_vector
    # delta_calcsimp_calctxt_vector <- (covar_X_txtbookcalc_vector-covar_X_simplifycalc_vector)/covar_X_txtbookcalc
    # 
    
    #Compare the R Autocovariance with the Textbook Autocovariance:
    
    # if(t.test(delta_calctxt_obs_vector, conf.level = 0.01)$conf.int[1]<=0 & 0<=t.test(delta_calctxt_obs_vector, conf.level = 0.01)$conf.int[2]){
    #   print("For the given sample size, the auto covariance formula from the textbook is equal to the autocovariance calculated by R at a 99% significance level")
    # }else if(0<t.test(delta_calctxt_obs_vector, conf.level = 0.01)$conf.int[1]){
    #   print("For the given sample size, the autocovariance calculated by R is larger than the textbook autocovariance.")
    # }else(
    #   print("For the given sample size, the autocovariance calculated by R is smaller than the textbook autocovariance.")
    # )
    # 
    # #Compare the R Autocovariance with the Simplified Autocovariance:
    # 
    # if(t.test(delta_calcsimp_obs_vector, conf.level = 0.01)$conf.int[1]<=0 & 0<=t.test(delta_calctxt_obs_vector, conf.level = 0.01)$conf.int[2]){
    #   print("For the given sample size, the simplified auto covariance formula is equal to the autocovariance calculated by R at a 99% significance level")
    # }else if(0<t.test(delta_calcsimp_obs_vector, conf.level = 0.01)$conf.int[1]){
    #   print("For the given sample size, the autocovariance calculated by R is larger than the simplified formula autocovariance.")
    # }else(
    #   print("For the given sample size, the autocovariance calculated by R is smaller than the simplifed formula autocovariance.")
    # )
    # 
    # 
    # #Compare the R Autocovariance with the Textbook Autocovariance:
    # 
    # if(t.test(delta_calcsimp_calctxt_vector, conf.level = 0.01)$conf.int[1]<=0 & 0<=t.test(delta_calctxt_obs_vector, conf.level = 0.01)$conf.int[2]){
    #   print("For the given sample size, the auto covariance formula from the simplified is equal to the autocovariance calculated by R at a 99% significance level")
    # }else if(0<t.test(delta_calcsimp_calctxt_vector, conf.level = 0.01)$conf.int[1]){
    #   print("For the given sample size, the autocovariance calculated by the textook formula is larger than the simplified formula autocovariance.")
    # }else(
    #   print("For the given sample size, the autocovariance calculated by Textbook formula is smaller than the simplified autocovariance.")
    # )
    # 
    
    #   
    # 
    # #Test Loop
    # mean_X_vector <- c()
    # while(length(mean_X_vector)<n){
    #   
    #   Z <- rnorm(n, mean=0, sd=4.5) #Zero-mean stationary series 
    #   X <- 5+2*t+Z
    #   mean_X_vector <- c(mean_X_vector, mean(X))
    #   
    # }
    # hist(mean_X_vector)
    # predIntNorm(mean_X_vector, conf.level = 0.00000001)
    # 
    # #So after running this, we can be 99.999999% certain that the mean of the time series is n+1+5
    # 
    # 
    # # Find the Variance -------------------------------------------------------    
    # 
    # var(X)  
    # 
    # #Test Loop
    # var_X_vector <- c()
    # while(length(var_X_vector)<n){
    #   
    #   Z <- rnorm(n, mean=0, sd=4.5) #Zero-mean stationary series 
    #   X <- 5+2*t+Z
    #   var_X_vector <- c(var_X_vector, var(X))
    #   
    # }
    # hist(var_X_vector)
    # predIntNorm(var_X_vector, conf.level = 0.00000001)
    # 
    # #So after the  running this, we can atleast see that the variation of X is dependent on n
    # 
    # #Find the Covariance
    # k <- 1 #Set the Lag
    # n <- 1000 #samplesize
    # R <- 30 #Number of tests for sampling distribution
    # t <- seq(1:n+100)
    # 
    # #Test Loop
    # covar_X_vector <- c()
    # calc_covar_Txtbook <- sum((xt-xmean)*(xtk-xmean))/n; calc_covar_Txtbook
    # calc_covar_simplify <- (1/3/(n-1))*(n^3-4*n-3) # I beleive this is what the formula simplifies to, If not, just remove this
    # covar_X_txtbookcalc_vector <- c()
    # covar_X_simplifycalc_vector <- c()
    # actualzmean_vector <- c()
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # while(length(covar_X_vector)<R){
    #   
    #   Z <- rnorm(n, mean=0, sd=4.5) #Zero-mean stationary series 
    #   X <- 5+2*t+Z
    #   xt <- X
    #   xtk <- 5+2*(t+k)+Z
    #   xmean <- 6+n
    #   
    #   covar_X_vector <- c(covar_X_vector, acf(X, type = "covariance")$acf[2])
    #   covar_X_txtbookcalc_vector <- c(covar_X_txtbookcalc_vector, calc_covar_Txtbook)
    #   covar_X_simplifycalc_vector <- c(covar_X_simplifycalc_vector, calc_covar_simplify)
    #   actualzmean_vector <- c(actualzmean_vector, mean(Z))
    #   
    #   print(percent(round(length(covar_X_vector)/R,2)))
    #   
    # }
    # hist(covar_X_vector)
    # covarXpred <- predIntNorm(covar_X_vector, conf.level = 0.1)
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # xmean <- 6+n
    # xtkmean <- 6+n+k
    # xt <- X
    # xtk <- 5+2*(t+k)+Z
    # 
    # covarXpred$parameters[1]
    # calc_covar_Txtbook <- sum((xt-xmean)*(xtk-xmean))/n; calc_covar_Txtbook
    # calc_covar_Txtbook <- sum((xt-xmean)*(xtk-xmean))/n; calc_covar_Txtbook
    # 
    # 
    # 
    # 
    # calc_covar_simplify <- (1/3/(n-1))*(n^3-4*n-3) # I beleive this is what the formula simplifies to, If not, just remove this
    # 
    # 
    # 
    # acf(xt, type = "covariance")$acf[2]
    # acf(X, type = "covariance")$acf[2]
    
    
    
    
    
    
    
    
    
    # Abandoned Prediction Interval Test --------------------------------------
    
    
    #   print("The Calculated Autocovariance, is the same as the observed Autocovariance, the formula is correct")
    # }else(
    #   
    #   print("The formula is wrong")
    # )
    
    
    
    
    
    
    
    
    