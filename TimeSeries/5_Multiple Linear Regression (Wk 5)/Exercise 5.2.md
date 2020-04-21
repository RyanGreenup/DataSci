# Load Iris Data


```r
irisTB <- as_tibble(iris)
```

Take out the Response Variable


```r
irisTB <- irisTB[-5]
```

Perform Principal Coponent Analysis


```r
pca.mod <- prcomp(irisTB, scale = TRUE)
```

The loading vectors correspond to the `rotation` matrix in the model:


```r
print(pca.mod$rotation, digits = 2)
```

```
##                PC1    PC2   PC3   PC4
## Sepal.Length  0.52 -0.377  0.72  0.26
## Sepal.Width  -0.27 -0.923 -0.24 -0.12
## Petal.Length  0.58 -0.024 -0.14 -0.80
## Petal.Width   0.56 -0.067 -0.63  0.52
```

This should provide that the loading vectors are:


```r
##                PC1    PC2   PC3   PC4
## Sepal.Length  0.52 -0.377  0.72  0.26
## Sepal.Width  -0.27 -0.923 -0.24 -0.12
## Petal.Length  0.58 -0.024 -0.14 -0.80
## Petal.Width   0.56 -0.067 -0.63  0.52
```

Which Corresponds mathematically to:
$$
\begin{aligned}
&\textbf{PC1:} \\
Z_{1} &= \begin{bmatrix} SL_i & SW_i & PL_i & PW_i \end{bmatrix} \times \begin{bmatrix} 0.52 \\ -0.26 \\ 0.58 \\ 0.56 \end{bmatrix} \\
& = 0.52 \cdot SL_i - 0.26 \cdot SW_i +0.58 \cdot PL + 0.56 \cdot PW \\
\ \\
\ \\

&\textbf{PC2:} \\
Z_{2} &= \begin{bmatrix} SL_i & SW_i & PL_i & PW_i \end{bmatrix} \times \begin{bmatrix} -0.37 \\ -0.92 \\ -0.02 \\ -0.07 \end{bmatrix} \\
&= -0.377 \cdot SL_i - 0.92 \cdot SW_i -0.02  \cdot PL -0.07 \cdot PW
\end{aligned}
$$


```r
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
```


 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 

```r
#Import Data Set
# library(readr)
# Ozone_1_ <- read_csv("C:...Ozone(1).csv")
Ozone_1_ <- read_csv("../../0DataSets/Ozone(1).csv")
```

```
## Warning: Missing column names filled in: 'X1' [1]
```

```
## Parsed with column specification:
## cols(
##   X1 = col_double(),
##   solar = col_double(),
##   wind = col_double(),
##   temp = col_double(),
##   month = col_double(),
##   day = col_double(),
##   airoz = col_double()
## )
```

```r
# View(Ozone_1_)
#Assign Variables

ozone_response_y <- Ozone_1_$airoz
solar_predict_x1 <- Ozone_1_$solar
wind_predict_x2 <- Ozone_1_$wind
temp_predict_x3 <- Ozone_1_$temp
month_predict_x4 <- Ozone_1_$month
factor_month_predict_x4 <- factor(month_predict_x4)
length(ozone_response_y)
```

```
## [1] 111
```

```r
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
```

```
## 
## Call:
## lm(formula = ozone_response_y ~ solar_predict_x1 + wind_predict_x2 + 
##     temp_predict_x3 + factor_month_predict_x4)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -2.12662 -0.28197 -0.02742  0.28834  1.16785 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)    
## (Intercept)              -0.3392540  0.6455802  -0.526 0.600363    
## solar_predict_x1          0.0024410  0.0005854   4.170 6.36e-05 ***
## wind_predict_x2          -0.0592068  0.0163260  -3.627 0.000449 ***
## temp_predict_x3           0.0511975  0.0084272   6.075 2.10e-08 ***
## factor_month_predict_x46 -0.1575432  0.2256326  -0.698 0.486607    
## factor_month_predict_x47 -0.0952694  0.1936372  -0.492 0.623767    
## factor_month_predict_x48 -0.0228939  0.2014990  -0.114 0.909762    
## factor_month_predict_x49 -0.1929045  0.1646140  -1.172 0.243957    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5125 on 103 degrees of freedom
## Multiple R-squared:  0.6719,	Adjusted R-squared:  0.6496 
## F-statistic: 30.13 on 7 and 103 DF,  p-value: < 2.2e-16
```

```r
sum_ozonelm <- summary(ozone_mult.lm); print(sum_ozonelm)
```

```
## 
## Call:
## lm(formula = ozone_response_y ~ solar_predict_x1 + wind_predict_x2 + 
##     temp_predict_x3 + factor_month_predict_x4)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -2.12662 -0.28197 -0.02742  0.28834  1.16785 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)    
## (Intercept)              -0.3392540  0.6455802  -0.526 0.600363    
## solar_predict_x1          0.0024410  0.0005854   4.170 6.36e-05 ***
## wind_predict_x2          -0.0592068  0.0163260  -3.627 0.000449 ***
## temp_predict_x3           0.0511975  0.0084272   6.075 2.10e-08 ***
## factor_month_predict_x46 -0.1575432  0.2256326  -0.698 0.486607    
## factor_month_predict_x47 -0.0952694  0.1936372  -0.492 0.623767    
## factor_month_predict_x48 -0.0228939  0.2014990  -0.114 0.909762    
## factor_month_predict_x49 -0.1929045  0.1646140  -1.172 0.243957    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5125 on 103 degrees of freedom
## Multiple R-squared:  0.6719,	Adjusted R-squared:  0.6496 
## F-statistic: 30.13 on 7 and 103 DF,  p-value: < 2.2e-16
```

```r
# Forecast the Data -------------------------------------------------------
#Values to forecast
#REMEMBER THAT MONTH MUST BE A FACTOR!!!!
fcastvals <- data.frame(solar_predict_x1=180,
                        wind_predict_x2=12,
                        temp_predict_x3=78,
                        factor_month_predict_x4="6")

  # Confidence Interval -----------------------------------------------------
predict(ozone_mult.lm, fcastvals, interval = 'confidence')
```

```
##        fit      lwr      upr
## 1 3.225515 2.886576 3.564455
```

```r
  #Prediction Interval -----------------------------------------------------
predict(ozone_mult.lm, fcastvals, interval = 'predict')
```

```
##        fit      lwr      upr
## 1 3.225515 2.154003 4.297027
```

```r
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
```

```
## [1] 3
```

```r
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
```

```
## [1] 3
```

```r
# Model Diagnostics -------------------------------------------------------
plot(ozone_mult.lm)
    #Is the trend of the sqrt(residuals) significant?
        rootstdresid <- sqrt(abs(rstandard(ozone_mult.lm)))
        fittedozone <- fitted(ozone_mult.lm)
        rootstdresid.lm <- lm( (  rootstdresid ) ~ (  fittedozone  )  )
        sumrootresidlm <- summary(rootstdresid.lm); print(sumrootresidlm)
```

```
## 
## Call:
## lm(formula = (rootstdresid) ~ (fittedozone))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.77742 -0.19131 -0.00197  0.22792  1.05355 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.41178    0.15773   8.950 1.05e-14 ***
## fittedozone -0.17920    0.04522  -3.963 0.000133 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3366 on 109 degrees of freedom
## Multiple R-squared:  0.1259,	Adjusted R-squared:  0.1179 
## F-statistic:  15.7 on 1 and 109 DF,  p-value: 0.0001325
```

```r
        plot(y = rootstdresid, x = fittedozone, xlab = "sqrt(| residual  |)")
        abline(rootstdresid.lm)
        
        if(sumrootresidlm$coefficients[8]<0.01
        ){
          print("Their is evidence for a linear correlation at a 99% significance level")
          }else(print("Their is not enough evidence to rule out a linear correlation at a 99% confidence level"))
```

```
## [1] "Their is evidence for a linear correlation at a 99% significance level"
```

```r
# Variable Selection -------------
  #Create a Vector of P-Values
  sum_ozonelm$coefficients ##The ozone coefficients
```

```
##                              Estimate  Std. Error    t value     Pr(>|t|)
## (Intercept)              -0.339253986 0.645580210 -0.5255025 6.003634e-01
## solar_predict_x1          0.002441041 0.000585358  4.1701682 6.360302e-05
## wind_predict_x2          -0.059206812 0.016326002 -3.6265347 4.486047e-04
## temp_predict_x3           0.051197521 0.008427244  6.0752393 2.104231e-08
## factor_month_predict_x46 -0.157543201 0.225632594 -0.6982289 4.866074e-01
## factor_month_predict_x47 -0.095269425 0.193637175 -0.4919997 6.237666e-01
## factor_month_predict_x48 -0.022893909 0.201499037 -0.1136180 9.097618e-01
## factor_month_predict_x49 -0.192904461 0.164613987 -1.1718595 2.439570e-01
```

```r
  sum_ozonelm$coefficients[25:32] ##The ozone coefficient p-values
```

```
## [1] 6.003634e-01 6.360302e-05 4.486047e-04 2.104231e-08 4.866074e-01 6.237666e-01 9.097618e-01 2.439570e-01
```

```r
  pvalcoef <- sum_ozonelm$coefficients[25:32]
  coefnames <- c("Intercept", "Solar", "Wind", "Temp", "June", "July", "August", "September")
  
  names(pvalcoef) <- coefnames; print(pvalcoef)
```

```
##    Intercept        Solar         Wind         Temp         June         July       August    September 
## 6.003634e-01 6.360302e-05 4.486047e-04 2.104231e-08 4.866074e-01 6.237666e-01 9.097618e-01 2.439570e-01
```

```r
    # Remove First Variable ---------------------------------------------------
    remainingpval <- pvalcoef[2:4]; print(remainingpval)  
```

```
##        Solar         Wind         Temp 
## 6.360302e-05 4.486047e-04 2.104231e-08
```

```r
    monthpval <- pvalcoef[5:8]; print(monthpval)
```

```
##      June      July    August September 
## 0.4866074 0.6237666 0.9097618 0.2439570
```

```r
    if(
      min(monthpval)>max(remainingpval)
    ){
      print("Remove the Months variable") 
    }else(
      print("Do Not Remove the Months Variable")
    )
```

```
## [1] "Remove the Months variable"
```

```r
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
```

```
## [1] 0.6722011
```

```r
              #Conclusion
              if(
                anova_month$`Pr(>F)`[2]>0.05
              ){
                print("The p-value is too big, remove the variable")
              }else(
                print("The p-value is small, keep the variable")
              )
```

```
## [1] "The p-value is too big, remove the variable"
```

```r
                    #Remove Months VAriable

      # Recreate the Linear Regression ------------------------------------------
        ozone_mult.lm <- lm(ozone_response_y~solar_predict_x1+wind_predict_x2+temp_predict_x3)
        sum_ozonelm <- summary(ozone_mult.lm)
        
        #Re-Create the p-value vector
        sum_ozonelm$coefficients ##The ozone coefficients
```

```
##                      Estimate   Std. Error    t value     Pr(>|t|)
## (Intercept)      -0.262132313 0.5535668608 -0.4735332 6.367975e-01
## solar_predict_x1  0.002515177 0.0005567301  4.5177673 1.616797e-05
## wind_predict_x2  -0.061562470 0.0157129654 -3.9179409 1.576806e-04
## temp_predict_x3   0.049171124 0.0060875025  8.0773888 1.069103e-12
```

```r
        sum_ozonelm$coefficients[13:16] ##The ozone coefficient p-values
```

```
## [1] 6.367975e-01 1.616797e-05 1.576806e-04 1.069103e-12
```

```r
        pvalcoef <- sum_ozonelm$coefficients[13:16]; print(pvalcoef)
```

```
## [1] 6.367975e-01 1.616797e-05 1.576806e-04 1.069103e-12
```

```r
        coefnames <- c("Intercept", "Wind", "Temp", "Solar")
        
        names(pvalcoef) <- coefnames; print(sum_ozonelm); print(pvalcoef)
```

```
## 
## Call:
## lm(formula = ozone_response_y ~ solar_predict_x1 + wind_predict_x2 + 
##     temp_predict_x3)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -2.06193 -0.29970 -0.00231  0.30756  1.23578 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)      -0.2621323  0.5535669  -0.474 0.636798    
## solar_predict_x1  0.0025152  0.0005567   4.518 1.62e-05 ***
## wind_predict_x2  -0.0615625  0.0157130  -3.918 0.000158 ***
## temp_predict_x3   0.0491711  0.0060875   8.077 1.07e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5086 on 107 degrees of freedom
## Multiple R-squared:  0.6644,	Adjusted R-squared:  0.655 
## F-statistic: 70.62 on 3 and 107 DF,  p-value: < 2.2e-16
```

```
##    Intercept         Wind         Temp        Solar 
## 6.367975e-01 1.616797e-05 1.576806e-04 1.069103e-12
```

```r
    # Remove Second Variable ---------------------------------------------------

   
      
      
      
      format(pvalcoef, scientific = FALSE)
```

```
##              Intercept                   Wind                   Temp                  Solar 
## "0.636797505736005132" "0.000016167973099241" "0.000157680597579425" "0.000000000001069103"
```

```r
      max(remainingpval)  #This corresponds to wind
```

```
## [1] 0.0004486047
```

```r
    # Recreate the Linear Regression ------------------------------------------
ozone_mult.lm <- lm( (ozone_response_y) ~ (wind_predict_x2) + (temp_predict_x3) )
sum_ozonelm <- summary(ozone_mult.lm); print(sum_ozonelm)
```

```
## 
## Call:
## lm(formula = (ozone_response_y) ~ (wind_predict_x2) + (temp_predict_x3))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -2.36759 -0.25774  0.05869  0.35009  1.16747 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     -0.38742    0.60050  -0.645 0.520189    
## wind_predict_x2 -0.05993    0.01706  -3.513 0.000649 ***
## temp_predict_x3  0.05655    0.00637   8.878 1.64e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5524 on 108 degrees of freedom
## Multiple R-squared:  0.6004,	Adjusted R-squared:  0.593 
## F-statistic: 81.14 on 2 and 108 DF,  p-value: < 2.2e-16
```

```r
# Forecast the 'best group' Data -------------------------------------------------------
#Values to forecast
#REMEMBER THAT MONTH MUST BE A FACTOR!!!!
fcastvals <- data.frame(solar_predict_x1=180,
                        wind_predict_x2=12,
                        temp_predict_x3=78)

  # Confidence Interval -----------------------------------------------------
predict(ozone_mult.lm, fcastvals, interval = 'confidence')
```

```
##        fit      lwr      upr
## 1 3.304161 3.178287 3.430034
```

```r
  #Prediction Interval -----------------------------------------------------
predict(ozone_mult.lm, fcastvals, interval = 'predict')
```

```
##        fit      lwr      upr
## 1 3.304161 2.202027 4.406295
```

```r
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
```

```
## [1] 3
```

```r
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
```

```
## [1] 3
```

```r
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
```

