#install.packages(EnvStats)
library(EnvStats)



#Include the data frame
#library(readr)
#Temperature <- read_csv("Temperature.csv")

#Use the attach command to attach the data frame to R's variable search, i.e. enable tabbing.
attach(Temperature)

#Create the linear Model of the Variables
x_mintemp <- Temperature$`Min Temperature`
y_maxtemp <- Temperature$`Max Temperature`
templm <- lm(y_maxtemp ~ x_mintemp)


#..(1)..Create a plot of the residuals (y-axis) against the fitted values of y (x-axis)
  
  #Create the residuals
  error_temp <- resid(templm)
  #Create the fitted values (i.e the values corresponding to the linear model)
  fit <- fitted(templm)
  



plot(fit, error_temp, xlab="Fitted Value, y-value predicted by model", ylab="Residuals", main="Residual vs Fitted Value", col="purple")
    #include a line to represent the expected mean value of 0
    abline(0,0, col="red")

    
    
#..(2)..Create a plot of the Root(Residuals) (y-axis) against the fitted values of y (x-axis)
    
    #Create the root residuals
    root_error_temp <- sqrt(abs(resid(templm)))
    
plot(fit,root_error_temp, xlab="Fitted Values, y-values predicted by model", ylab = "Square Root of Residuals", main = "Root Residuals vs Fitted Values", col="red")
abline(0,0, col="yellow")


#..(2.5)..Create a plot of the standard residual (observed-expected)/sqrt(expected)

  #Create the standard residual
  standard_error_temp_root <- sqrt(rstandard(templm))

plot(fit, standard_error_temp_root, xlab="Fitted Values, y-values predicted by model", ylab = "Root of Standard Residuals", main = "Standard Residuals vs Fitted Values", col="blue")



#..(3)..Create a Q-Q plot of the residuals and fitted values
qqnorm(standard_error_temp, main="Normal Q-Q plot; Actual Residuals", ylab = "Quantiles of Actual Residuals")
qqline(standard_error_temp)

#..(3.5)..Create a Q-Q plot of the standard residuals and fitted values
  qqnorm(standard_error_temp, main="Normal Q-Q Plot; Standard Residuals", ylab = "Quantiles of Standard Residuals")
  qqline(standard_error_temp)

  
  
#..(4)..Create a Plot of Residuals against Leverage (Cook's Distance)
    #Create the Leverage values
    leverage <- cooks.distance(templm)
  
plot(leverage, resid(templm), xlab="Leverage (Cook's Distance)", ylab = "Residual", main = "Residual vs Leverage")

#..(4.5)..Create a Plot of Standard Residuals against Leverage (Cook's Distance)

plot(leverage, resid(templm), xlab="Leverage (Cook's Distance)", ylab = "Standard Residual", main = "Std. Residual vs Leverage")


# Automatically Generate all Residual Plots -------------------------------
plot(lm(Temperature$`Max Temperature`~Temperature$`Min Temperature`))


#It could be prudent to detach the Temperature library, or make that a habit...I guess
detach(Temperature)

