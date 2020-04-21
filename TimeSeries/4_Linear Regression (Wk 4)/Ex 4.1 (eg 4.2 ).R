#install.packages(EnvStats)
library(EnvStats)

#Include the data frame
#library(readr)
#Temperature <- read_csv("Temperature.csv")

#Use the attach command to attach the data frame to R's variable search, i.e. enable tabbing.
attach(Temperature)

#Create a linear model

x_mintemp <- Temperature$`Min Temperature`
y_maxtemp <- Temperature$`Max Temperature`

temp_line <- lm(y_maxtemp ~ x_mintemp)

#Print a summary of the linear Model
summary(temp_line)

#Generate a Scatter plot of the temperatures
plot(x_mintemp, y_maxtemp, col="purple", main="Comparison of Max/Min Temperatures")

#add a line through the generated plot
abline(temp_line, col="blue")



#Print the equation of the line
# The Intercept value is:
temp_line$coefficients[1]
# The slope value is:
temp_line$coefficients[2]
# Thus the equation is
print("`Max Temperature`= 7.4 * `Min Temperature` * 1.1")

#Is the linear Correlation Significant, Check the t-stat, F-stat and p-value of the Linear Model

if(summary(temp_line)$coefficients[7] < 0.001 & summary(temp_line)$coefficients[8] < 0.001){
  print("The coefficients differ from zero at a 99.9% confidence level")
}else(print("The t-values cannot be said to be different from zero at a 99.9% confidence level"))


#Forecast the Maximum temperature that would be recorded given the minimum temperature was 22
forecast_temp <- 22

predict.lm(temp_line, newdata=data.frame(x_mintemp=forecast_temp), interval="predict", level=0.95)

#Forecast the Mean value of the Maximum temperature that would be recorded given the minimum temperature was 22

predict.lm(temp_line, newdata=data.frame(x_mintemp=forecast_temp), interval="confidence", level=0.95)



#It could be prudent to detach the Temperature library, or make that a habit...I guess
detach(Temperature)









