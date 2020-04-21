###Mean difference between Maximum and Minimum Temperatures, Jan, Melb.
#---------------------------------------------------------------------------------------------------

###Create the defecit vector from the data frame
defecit <- Temperature$`Max Temperature`-Temperature$`Min Temperature`
defecit


###State the equation
  #upper <-  x_sample + (s/sqrt(n)) * t_25
  #lower <-  x_sample - (s/sqrt(n)) * t_25

###Variables
x_mean <- mean(defecit)
s <- sd(defecit) #This is the sample standard deviation
n <- length(defecit)
df <- n-1

###Calculate the t-value 
t <- qt(0.05/2, df,  lower.tail=TRUE, log.p=FALSE) #This value will be negative

#Calculate the Upper Limit
upper <- x_mean - (s/sqrt(n)*t)

#Calculate the Lower Limit
lower <- x_mean + (s/sqrt(n)*t)

#Print the limits
upper
lower




##Print Descriptive statistics

statistics_1 <- c("Sample Mean"=x_mean, "Sample Standard Deviation"=s)
statistics_2 <- c("Sample Size"=n, "Degrees of Freedom"=df, "t-value, 95%, d.f.=364"=t)



statistics_1
statistics_2

