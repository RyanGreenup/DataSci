require(astsa)

# Generate 100 observations from the AR(1) model
x <- arima.sim(model = list(order = c(1, 0, 0), ar = .9), n = 100) 

# Plot the generated data 
plot(x)

# Plot the sample P/ACF pair
acf2(x)

# Fit an AR(1) to the data and examine the t-table
sarima(x, 1, 0, 1)

# 1.	Standardized residuals
#   a.	The standardised residuals can be inspected for patterns that challenge the assumption of normality.
# 2.	Sample ACF of residuals
#   a.	White noise should not have any autocorrelation, if the residuals have autocorrelation then the residuals are not normally distributed white noise and the model may not be correct.
# 3.	Normal Q-Q plot
#   a.	If the residuals are normal the points should line up 1-1, if the points do not line up this may be evidence that the residuals are not normally distributed, and hence the model is not appropriate.
# 4.	Q-Statistic p-values
#   a.	A statistical test for 'whiteness', if most blue points are above the line it's safe to assume the noise is white, i.e. residuals are normally distributed.
