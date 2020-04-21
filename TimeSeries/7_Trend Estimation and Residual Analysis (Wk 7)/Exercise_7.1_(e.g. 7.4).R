
# Preamble ----------------------------------------------------------------
  rm(list=ls())
layout(matrix(1:1))
  
  #Assign Variables
  
  
# 1. Generate plot of Data
  
plot(uspop, col="palegreen4", lwd=8, xlab="Year", ylab="US Population", main="US Population over Time")


ts.plot(uspop, 
        type="b",
        ylab="US Population",
        xlab="Year",
        main="1790-1970; US Population",
        lwd=2, col="purple")

#2. Create the Quadratic Model for Exercise 7.1
    #Create the quadratic regression y=ax+bx^2
    
    #x-value
    uspoptimevar1 <- as.numeric(index(uspop))
    uspoptimevar2 <- as.numeric(index(uspop))^2
    
    #Find the coefficients
    uspop.qm <- lm(uspop~uspoptimevar1+uspoptimevar2)
    uspop.qm_sum <- summary(uspop.qm)
    
    
    #Plot a dashed line over using the points command
    uspop.qm_fit <- uspop.qm$fitted.values 
    points(y=uspop.qm_fit, x=uspoptimevar1, type="l", lty=4,lwd=2, col="red")
    mtext("Comparison of observed data to quadratic model")
    legend(1800,150, legend = c("Quad. Model", "Observed Values"),
           fill = TRUE, col=c("purple", "Red"),lty=(1), lwd=c(2,4))

# Generate the ACF Plot ---------------------------------------------------

uspop.qm.resid <- uspop.qm$residuals
acf(uspop.qm.resid, lwd=15, col="cadetblue", main="ACF of Resdiuals arising from Quadratic Model")


# Plot the standard residual diagnostics ----------------------------------

layout(matrix(nrow=2, data=1:4))
plot(uspop.qm, col="darkviolet", lwd=2, cex=1.3)
