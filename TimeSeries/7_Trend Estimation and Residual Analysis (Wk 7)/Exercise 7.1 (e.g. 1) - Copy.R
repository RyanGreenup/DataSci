# Preamble ----------------------------------------------------------------
layout(matrix(1))

#Include packages
require(xts)
require(astsa)

#Import Data Frame
  #uspop is builtin

#Assign Variables
if(require(xts)==FALSE){
  return("you need to install xts as well as as.Y to use xts and/or zoo")
}else{

  uspop_vec_index <- as.character(index(uspop))
  uspop_vec_index <- as.Date(uspop_vec_index, format="%Y")
  uspop_vec_vals <- as.vector(uspop)
  uspop_xts <- as.xts(uspop_vec_vals, order.by=uspop_vec_index)
    
}






 

#Plot Us Population 1790-1970 --------------------------------------------

  #As an extended time series (xts)

    plot(uspop_xts, 
            type="b",
            ylab="US Population",
            xlab="Year",
            main="1790-1970; US Population",
            lwd=8, col="lightblue")
 
  #As a built in time series (ts) 
    ts.plot(uspop, 
         type="b",
         ylab="US Population",
         xlab="Year",
         main="1790-1970; US Population",
         lwd=3, col="purple")

#Create a Quadratic model to compare

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
   legend(1800,150, legend = c("Observed Values", "Quad. Model"),
          fill = TRUE, col=c("purple", "Red"),lty=(1), lwd=c(2,4))


   
   
   
# #Create a residual plot -------------------------------------------------

layout(matrix(nrow=3, data = 1:6))
uspopqmresid <- uspop-uspop.qm_fit
plot(y=uspopqmresid, x=uspoptimevar1, type = 'b', lwd=4, col="mediumpurple2", xlab="Time-Value: Years", ylab="Residual Error", main="Residuals Against Time")
par(xpd=FALSE);abline(0,0, col="violetred2", lwd=3, lty=3)
   
plot(y=uspopqmresid, x=uspop.qm_fit, type = 'b', lwd=4, col="mediumseagreen", xlab="Fitted Value: US-Population", ylab="Residual Error", main="Residuals Against fitted Values")
par(xpd=FALSE);abline(0,0, col="royalblue2", lwd=3, lty=3)

plot(uspop.qm, lwd=4, col="Blue", lty=3, cex=2)


library(astsa)
par(xpd=FALSE)
sarima(uspop.qm$residuals, p=0, d=0, q=0)
acf(uspopqmresid)
library("astsa")

