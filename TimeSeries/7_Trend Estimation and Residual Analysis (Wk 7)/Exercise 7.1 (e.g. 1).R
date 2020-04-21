# Preamble ----------------------------------------------------------------

#Include packages
if(require('pacman')){
  library('pacman')
}else{
  install.packages('pacman')
  library('pacman')
}

pacman::p_load(xts, sp, gstat, ggplot2, rmarkdown, reshape2, ggmap,
               parallel, dplyr, plotly, sarima, xts)
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
         lwd=2, col="purple")

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
   legend(1800,150, legend = c("Quad. Model", "Observed Values"),
          fill = TRUE, col=c("purple", "Red"),lty=(1), lwd=c(2,4))


   
   
   
# #Create a residual plot -------------------------------------------------

layout(matrix(nrow=3, data = 1:6))
uspopqmresid <- uspop-uspop.qm_fit
plot(y=uspopqmresid, x=uspoptimevar1, type = 'b', lwd=4, col="mediumpurple2", xlab="Time-Value: Years", ylab="Residual Error", main="Residuals Against Time")
par(xpd=FALSE);abline(0,0, col="violetred2", lwd=3, lty=3)
   
plot(y=uspopqmresid, x=uspop.qm_fit, type = 'b', lwd=4, col="mediumseagreen", xlab="Fitted Value: US-Population", ylab="Residual Error", main="Residuals Against fitted Values")
par(xpd=FALSE);abline(0,0, col="royalblue2", lwd=3, lty=3)

plot(uspop.qm, lwd=4, col="Blue", lty=3, cex=2)


require(sarima)
acf(uspopqmresid)
?sarima(uspop.qm_fit)


    # Built in Residual Diagnostics -------------------------------------------
    arima_resid <- function(resid, p=0, q=0){
      my_colours <- c("#798BC6", "#61CC96", "#242038")
      
      #Residual diagnostics
      layout( matrix(nrow=3, ncol=2, byrow=1, data=c(1,1,2,3,4,4)))
      
      ts.plot(resid, xlab="Residuals", 
              main="Residuals over Time", col=my_colours[3], lwd=2)
      abline(0,0, col=my_colours[2], lwd=3)
      
      x.acf <- acf(resid, plot = 0)
      #Remove Lag 0
      x.acf$acf[1] <- NA
      plot(x.acf,lwd=10, col=my_colours[1], ylim=c(-1,1), main="Residuals")
      
      qqplot(resid, rnorm(2000,0, sd(resid)), col=my_colours[2], cex=2)
      abline(0,1, lwd=3, col=my_colours[3])
      
      require(FitAR)
       # LJBT <- LjungBoxTest(resid, k=1)
       # plot(LJBT[1:20,3], ylim=c(0,1))
      LBQPlot(resid, k=p+q)
      
      layout(matrix(1)) 
    }
        
    arima_resid(uspopqmresid)
    
    

  # Forecast Package --------------------------------------------------------
    require(forecast)
    my_colours <- c("#798BC6", "#61CC96", "#242038")
   checkresiduals(uspopqmresid, col=my_colours[1])

