
# Create Plots of AR(2) and MA(2) --------------------------


# AR(2) Plots -------------------------------------------------------------

    plotar2 <- function(phi1=1.2, phi2=-0.7, n=100){     

x <- arima.sim(model = list(ar=c(phi1, phi2)), n = n)
ts.plot(x, ylab="AR(2) Response", col="palegreen2", lwd=5, lty=1, main="AR(2) Time Series Plot")
mtext(expression(paste(phi, "_1 =")), adj=0.5)
mtext(paste(phi1), adj = 0.585)        
mtext(expression(paste(phi, "_2 =")), adj=0.7)
mtext(paste(phi2), adj = 0.8)        


acf(x,  col="plum2", lwd=5, main="AR(2) Auto-Correlation Plot")
mtext(expression(paste(phi, "_1 =")), adj=0.5)
mtext(paste(phi1), adj = 0.585)        
mtext(expression(paste(phi, "_2 =")), adj=0.7)
mtext(paste(phi2), adj = 0.8)        

}

# MA(2) Plots -------------------------------------------------------------


    plotma2 <- function(theta1=1.2, theta2=-0.7, n=100){
x <- arima.sim(model = list(ma=c(theta1, theta2)), n = n)
ts.plot(x, ylab="MA(2) Response", col="palegreen2", lwd=5, lty=1, main="MA(2) Time Series Plot")
mtext(expression(paste(theta, "_1 =")), adj=0.5)
mtext(paste(theta1), adj = 0.585)        
mtext(expression(paste(theta, "_2 =")), adj=0.7)
mtext(paste(theta2), adj = 0.8)        


acf(x, lag.max = 8, col="plum2", lwd=15, main="MA(2) Auto-Correlation Plot")
mtext(expression(paste(theta, "_1 =")), adj=0.5)
mtext(paste(theta1), adj = 0.585)        
mtext(expression(paste(theta, "_2 =")), adj=0.7)
mtext(paste(theta2), adj = 0.8)  
    }
    
# ARMA (1,1) Plot ---------------------------------------------------------    
    
    plotarma11 <- function(phi=0.7, theta=0.4, n=100){
      
x <- arima.sim(   model = list(   order = c(1,0,1), ar=phi, ma=theta      ), n=n         )
ts.plot(x, ylab="ARMA(1,1) Response", col="palegreen2", lwd=5, lty=1, main="ARMA(1,1) Time Series Plot")
mtext(expression(paste(theta, "=")), adj=0.5)
mtext(paste(theta), adj = 0.585)        
mtext(expression(paste(phi, "=")), adj=0.7)
mtext(paste(phi), adj = 0.8)        


acf(x, lag.max = 8, col="plum2", lwd=15, main="ARMA(1,1) Auto-Correlation Plot")
mtext(expression(paste(theta, "=")), adj=0.5)
mtext(paste(theta), adj = 0.585)        
mtext(expression(paste(phi, "=")), adj=0.7)
mtext(paste(phi), adj = 0.8)  

    }
    
    
    
# Create the plots --------------------------------------------------------
layout(matrix(1:6, byrow=TRUE, ncol=2))
plotar2()
plotma2()
plotarma11()
layout(matrix(1))





