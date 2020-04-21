
# ARIMA(2,0,1) Plots -------------------------------------------------------------
    plotarima201 <- function(phi1=1, phi2=-0.25, theta=0.5, n=100){     
        x <- arima.sim(model = list(order=c(2,0,1), ar=c(phi1, phi2), ma=theta), n = n)
        ts.plot(x, ylab="AR(2) Response", col="palegreen2", lwd=5, lty=1, main="ARIMA(2,0,1) Time Series Plot")
        acf(x,  col="plum2", lwd=5, main="ARIMA(2,0,1) Auto-Correlation Plot")
              }

# Create the plots --------------------------------------------------------
layout(matrix(1:2, byrow=TRUE, nrow=2))
  plotarima201()
layout(matrix(1))





