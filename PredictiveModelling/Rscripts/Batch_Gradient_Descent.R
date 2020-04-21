#0. Preamble
#Include Packages
if(require('pacman')){
  library('pacman')
}else{
  install.packages('pacman')
  library('pacman')
}

pacman::p_load(ggplot2, rmarkdown)
               


# 1. Define a sample table of temperatures

temperatures <- data.frame(Celsius=c(26, 25, 21, -4), Fahrenheit = c(79, 78, 71, 24))

# 2. Set a random intercept (w_0), and a random slope w_1 from which to start

w_0=50*runif(1)
w_1=50*runif(1)

#3. Set RSS and eta to a small value

tol <- 10^-3
dRSS <- tol + 1
eta <- tol/2 #If eta>is too much larger than the tolerance, the values will not converge

#4. Loop until dRSS is a small value
#while(dRSS>0.01) {print(dRSS);dRSS <- dRSS-eta*dRSS}


while(dRSS>0.0001) {
  
  #4.0 Create a Progress Printout:
  print(dRSS); 
  
  
  #4.1 Compute the derivative values along x and y, dw_0, dw_1 using the whole dataset
  
  y=temperatures$Fahrenheit
  x=temperatures$Celsius
  y_model=w_1*x+w_0
  
  
  dw_0=-2*sum( y-y_model) #this is \partialRSS/w_0        #Pretty sure these are correct
  dw_1 = -2*sum( (y-y_model)*x  ) #this is \partialRSS/w_1
  
  
  # 4.2 Update W_0 and W_1 using the values computed in step 4.1.
  
  #The point of using the derivative as a factor:
  #       is so that the increments will be less as we approach the minimum value
  #       When the derivative is negative w_0 will become larger and hence the new value will have a derivative closer to 0
  
  w_0=w_0-eta*dw_0      
  w_1=w_1-eta*dw_1
  
  #4.3 With the new w_0 and w_1, compute a new value for RSS.
  
  #dRSS=sqrt(  (dw_0)^2 + (dw_1)^2  )
  dRSS = 0.5 * (abs(dw_0) + abs(dw_1)) # This makes it easier to predict eta
                                       #values such that the algorithm will
                                        #converge within the tolerance 
                                          #(Rather than bouncing over)
  
  
  #4_Conclusion; end the loop
  
}



#Plot the Line
ggplot(data = temperatures, aes(y = Fahrenheit, x = Celsius)) +
  geom_point(col = "royalblue", size = 4) +
  theme_classic() + 
  geom_abline(slope = w_1, intercept = w_0, col = "pink",
              size = 3, alpha = 0.3) +
  ggtitle("Linear Relationship between Temperature Scales")
