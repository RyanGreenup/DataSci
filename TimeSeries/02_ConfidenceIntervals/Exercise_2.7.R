###Mean difference between Maximum and Minimum Temperatures, Jan, Melb.
#---------------------------------------------------------------------------------------------------

###Create the Concentration vector from the data frame
Concentration_vector <- c(ChemConc$Concentration) #So I can use the length command to count the observations


###State the equation
#upper <-  p + t * SE
#lower <-  p - t * SE

###Variables
  ###The How many contaminated site?
  contam <- with(ChemConc, Concentration>5)
  no._contam <- length(contam[contam==TRUE]) # I just found the test==true on Stack Exchange, apparently better than table(contam)["TRUE"]  
  

n <- length(ChemConc$Concentration)
p <- no._contam/n
df <- n-1
SE <- sqrt(p*(1-p)/n)
alpha <- 0.05


###Calculate the t-value 
t <- qt(alpha/2, df, lower.tail=FALSE, log.p=FALSE )

#Calculate the Upper Limit
upper <-  p + t * SE

#Calculate the Lower Limit
lower <-  p - t * SE

#Print the limits
upper
lower

statistics_1 <- c("Sample Proportion"=p, "Standard Error"=SE)
statistics_2 <- c("Sample Size"=n, "Degrees of Freedom"=df, "t-value, 95%, d.f.=364"=t)



statistics_1
statistics_2


