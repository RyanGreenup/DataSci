n <- 10000


#Random Walk
#######################################
z <- 0
i <- 0
z_vector <- c()


while(length(z_vector)<n){
  i <- i+1
  z <- z+white_noise_vector[i+1]
  z_vector <- c(z_vector, z)
  
  percent(length(z_vector)/n)
  
}

plot.ts(z_vector, col="brown", ylab="Random Walk; Z_t=Z_(t-1)+'White noise'", lwd=1, type="s")

#Moving Average
#######################################

white_noise_vector <- rnorm(n, mean = 0, sd=8)
Z <- 0
i <- 0
zt <- c()

while(length(zt)<100){
  
  i <- i+1
  zt <- c(zt, white_noise_vector[i]-(1/4)*white_noise_vector[i-1])
}
plot.ts(zt, col="grey", lwd=1,ylab="zt=at-1/4at")



# White Noise -------------------------------------------------------------
#######################################
z <- 0
i <- 0
z_vector <- c()


while(length(z_vector)<n){
  i <- i+1
  z <- z+white_noise_vector[i+1]
  z_vector <- c(z_vector, z)
  
  percent(length(z_vector)/n)
  
}

plot.ts(rnorm(3000), ylab="Random Normal Values", col="tan")
plot.ts(runif(300,max = 1, min = -1), ylab="Random Uniform Values", col="brown", lwd=3)

