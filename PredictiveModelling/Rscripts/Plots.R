
# Load Packages -----------------------------------------------------------

if(require('pacman')){
  library('pacman')
}else{
  install.packages('pacman')
  library('pacman')
}

pacman::p_load(ggplot2, dplyr) 



# Set the Working Directory -----------------------------------------------

set_wd <- function() {
  
  #Install the RStudio API package
  
  if(require('rstudioapi')){
    library('rstudioapi')
  }else{
    install.packages('rstudioapi')
    library('rstudioapi')
  }
  
  #Use the Rstudio API to get the working directory
  
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
}

set_wd()


# Create the Arbitrary Points ---------------------------------------------

n     <- runif(1, min = 5, max = 20)
error <- rnorm(n, mean = 0, sd = 8)

Ct    <- c(runif(n, min = 0, max = 50))
Ft    <- c(1.8*Ct + 30 + error) #Length of Class

temps <- (data.frame("Celsius" = Ct, "Fahrenheit" = Ft))

  #Create Plot
    ggplot(data = temps, aes(x = Celsius, y = Fahrenheit)) +
      geom_point(size = 4, col = "royalblue") +
      theme_classic() + 
      ggtitle("Comparison of various temperatures")















