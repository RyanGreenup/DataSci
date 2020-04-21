
# Load Packages -----------------------------------------------------------

if(require('pacman')){
  library('pacman')
}else{
  install.packages('pacman')
  library('pacman')
}

pacman::p_load(xts, sp, gstat, ggplot2, rmarkdown, reshape2, ggmap,
               parallel, dplyr, plotly)


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


# Import Data Set ---------------------------------------------------------

housescrime.df <- read.csv("housescrime.csv", header = TRUE, sep = ",")
head(housescrime.df)
##Create Assignments
price   <- housescrime.df$HousePrice
crime   <- housescrime.df$CrimeRate
density <- housescrime.df$HsPrc10k
milesp  <- housescrime.df$MilesPhila
popchg  <- housescrime.df$PopChg

housescrime.lm <- lm(price ~ crime)
summary(housescrime.lm)

layout(matrix(nrow = 2, 1:4))
plot(housescrime.lm)

plot(y = crime, x = price, main = "Crime Rates", xlab = "Property Prices", ylab = "Crime Rate")

#it appears as if crime is independent of houseprices
#there are more samples at more average house prices
#and there is a slight negative trend,
#but the data suggests that other variables may account for this

crime.mlm <- lm(crime ~ price + density + milesp +popchg)
summary(crime.mlm)

#a multiplle linear regression suggests that population change is a better predictor
