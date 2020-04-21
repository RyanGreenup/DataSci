##Set working directory
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




## Load data
mydata <- read.csv("q3-1.csv")



####Question 1

mydata$x1[is.na(mydata$x1)] <- 60

mean(mydata$x1)

###Question 2

mydata$x2[is.na(mydata$x2)] <- 0.4

mean(mydata$x2)










