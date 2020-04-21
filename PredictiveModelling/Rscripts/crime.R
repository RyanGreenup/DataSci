
# Preamble ----------------------------------------------------------------
  ##Load Packages
if(require('pacman')){
  library('pacman')
}else{
  install.packages('pacman')
  library('pacman')
}

pacman::p_load(xts, sp, gstat, ggplot2, rmarkdown, reshape2, ggmap,
               parallel, dplyr, plotly)


  ##Set Working Directory
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



library(glmnet)
# crime data set
mydata <- read.csv("CommViolPredUnnormalizedData.csv")

for (i in 3:ncol(mydata)) {
  mydata[is.na(mydata[,i]), i] <- median(mydata[,i], na.rm = TRUE)
}

myx <- scale(subset(mydata, select = A1:A124))
myy <- as.matrix(subset(mydata, select = B2))

set.seed(1234)
lasso <- cv.glmnet(myx, myy, alpha=1)
ridge <- cv.glmnet(myx, myy, alpha=0)


lasso
