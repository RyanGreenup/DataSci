
# Preamble ----------------------------------------------------------------
##Clear latent variables
rm(list = ls())

##Set working directory and load packages

setwd.loadpac <- function() {
  
  if(require('pacman')){
    library('pacman')
  }else{
    install.packages('pacman')
    library('pacman')
  }
  
  pacman::p_load(ggmap, plotly, EnvStats, ggplot2, GGally, corrplot, dplyr,
                 tidyr, stringr, reshape2, cowplot, ggpubr, reshape2,
                 ggplot2, rmarkdown, dplyr, plotly, rstudioapi, wesanderson,
                 RColorBrewer, colorspace, glmnet)
  
  #Use the Rstudio API to get the working directory
  
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
  
}

setwd.loadpac()


# Load Dataset ------------------------------------------------------------
