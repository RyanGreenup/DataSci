
# Preamble ----------------------------------------------------------------


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
    
    
    
    # Install / Load Packages -------------------------------------------------
    
    
    if(require('pacman')){
      library('pacman')
    }else{
      install.packages('pacman')
      library('pacman')
    }
    
    pacman::p_load(xts, sp, gstat, ggplot2, rmarkdown, reshape2, ggmap,
                   parallel, dplyr, plotly)
    
    
    # Import Data Set ---------------------------------------------------------
    
    all.df <- read.csv(file = "practical04b.csv", header = TRUE, sep = ",")
    head(all.df)
    

    # Set the Seed ------------------------------------------------------------
    #Such that the code is consistent we will set the seed.
    set.seed(seed = 23)

input <- all.df$input
output <- all.df$output

model1 <- lm(output ~ input)    
model2 <- lm(output ~ I(input^2) + input)
model3 <- lm(output ~ I(input^3) + I(input^2) + input)

rss1 <- sum((resid(model1))^2)
rss2 <- sum((resid(model2))^2)
rss3 <- sum((resid(model3))^2)

RSS.vec <- c("model1 RSS" = rss1,
                     "model2 RSS" = rss2,
                     "model3 RSS" = rss3)

min.mod <- names(RSS.vec[RSS.vec == min(RSS.vec)])

    
print(
  
  paste(
    
    "The model with the lowest RSS is", min.mod
  )
)

