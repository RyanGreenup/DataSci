
# Preamble ----------------------------------------------------------------

##Load the Packages
if(require('pacman')){
  library('pacman')
}else{
  install.packages('pacman')
  library('pacman')
}

pacman::p_load(xts, sp, EnvStats, gstat, ggplot2, rmarkdown, reshape2, ggmap,
               RColorBrewer, parallel, dplyr, plotly)

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



##Load the dataset
house.df <- read.csv(file = "house2018.csv", header = TRUE, sep = ",")

###Create Assignments
    #when using the predict() function, using assignments
    #becomes important, so when creating linear models, get
    #get into the habit of creating assignments

head(house.df)
home      <- house.df$home
nbhd      <- house.df$nbhd
offers    <- house.df$offers
sqm       <- house.df$sqm
brick     <- house.df$brick
bedrooms  <- house.df$bedrooms
bathrooms <- house.df$bathrooms
price     <- house.df$price

#Create a plot of the data
#layout(matrix(1:2, ncol=2)) #Create a Layout Grid
#par(mfrow = c(1,2))
  #Use Cowplot for ggplot2  

house.baseplot <- function(){
  
  plot(y = price, x = sqm, col = "indianred", cex = 2, pch = 20,
     xlab = "Size (sqm)",
     ylab = "Price ($)",
     main = "Property Cost Relative to Size")
}

house.ggplot <- ggplot(house.df, aes(x = sqm, y = price)) +
                  geom_point() +
                  xlab("Size (sqm)") +
                  ylab("Price ($)") +
                  ggtitle("Price Relative to Size") +
                  theme_classic()

house.ggplot
house.baseplot()

#Create the Linear Model
house.lm <- lm(price ~ sqm)
summary(house.lm)

#Plot the Linear Model
house.baseplot()
abline(house.lm)

house.ggplot + geom_abline(slope     = house.lm$coefficients[2],
                           intercept = house.lm$coefficients[1],
                           lwd       = 1,
                           col       = "royalblue")

#Predict the Linear Model for sqm = 210

predict(lm(price ~ sqm),
        data.frame(sqm=210),
        interval = "prediction",
        level = 0.95)


# render(input = "HousingProblem Script.R",
#        output_file = "HousingProblem.docx",
#        output_format = "word_document")
