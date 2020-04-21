
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

milk.df <- read.csv("milk.csv", header = TRUE, sep = ",")
head(milk.df)
    ##Create Assignments
    month <- milk.df$Month
    prod  <- milk.df$Production
    
    milk.lm <- lm(prod ~ month)

# Inspect the Data Set ----------------------------------------------------

plot(x = month, y = prod, lty = 1, type = "b")
    
    ggplot(data = milk.df, aes(x = Month, y = Production)) +
      geom_line(col = "Royalblue", lwd = 1.25, alpha = 0.6) +
      theme_classic() +
      ggtitle("Milk Production") +
      geom_point(col = "purple", lwd = 2)

  
  #So observe that this data is seasonal and has a positive trend, 
    #in time series we would take the difference of the data
    #and then try to remove the seasonality in order to fit a model.
    
    # Inspect the Residuals
    layout(matrix(nrow = 2, 1:4))
    plot(milk.lm)
    layout(matrix(nrow = 1, 1))
    
    norm.pval <- shapiro.test(resid(milk.lm))
    if(norm.pval$p.value < 0.05){
      print("The residuals are Sufficiently normal, a linear model is appropriate")
    }

layout(matrix(nrow =2, 1:2))
acf(prod, lag.max = 50)
pacf(prod, lag.max = 50)
layout(matrix(nrow = 1, 1))

#This suggests that a seasonal ARIMA model may be more useful,
#however we will use a linear model with trigonometric variables,
#Like in the lecture Notes


# Create Linear Model -----------------------------------------------------

milk.lmtrig <- lm(prod ~ month + I(sin(2*pi*month/12)) + I(cos(2*pi*month/12)))

  #In order to plot this model, we need to use it to predict
  #over a data frame
    x.pred <- 1:length(month)      
    y.pred <- predict(milk.lmtrig, data.frame(month = x.pred))
    milk.mod <- data.frame("Month" = x.pred, "Production" = y.pred)    
  
    #Plot the Prediction over the initial plot
    
      #Base Plot
      plot(x = month, y = prod, lty = 1, type = "b")
      lines(x = milk.mod$Month, y = milk.mod$Production, col = "red")
      
      #ggplot2
      ggplot(data = milk.df, aes(x = Month, y = Production)) +
        geom_line(col = "Royalblue", lwd = 3, alpha = 0.6) +
        theme_classic() +
        ggtitle("Milk Production and Model") +
        geom_point(col = "purple", lwd = 2, alpha = 0.6) +
        #add in the model
        geom_line(col = "Indianred", lwd = 1) 
      
        ##ggplot2 melted
        milk.df.fin <- data.frame("Month" = month,
                                  "Observed" = prod,
                                  "Modelled" = milk.mod$Production)  
        milk.df.fin.melt <- melt(milk.df.fin, id = "Month") 
        ggplot(data = milk.df.fin.melt, aes(x = Month,
                                            y = value,
                                            col = variable)) +
          geom_line(lwd = 1.2, alpha = 0.9) +
          theme_classic() +
          ggtitle("Monthly Milk Production") +
          xlab("Milk Production") +
          scale_color_manual(values = c('Observed' = 'grey',
                                        'Modelled' = 'indianred'))
      
      