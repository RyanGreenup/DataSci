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
  
  pacman::p_load(ggmap, plotly, EnvStats, ggplot2, GGally, corrplot, dplyr, tidyr, stringr, reshape2, cowplot, ggpubr, tidyverse, reshape2, ggplot2, rmarkdown, dplyr, plotly, rstudioapi, wesanderson, RColorBrewer)
  
  #Use the Rstudio API to get the working directory
  
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
  
}

setwd.loadpac()


# Load Dataset ------------------------------------------------------------

all.df <- read.csv(file = "practical06a.csv", TRUE, ",")
#all.df <- read.csv(file = file.choose(), TRUE, ",")


# Convert the Output to a categorical varialbe (factor) -------------------
all.df$y <- as.factor(all.df$y)
head(all.df)


# Plot the Data -----------------------------------------------------------
  ##Outcome by colour
col.plot <- ggplot(all.df, aes(y = x2, x = x1, col = y)) +
  geom_point() + 
  theme_classic() +
  labs(x = "Predictor 1", y = "Predictor 2",
       title = "Discrete Outcome From Predictor Values")
col.plot

  ##Side by Side
outvx1.plot <- ggplot(all.df, aes(y = y, x = x1, col = y)) +
  geom_point() + 
  theme_classic() +
  labs(x = "Predictor 1", y = "Outcome",
       title = "Discrete Outcome From Predictor Values")

outvx2.plot <- ggplot(all.df, aes(y = y, x = x2, col = y)) +
  geom_point() + 
  theme_classic() +
  labs(x = "Predictor 2", y = "Outcome",
       title = "Discrete Outcome From Predictor Values") 


ggarrange(outvx1.plot, outvx2.plot)


  ##3d Scatter Plot
obs.plotly <- plot_ly(all.df, x = ~x1, y = ~x2, z = ~y, color = ~y, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(title = "Observed Values", scene = list(xaxis = list(title = 'Predictor 1'),
                      yaxis = list(title = 'Predictor 2'),
                      zaxis = list(title = 'Outcome')))

obs.plotly


# Fit a Logistic Regression -----------------------------------------------
log.mod <- glm(y ~ x1 + x2, data = all.df, family = "binomial")
  #Comment here on why we write binomial
  #also specifically point out the format
    ##i.e. don't specify vectors, specify dataframes with
        #syntactically correct column names



# Create a 3d surface plot of that model ----------------------------------
  ##Plot the modelled point probabilities
all.df2 <- data.frame(all.df, pred = predict(log.mod, type = 'response'))
pred.plotly <- plot_ly(all.df2, x = ~x1, y = ~x2, z = ~pred, color = ~y, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Predictor 1'),
                      yaxis = list(title = 'Predictor 2'),
                      zaxis = list(title = 'Outcome')))

pred.plotly
  
  ##Plot the Model surface
  
  xgrid       <-  seq(min(all.df$x1), max(all.df$x1), length.out = 100)
  ygrid       <-  seq(min(all.df$x2), max(all.df$x2), length.out = 100)
  
  xy.surface  <- expand.grid(x1 = xgrid, x2 = ygrid)
  
  xy.pred     <- predict(log.mod, newdata = xy.surface, type = "response")
  z.pred      <-  matrix(xy.pred, nrow = 100, ncol = 100)
  
  
  contour(x = xgrid, y = ygrid, z = z.pred,
          xlab = "Predictor 1",
          ylab = "Predictor 2",
          main = "Probability of Outcome")
 par(mfrow=c(1,1)) 
  persp(xgrid, ygrid, z.pred,
        xlim = c(min(all.df$x1), max(all.df$x1)),   #the values domain needs to be adjusted above
        ylim = c(min(all.df$x2), max(all.df$x2)),          #Make sure to set an appropriate domain
        # zlim = c(0, 6),
        theta = -45, phi = 30, d = 0.5,
        xlab="-Longitude (degrees West)",
        ylab="Latitude (degrees North)",
        zlab="Benthic Index", ticktype = "detailed")
  title(main="Logistic Regression Model")   
  
  model.surface.plotly <- plot_ly(x = ygrid, y = xgrid, z = z.pred) %>%
    add_surface() %>%
    layout(
      title = "Probability of Outcome",
      scene = list(
        xaxis = list(title = "Predictor 1",
                     range = c(min(all.df$x1), max(all.df$x1))),  #You shouldn't need to edit the
        yaxis = list(title = "Predictor 2",      #The domain here, do it above
                     range = c(min(all.df$x2), max(all.df$x2))),
        zaxis = list(title = "Probability of Outcome")
      ))                                                         
  model.surface.plotly
  
  
  

# 2d Model Plot -----------------------------------------------------------

  xgrid       <-  seq(min(all.df$x1), max(all.df$x1), length.out = 100)
  ygrid       <-  seq(min(all.df$x2), max(all.df$x2), length.out = 100)
  
  mod.pred  <- expand.grid(x1 = xgrid, x2 = ygrid)
mod.pred$pred <- predict(log.mod, newdata = xy.surface, type = "response")

head(mod.pred)
 
all.df3 <- all.df
all.df3$y <- as.numeric(all.df3$y)-1
head(all.df3)

ggplot(all.df3, aes(x = x1)) +
  geom_point(data = all.df3, aes( x = x1, y = y, col = y), size = 8) +
  guides(col = FALSE) +
  geom_point(data = mod.pred, aes(y = pred, x = x2), col = "Purple", alpha = 0.2) +
  labs(title ="Modelled Probability Relative to Second Predictor", x = "First Predictor", y = "Probability of outcome (and/or Observation)")


# Compute and Plot the Decision Boundary ----------------------------------
  ##So the decision boundary is a straight line
    ##we can pull this out of the model

  ##The decision boundary, is the line:
intercept <- (-1)*log.mod$coefficients["(Intercept)"]/log.mod$coefficients["x2"]
slope     <- (-1)*log.mod$coefficients["x1"]/log.mod$coefficients["x2"]
  
  ##This can be plotted in base graphics:
  col.vec <- c("red", "blue")
  plot(x = all.df$x1, y = all.df$x2, col = col.vec[as.numeric(all.df$y)],
     xlab = "Predictor 1", ylab = "Predictor 2",
     main = "Outcome from Predictor Values")
  abline(intercept, slope, lwd = 3, col = "purple", lty = 3)
  
  ##In ggplot
  
 col.plot +
   geom_abline(intercept = intercept, slope = slope, lwd = 1, col = "purple")
 