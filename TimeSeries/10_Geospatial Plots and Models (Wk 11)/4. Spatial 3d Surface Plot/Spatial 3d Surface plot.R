# Preamble ----------------------------------------------------------------
#Packages
library(EnvStats)
library(ggplot2)
library(GGally)
library(corrplot)
library(dplyr)
library(tidyr)
library(stringr)
library(reshape2)
library(ggmap)
library(stringr)
library(plotly)


#Assignments
benthic <-   Benthic.df
lat     <- benthic$Latitude 
lon     <- benthic$Longitude
index   <- benthic$Index
sal     <- benthic$Salinity
silt    <- benthic$Silt

#Lattitude and Longitude
#Lattitude is a line that runs east to west that measures how far north of the equatur
#Lattitude is assigned to the y-axis
#Longitude is a line that runs north to south that measures east/west
#Longitude is assigned to the x-axis 

#Scatter plot
   #It's a good idea to make a scatter plot first to get an idea of the data
        #doing this over a map with ggmap() is even better


# Create Surface Model ----------------------------------------------------
  
  #It is necessary to create a model that will become the plane that will be plotted
  
  #Before any 3d surface or contour plots can be made
  #A smooth modelled surface needs to be predicted
  
  #Create model for smoothed surface
  alpha       <- 0.1
  index.model <- loess(formula = index ~ lon * lat, span = alpha)
  
  
  # Create a sequence of incrementally increasing for the X and Y axis 
  xgrid       <-  seq(min(lon), max(lon), length.out = 100)
  ygrid       <-  seq(min(lat), max(lat), length.out = 100)
  
  #Having already gone through this, I already know the 
  #domain is going to have to be these values
  #Otherwise the z-value has outliesrs (i.e. 50 instead of 5)
  #This fucks up colour scales
  
  xgrid       <-  seq(-77, -75.9, length.out = 100)
  ygrid       <-  seq(38.2, 38.9, length.out = 100)
  
  # Generate a dataframe with every possible combination of wt and hp
  xy.surface  <- expand.grid(lon = xgrid, lat = ygrid)
  
  # Feed the dataframe into the loess model and receive a matrix output 
  #with estimates of the Z-value (Bethnic Index) 
  
  index.fit   <-  predict(index.model, newdata = xy.surface)
  
  # Abbreviated display of final matrix
  index.fit[1:4, 1:4]
  
  #Create a Base graphic of the contours
  contour(x = xgrid, y = ygrid, z = index.fit,
          xlab = "Longitude",
          ylab = "Lattitude",
          main = "Bethnic Index")
  mtext(paste("smoothing factor of", alpha))
    
    #Because the contour plot looks legit, we know that
        #The data is good for plotting
  
  

# Create the Surface Plot -------------------------------------------------

  #The method to create a surface plot in Base and in plotly is reasonably similar
  
  #Base Surface Plot
  persp(xgrid, ygrid, index.fit,
        xlim = c(-77.3, -75.9),   #the values domain needs to be adjusted above
        ylim = c(38.1, 39.5),          #Make sure to set an appropriate domain
        # zlim = c(0, 6),
        theta = -45, phi = 30, d = 0.5,
        xlab="-Longitude (degrees West)",
        ylab="Latitude (degrees North)",
        zlab="Benthic Index", ticktype = "detailed")
  title(main=paste("Surface Plot of Benthic Index", "Loess Smoothing", sep="\n"))   
  
  #Plotly Surface Plot
  p <- plot_ly(x = ygrid, y = xgrid, z = index.fit) %>%
    add_surface() %>%
    layout(
      title = "Benthic Index in  Chesapeake Bay",
      scene = list(
        xaxis = list(title = "Longitude",
                     range = c(38.2, 38.9)),  #You shouldn't need to edit the
        yaxis = list(title = "Latitude",      #The domain here, do it above
                     range = c(-77, -75.9)),
        zaxis = list(title = "Benthic Index")
      ))
                                                                  
  p
  
  
  
  
  
  
  
  
  
  
  
  
