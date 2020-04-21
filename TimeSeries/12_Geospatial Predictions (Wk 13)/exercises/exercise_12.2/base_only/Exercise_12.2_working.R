# ---
#   title: "Exercise 12.1"
# output: 
#   html_notebook: 
#   toc: yes
# ---
#  The goal here is to create a prediction model for 
  # the value of benthic over the Chesepeake bay region.

#This prediction model will be graphically portrayed with a:
  
#   1. Heatmap
# i) with base plot
# ii) wigh `ggplot2` over an actual map
# 2. Contour plot
# i) with base plot
# ii) With `ggplot2` over an actual map
# 3. 3d Surface plot
# i) with base plot
# ii) with a dynamic `plotly` surface plot
# 

# Preamble ----------------------------------------------------------------


##Import Library
if(require('pacman')){
  library('pacman')
}else{
  install.packages('pacman')
  library('pacman')
}

pacman::p_load(sp, EnvStats, gstat, ggplot2, rmarkdown, reshape2, classInt,
               tmap, ggmap,RColorBrewer, parallel, dplyr, plotly)


##Create Assignments
#Note that  Benthic.df is included in the `EnvStats` package and is not a 
  #data frame but an `sp` class, which is very similar to a data frame

#Assignments
benthic <- Benthic.df
lat   <- benthic$Latitude
lon   <- benthic$Longitude
index <- benthic$Index

#Inspect the Data Set

head(benthic, 3)


#Lattitude and Longitude
# * Lattitude is a line that runs east to west that measures how far 
# north of the equatour a location is
# + Lattitude is assigned to the y-axis
# * Longitude is a line that runs north to south that measures east/west
# + Longitude is assigned to the x-axis




# Assign Co-ordinates -----------------------------------------------------
#The *Benthic* Data set is an `sp` class objects and requires for its co-ordinates to be set:
rm(benthic)
benthic <- Benthic.df
coordinates(benthic) = ~Longitude + Latitude


# Preliminaries = Create a Krige Prediction Model --------------------------
 #In order to create a krige prediction model two things are required:
    # Fitted Variogram Model (e.g. an exponential model)
    # A domain values over which to predict values

##Create a variogram Model

 ###Create a Variogram 
  
  #(I'm not totally sure why we specify a polynomial object here)
   
   x <- lon
   y <- lat
   
   benthic.vg <- variogram(object = index ~ 
   x + y + 
   x^2 + x*y + y^2, 
   data = benthic)
   
   head(benthic.vg)
   
   
   ###Fit an exponential model to that variogram
   
   benthic.vg.fit.exp <- fit.variogram(benthic.vg, model=vgm(1,"Exp", 0.5,1))
   
   
   #Thus the variogram model needed for the krige prediction has been ascertained
   
   ##Create a Grid of Domain values to use as predictor values
   ### Create a sequence of incrementally increasing for the X and Y axis 
   xgrid       <-  seq(min(lon), max(lon), length.out = 20)
   ygrid       <-  seq(min(lat), max(lat), length.out = 20)
   
   ###Combine into a domain base
    #This domain base however has to be a spatial object, this can be
     #acheived by defining co-ordinates in the data frame
   
   xy.surface  <- expand.grid(lon = xgrid, lat = ygrid)
   coordinates(xy.surface) = ~lon + lat
   head(xy.surface, 3)
   
   ##Create the Krige prediction object
   
   benthic.pred <- krige(formula = Index ~1,
   locations =  benthic,
   newdata = xy.surface,
   model= benthic.vg.fit.exp)
   head(benthic.pred)
   
   # Now there is a prediction model, if that prediction model is
   # extrapolated over our domain (`xy.surface`) the various plots can be
   # created
   
   

# Part 1. Create a Heatmap ------------------------------------------------

   #Create a heatmap
        #Make the prediction object (which is essentially a data frame) and
              #Make it into pixel data
   #In order to create a heatmap, it is first necessary to take
   #the `sp` prediction object and make it into pixel data,
   #otherwise the heatmap won't look quite right:


class(benthic.pred)
gridded(benthic.pred) = TRUE
class(benthic.pred)


spplot(benthic.pred["var1.pred"],main="Predictions of Benthic Index - ordinary kriging")

# Part 2: Contour Map -----------------------------------------------------
  #Recall from Week 11/Exercise 10, that contour and surface plots
  #work a little differently to ordinary plots.

  #A domain grid of equally spaced x and y values is required and
  #A matrix of z values that can be overlayed onto that domain grid
  #in order to provide the surface values.
  
  #Method one
  z_surface_matrix <- data.matrix(benthic.pred[1])
  
  
  #Method two (From Lecture 10 of Wk. 11)
  # The method from Lecture 10/Wk. 11 involved predicting over
  # the square domain of xy.surface, this won't work here
  # because instead of the predict function, the krige function
  # was used, hence it is necessary to convert the output from
  # the krige function, into the output that would have been 
  # given by the predict function, which is the required
  # z surface matrix.
  
  
  
  contour(xgrid, ygrid, z_surface_matrix,
          levels=seq(1, 5, by=0.5), labcex=0.75,
          xlab="-Longitude (degrees West)", ylab="Latitude (degrees North)")
  
  
  contour(xgrid, ygrid, z_surface_matrix,
          xlab="-Longitude (degrees West)", ylab="Latitude (degrees North)",
          main = "contour plot of Krige Prediction",
          labcex = 0.8)
  mtext("Krige using a 4th degree polynomial model")
  
# Part 3 - Surface Plot ---------------------------------------------------

#Base Plot
  persp(xgrid, ygrid, z_surface_matrix,
        xlim = c(-77.3, -75.9),   #the values domain needs to be adjusted above
        ylim = c(38.1, 39.5),          #Make sure to set an appropriate domain
        # zlim = c(0, 6),
        theta = -44, phi = 30, d = 0.5,
        xlab="-Longitude (degrees West)",
        ylab="Latitude (degrees North)",
        zlab="Benthic Index", ticktype = "detailed")
  title(main=paste("Surface Plot of Benthic Index", "Loess Smoothing", sep="\n"))    
  






  






