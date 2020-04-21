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
xgrid       <-  seq(min(lon), max(lon), length.out = 20)
ygrid       <-  seq(min(lat), max(lat), length.out = 20)

#Having already gone through this, I already know the 
#domain is going to have to be these values
#Otherwise the z-value has outliesrs (i.e. 50 instead of 5)
#This fucks up colour scales

xgrid       <-  seq(-77, -75.9, length.out = 20)
ygrid       <-  seq(38.2, 38.9, length.out = 20)

# Generate a dataframe with every possible combination of wt and hp
xy.surface  <- expand.grid(lon = xgrid, lat = ygrid)
# Feed the dataframe into the loess model and receive a matrix output 
#with estimates of the Z-value (Bethnic Index) 

index.fit   <-  predict(index.model, newdata = xy.surface)

# Abbreviated display of final matrix
index.fit[1:4, 1:4]


# Base Contour Plot -------------------------------------------------------


#Create a Base graphic of the contours
contour(x = xgrid, y = ygrid, z = index.fit,
        xlab = "Longitude",
        ylab = "Lattitude",
        main = "Bethnic Index")
mtext(paste("smoothing factor of", alpha))



# GGplot Contour Plot -----------------------------------------------------

#Create a ggplot2 Contour Plot
#Create a 'tidy' data frame (1 variable per column)
index.fit.melt <- melt(index.fit, varnames = c('Longitude', 'Latitude'),
                       value.name = "Benthic_Index") 
head(index.fit.melt)

#Use the regex to remove lon= rubbish  
index.fit.melt$Longitude <- as.numeric(
  str_sub(
    index.fit.melt$Longitude, 
    str_locate(index.fit.melt$Longitude,
               '=')[1,1] + 1))

index.fit.melt$Latitude <- as.numeric(
  str_sub(
    index.fit.melt$Latitude, 
    str_locate(index.fit.melt$Latitude,
               '=')[1,1] + 1))


ggplot(data = index.fit.melt, aes(x = Longitude,
                                  y = Latitude,
                                  z = Benthic_Index,
)) +
  geom_contour(binwidth = 2) +
  geom_raster(aes(fill = Benthic_Index)) +
  geom_contour(colour = "white", size = 0.75)



# GGplot Contour Plot with Map --------------------------------------------

bbox <- c("left"=-77, "bottom" = 38.2, "right" = max(lon), "top" = 38.9)
map <- get_map(location = bbox, maptype = 'watercolor')
ggmap(ggmap = map, base_layer = ggplot(
  data = index.fit.melt, 
  aes(x = Longitude, y = Latitude))) +
  geom_contour(binwidth = 0.25,
               data = index.fit.melt,
               aes(x = Longitude,
                   y = Latitude,
                   z = Benthic_Index,
                   colour = ..level..),
               size = .5) +
  labs(x = 'Longitude', y = 'Lattitude',
       title = "Benthic Index",
       subtitle = "Chesapeake Bay, Maryland",
       caption = 'Higher index value  is better',
       col = 'Benthic Index', size = 'Benthic Index') +
  scale_color_continuous(low = 'black', high = 'indianred') 



# GGplot Contour Plot with Map + Tile (Tile is slow for a large grid domain)

bbox <- c("left"=-77, "bottom" = 38.2, "right" = max(lon), "top" = 38.9)
map <- get_map(location = bbox, maptype = 'watercolor')
ggmap(ggmap = map, base_layer = ggplot(
  data = index.fit.melt, 
  aes(x = Longitude, y = Latitude))) +
  geom_contour(binwidth = 0.25,
               data = index.fit.melt,
               aes(x = Longitude,
                   y = Latitude,
                   z = Benthic_Index,
                   colour = ..level..),
               size = .5) +
  labs(x = 'Longitude', y = 'Lattitude',
       title = "Benthic Index",
       subtitle = "Chesapeake Bay, Maryland",
       caption = 'Higher index value  is better',
       col = 'Benthic Index', size = 'Benthic Index') +
  geom_tile(data = index.fit.melt, aes(fill = Benthic_Index), alpha = 0.2) +
  scale_color_continuous(low = 'black', high = 'indianred') 