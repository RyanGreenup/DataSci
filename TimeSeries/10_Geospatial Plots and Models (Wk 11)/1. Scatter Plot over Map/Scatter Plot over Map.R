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


# Base Plot ---------------------------------------------------------------

plot(x = lon, y = lat, xlab = "Longitude", ylab = "Lattitude", 
     main = "Sampling Station Locations")


# ggplot2 ------------------------------------------------------------------

scatterplot <- ggplot(data = benthic, aes(x = lon, y = lat, col = index))

scatterplot +
  geom_jitter(alpha = 0.5, width = 0.1, size = 2) +
  labs(x = "longitude", y = 'lattitude',
       title = "Sampling Station Locations",
       col = "Benthic Index") +
  theme_light() +
  #Choose nicer colours
  scale_color_continuous(low = 'royalblue', high = 'indianred')


# Plot with map (ggmap) ---------------------------------------------------

bbox <- make_bbox(lon, lat, f = 0.01)
map <- get_map(location = bbox, maptype = 'watercolor')
ggmap(base_layer = ggplot(data = benthic,
                          aes(x = lon, y = lat, col = index)),
      ggmap = map) +
  geom_jitter(width = 0.1, height = 0.1, size = index, alpha = 0.8) +
  scale_color_continuous(low = 'black', high = 'indianred') +
  labs(x = 'Longitude', y = 'Lattitude',
       title = "Benthic Index",
       subtitle = "Chesapeake Sanctuary, Maryland",
       caption = 'Higher index value  is better(smaller and blacker)',
       col = 'Benthic Index', size = 'Benthic Index')


