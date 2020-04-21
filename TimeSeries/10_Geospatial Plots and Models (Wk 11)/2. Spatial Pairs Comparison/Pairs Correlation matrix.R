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


# Correlation Matrix ------------------------------------------------------

par(cex = 2, mex = 0.5)
cormat <- cor(benthic[,-(1:4)], use = 'complete.obs', method = 'pearson') 
corrplot(method = 'ellipse', type = 'lower', corr = cormat )


# Base Scatterplot matrix -------------------------------------------------

pairs(formula = ~index + sal + silt, labels = c('Index', "Saline", "Silt"))


# ggplot2 scatterplot matrix ----------------------------------------------

ggpairs(data = benthic[,-(1:4)], mapping = ggplot2::aes(alpha =0.5, col = 'orchid3'),
        lower = list(
          continuous=wrap(
            "points",
            col = 'mediumpurple3',
            alpha = 0.3,
            size = 1.2,
            position=position_jitter(height=0.4, width=0.4))))+
  theme_classic()
