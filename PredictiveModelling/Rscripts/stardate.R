
# Preamble ----------------------------------------------------------------

##Load the Packages
if(require('pacman')){
  library('pacman')
}else{
  install.packages('pacman')
  library('pacman')
}

pacman::p_load(xts, sp, gstat, ggplot2, rmarkdown, reshape2, ggmap,
               parallel, dplyr, plotly)

#Change the Working Directory to the folder location
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
rm(this.dir)

##Load the dataset
star.df <- read.csv(file = "stardate.csv", header = TRUE, sep = ",")


# Create Assignments ------------------------------------------------------
head(star.df)
date     <- star.df$date
stardate <- star.df$stardate


# Create the Linear Model -------------------------------------------------

star.lm <- lm(stardate ~ date)


# Plot the Data and Model -------------------------------------------------
plot(x = date, y = stardate,
     xlab = "Date",
     ylab = "Stardate",
     main = "Stardates", 
     pch = 20,
     col = "royalblue", cex = 2)
abline(star.lm, col = "Indianred", lty = 6, cex = 4)


# Predict a new value -----------------------------------------------------
predict(object = star.lm, data.frame(date = 2356.1))
