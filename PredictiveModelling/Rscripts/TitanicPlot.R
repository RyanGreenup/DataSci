
# Preamble ----------------------------------------------------------------
##Load Packages
if(require('pacman')){
  library('pacman')
}else{
  install.packages('pacman')
  library('pacman')
}

pacman::p_load(ggmap, plotly, EnvStats, ggplot2, GGally, corrplot, dplyr, tidyr,
               stringr, reshape2, cowplot, ggpubr, tidyverse, reshape2, ggplot2,
               rmarkdown, dplyr, plotly, rstudioapi, wesanderson, RColorBrewer,
               colorspace)
##Set the Working Directory
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

#Load data
all.df <- read.csv("titanictrain.csv", TRUE, ",")

#Base Plot
##Numeric
plot(x = all.df$Pclass, y = all.df$Age, data = all.df, col = PlotCol.vec[all.df$Survived + 1], pch = 16, cex = 2)

##Factors
###Create Categorical Factors
all.df$Pclass <- as.factor(all.df$Pclass)
all.df$Survived <- as.factor(all.df$Survived)
###Create the Plot (Box Plot)
PlotCol.vec <- rainbow_hcl(2)
plot(x = all.df$Pclass, y = all.df$Age, data = all.df,
     col = PlotCol.vec[all.df$Survived], pch = 16,
     xlab = "Passenger Class", ylab = "Passenger Age",
     main = "Titanic Survivors")

#GGplot
##Numeric
all.df <- read.csv("titanictrain.csv", TRUE, ",")
ggplot(all.df, aes(x = Pclass, y = Age, col = Survived)) +
  geom_point(lwd = 3) +
  guides(col = FALSE) +
  labs(title = "Titanic Survivors", x = "Passenger Class", y = "Passenger Age")

##Factors

###Create Categorical Factors
all.df$Pclass <- as.factor(all.df$Pclass)
all.df$Survived <- as.factor(all.df$Survived)

ggplot(all.df, aes(x = Pclass, y = Age, col = Survived)) +
  geom_point(lwd = 4) +
  labs(title = "Titanic Survivors", x = "Passenger Class", y = "Passenger Age")
