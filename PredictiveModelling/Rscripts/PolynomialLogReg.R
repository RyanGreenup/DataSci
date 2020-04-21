
# Preamble ----------------------------------------------------------------


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

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

#Load data
all.df <- read.csv(file = "practical06b.csv", TRUE, ",")
all.df$y_numeric <- all.df$y
all.df$y_factor  <- as.factor(all.df$y)
all.df <- all.df[,-c(colnames(all.df)=="y")]

# Plot the Data -----------------------------------------------------------

PlotCol.vec <- rainbow_hcl(2)

plot(x2 ~ x1, col = PlotCol.vec[c(y_factor)], data = all.df, cex = 2,
     xlab = "Predictor 1",
     ylab = "Predictor 2",
     main = "Categorised Variables" )

col.plot <- ggplot(all.df, aes(y = x2, x = x1, col = y_factor)) +
  geom_point() + 
  theme_classic() +
  labs(x = "Predictor 1", y = "Predictor 2",
       title = "Discrete Outcome From Predictor Values",
       col = "Output")

col.plot


# Create the Logistic Regression Model ------------------------------------
fit <- glm(formula = y_factor ~ x1+I(x1^2)+x2+I(x2^2), family = 'binomial', data = all.df)

#Plot both types of infinite parabaloids

res   <- 20
xgrid <- seq(min(all.df$x1), max(all.df$x1), length.out = res)
ygrid <- seq(min(all.df$x2), max(all.df$x2), length.out = res)

inf.para.z1 <- xgrid^2 + ygrid^2

x <- xgrid[1]
y <- ygrid[1]
z <- inf.para.z1
zgrid <- as.data.frame(matrix(nrow = res, ncol = res))
row.names(zgrid) <- signif(ygrid, 2)
names(zgrid) <- signif(xgrid, 2)
zgrid

for (j in 1:res) {
  
  for (i in 1:res) {
    
    x <- xgrid[i]
    y <- ygrid[j]
    zgrid[j,i] <- x^2+y^2
    
    parab.twoterm.z <- zgrid
    
    zgrid[j,i] <- x^2+y^2 + x + y
  
    parab.fourterm.z <- zgrid
    
  }
  
}


plot_ly(z = ~ as.matrix(parab.twoterm.z)) %>% add_surface()
plot_ly(z = ~ as.matrix(parab.fourterm.z)) %>% add_surface()

max <- sin(pi/4)
max^2 + max^2



log.mod.circle


#Plot the Decision Boundary
mydata <- read.csv("practical06b.csv")
mycolours <- c("red", "blue")
plot(mydata$x1, mydata$x2, col=mycolours[1+mydata$y], xlab="x1", ylab="x2")
plot(all.df$x1, all.df$x2)
x1 <- c(-50:50)/10
w0 <- fit$coefficients[1]
w1 <- fit$coefficients[2]
w2 <- fit$coefficients[3]
w3 <- fit$coefficients[4]
w4 <- fit$coefficients[5]
x2a <- sqrt( w2/2*w4 - w0/w4 - w1/w4*x1 - w3/w4*(x1^2))
x2b <- -sqrt( w2/2*w4 - w0/w4 - w1/w4*x1 - w3/w4*(x1^2))
points(x1,x2a,type="l")
points(x1,x2b,type="l")

cols.vec <- rainbow_hcl(3)
plot(x2~x1, col = cols.vec[y_factor], data = all.df)
x1 <- c(-50:50)/10
w0 <- fit$coefficients[1]
w1 <- fit$coefficients[2]
w2 <- fit$coefficients[3]
w3 <- fit$coefficients[4]
w4 <- fit$coefficients[5]
x2a <- sqrt( w2/2*w4 - w0/w4 - w1/w4*x1 - w3/w4*(x1^2))
x2b <- -sqrt( w2/2*w4 - w0/w4 - w1/w4*x1 - w3/w4*(x1^2))
points(x1,x2a,type="l")
points(x1,x2b,type="l")


PlotCol.vec <- rainbow_hcl(3)

plot(x2 ~ x1, col = PlotCol.vec[c(y_factor)], data = all.df, cex = 2,
     xlab = "Predictor 1",
     ylab = "Predictor 2",
     main = "Categorised Variables" )
points(x1,x2a, type="l", lty = 3, col = PlotCol.vec[3], lwd = 3)
points(x1,x2b, type="l", lty = 3, col = PlotCol.vec[3], lwd = 3)

col.plot <- ggplot(all.df) +
  geom_point(data = all.df, aes(y = x2, x = x1, col = y_factor)) + 
  theme_classic() +
  labs(x = "Predictor 1", y = "Predictor 2",
       title = "Discrete Outcome From Predictor Values",
       col = "Output") +
  geom_line(data = data.frame(x1 = x1, mod = x2.mine), aes(x = x1, y = x2a), col = "Purple", lwd = 3) #+
  geom_line(data = data.frame(x1 = x1, mod = x2b), aes(x = x1, y = x2b), col = "Purple", lwd = 3) +
  geom_


col.plot

y <- predict(fit)
x1 <- all.df$x1

x2.mine <- sqrt(w3^2-4*w4*(w2*x1^2+w1*x1+w0-y)+w3)/2*w4
x2.prof <- sqrt( w2/2*w4 - w0/w4 - w1/w4*x1 - w3/w4*(x1^2))

x2.mine
