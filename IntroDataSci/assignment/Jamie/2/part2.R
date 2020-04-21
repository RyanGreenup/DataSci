#Set working directory
setwd("J:/intro to data modelling/assignment/2")

#Assign and attach file
mydata <- read.csv("diabetes.csv")
attach(mydata)

summary(mydata)

obj = prcomp(mydata[,1:9])
biplot(obj,scale = 0)

screeplot(obj)
View(mydata)

plot(obj$x[,1:2],pch=16,asp = 1)
