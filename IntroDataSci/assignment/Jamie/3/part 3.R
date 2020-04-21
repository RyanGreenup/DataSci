#A hierachical cluster analysis of the 5 numeric variables using Euclidean distance Cluster analysis, and defining 2 clusters.(include dendrogram)

#Set working directory
setwd("J:/intro to data modelling/assignment/3")

mydata <- read.csv("diabetes.csv")
attach(mydata)

X = mydata[,1:9]
km = kmeans(X, centers = 2)

fitted(km, "classes")

pp = prcomp(X)
plot(pp$x[,1:2],col=fitted(km,"classes")+1, xaxt="n", yaxt="n")

#Compare the clusters
table(Outcome=mydata$Outcome, cluster=fitted(km,"classes"))

#solution for all cluster memebers
hh = hclust(dist(X),method = "complete")

cutree(hh, k=2)


plot(hh, xlab = "", sub = "Complete link cluster analysis")
rect.hclust(hh,k=2)
