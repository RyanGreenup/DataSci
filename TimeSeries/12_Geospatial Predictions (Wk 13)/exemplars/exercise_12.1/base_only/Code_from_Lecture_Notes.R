library(sp)
library(EnvStats)
library(gstat)
data(Benthic.df)
coordinates(Benthic.df)=~Longitude+Latitude
vg.benthic <- variogram(Index ~ Longitude + Latitude +
                          Longitude^2 + Longitude*Latitude + Latitude^2 +
                          Longitude^3 + Longitude^2*Latitude +
                          Longitude*Latitude^2 + Latitude^3 + Longitude^4+                          Longitude^3*Latitude + Longitude^2*Latitude^2 +
                          Longitude*Latitude^3 + Latitude^4, data=Benthic.df)
vg.fit.benthic<-fit.variogram(vg.benthic, model=vgm(1,"Exp", 0.5,1))


lat <- Benthic.df$Latitude
lon <- Benthic.df$Longitude
Latitude <- seq(min(lat), max(lat), length=20)
Longitude <- seq(min(lon), max(lon), length=20)
predict.list <- list(Longitude=Longitude,Latitude=Latitude)
predict.grid <- expand.grid(predict.list)
class(predict.grid)
coordinates(predict.grid) = ~Longitude+Latitude

gridded(predict.grid) = TRUE

krige.fit.benthic <- krige(formula = Index ~1,
                           locations =  Benthic.df,
                           newdata = predict.grid,
                           model= vg.fit.benthic)
head(krige.fit.benthic)
head(benthic.predict)
spplot(krige.fit.benthic["var1.pred"],main="Figure 12.1  Predictions of Benthic Index - ordinary kriging")

class(predict.grid)
class(benthic.pred)



index.chull <- chull(lon, lat)
inside <- point.in.polygon(point.x = predict.grid$Longitude,point.y = predict.grid$Latitude,pol.x = lon[index.chull],pol.y = lat[index.chull])
krige.fit.benthic[inside == 0] <- NA

contour(Longitude, Latitude, data.matrix(krige.fit.benthic), levels=seq(1, 5, by=0.5), labcex=0.75,xlab="-Longitude (degrees West)", ylab="Latitude (degrees North)")

title(main=paste("Figure 12.2  Contour Plot of Benthic Index", "ordinary kriging", sep="\n"))
persp(Longitude, Latitude, data.matrix(krige.fit.benthic), xlim = c(-77.3, -75.9), ylim = c(38.1, 39.5), zlim = c(0, 6), theta = -45, phi = 30, d = 0.5, xlab="-Longitude (degrees West)",
      ylab="Latitude (degrees North)",
      zlab="Benthic Index", ticktype = "detailed")
title(main=paste("Figure 12.3  Surface Plot of Predictions of Benthic Index", "Based on Kriging", sep="\n"))
