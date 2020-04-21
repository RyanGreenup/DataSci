
# Preamble ----------------------------------------------------------------
#Because this is loaded from a project file
#There is no need to set the home directory

test <- read.csv(file = "Data Sets/Benthic.csv")

#Library
if(require('pacman')){
  library('pacman')
  print("Package Loaded")
}else{
  install.packages('pacman')
  print("Package Installing")
  library('pacman')
  print("Pacman Loaded")
}

pacman::p_load(sp, EnvStats, gstat, ggplot2, rmarkdown)


#Assignments
benthic <- Benthic.df
lat <- benthic$Latitude
lon <- benthic$Longitude



# Create Variogram --------------------------------------------------------

coordinates(benthic) = ~Longitude + Latitude
benthic.vg <- variogram(Index ~ 1, data = benthic)
plot(benthic.vg, main="Figure 11.1  Empirical variogram for Benthic Index")


# Plot the Variogram in ggplot --------------------------------------------

p <- ggplot(data = benthic.vg, aes(x = dist, y = gamma, col = gamma, size = np)) +
  geom_point() +
  labs(size = "Number of Point Pairs", col = "Sample Variogram Estimate",
       y = "Sample Variogram estimate", x = "Distance",
       title = 'Benthic Variogram Plot') +
  theme_bw()
p


# Fit and plot Models (base) ---------------------------------------------
benthic.vg.fit.exp <- fit.variogram(benthic.vg, model=vgm(1,"Exp", 0.5,1))
plot(benthic.vg, benthic.vg.fit.exp, main="Exponential variogram for Benthic Index")


benthic.vg.fit.gau <- fit.variogram(benthic.vg, model=vgm(1,"Gau", 0.5,1))
plot(benthic.vg, benthic.vg.fit.gau, main="Gaussian variogram for Benthic Index")

benthic.vg.fit.sph <- fit.variogram(benthic.vg, model=vgm(1,"Sph", 0.5,1))
plot(benthic.vg, benthic.vg.fit.sph,main="Spherical variogram for Benthic Index")



# Directional Variograms --------------------------------------------------

#coordinates(benthic) = ~Longitude + Latitude #This is already done

benthic.dvg <- variogram(Index ~ 1, data=benthic, alpha=c(0, 45,90,135))
benthic.dvg.fit <- vgm(1,"Sph", 0.5,1,anis=c(30,0.4))
plot(benthic.dvg, benthic.dvg.fit, main="Figure 11.5  Directional variograms for Benthic Index")











