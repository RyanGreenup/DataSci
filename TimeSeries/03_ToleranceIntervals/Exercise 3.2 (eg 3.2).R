#install.packages(EnvStats)
library(EnvStats)
library(qcc)


###Import the Data Set
#library(readr)
#Arsenic <- read_csv("Arsenic.csv")

#Is the data distributed lognormally or normally?
x <- Arsenic$Background
xb <- mean(Arsenic$Background)
sd <- sd(Arsenic$Background)

arse_z <- pnorm(x/sd-xb/sd, mean = 0, sd = 1)
arse_zlog <- pnorm(log(x)/sd-xb/sd, mean = 0, sd = 1)

plot(arse_z, col="red")
plot(arse_zlog, col="blue")
hist(Arsenic$Background)
hist(log(Arsenic$Background))


if(0.05 < shapiro.test(Arsenic$Background)[2]){
  print("The data isn't not normal")
  
}else(print("The data is not normally distributed"))

if(0.05 < shapiro.test(log(Arsenic$Background))[2]){
  print("The data isn't not log-normal")
  
}else(print("The data is not log-normally distributed"))

  

#Use the predIntNorm command as found in the Help docs



Prediction_Interval <- predIntLnorm(
              Arsenic$Background,
              n.geomean = 1,
              k = 4,
              method = "exact",
              pi.type = "upper",
              conf.level = 0.95
                                    )

#What is the Upper Value of the Prediction Interval?
UPL <- Prediction_Interval$interval$limits[2]
LPL <- 0

#Has the Upper Value of the prediction interval, established by the Background data been exceeded by the Compliance sample?
if(all(Arsenic$Compliance[1:8]<UPL)){
                              print("The 4 Values from either year fall within the prediction interval, there is no sign of contamination")
  
                                          }else{print("The prediction interval has been exceeded, there may be signs of contamination")}

#Plot the Prediction Interval, the Minimum amount of possible Arsenic is 0 ppb, not -inf.

qcc(Arsenic$Background, type="xbar", sizes=2, limits=c(LPL, UPL), xlab = "Observation Number", ylab = "PPB of Arsenic", data.name = "Arsenic Concentration")
    





















