#install.packages(EnvStats)
library(EnvStats)
library(qcc)


###Import the Data Set
#library(readr)
#Arsenic <- read_csv("Arsenic.csv")

#Is the data distributed normally?

if(0.05 < shapiro.test(Arsenic$Background)[2]){
  print("The data isn't not normal")
  
}



#Use the predIntNorm command as found in the Help docs



Prediction_Interval <- predIntNorm(
  Arsenic$Background,
  n.mean = 1,
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














