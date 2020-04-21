#install.packages(EnvStats)
library(EnvStats)
library(plyr)


###Import the Data from the question
tccb_Ref <- c(0.22, 0.23, 0.26, 0.27, 0.28, 0.28, 0.29, 0.33, 0.34, 0.35, 0.38, 0.39, 0.39, 0.42, 0.42, 0.43, 0.45, 0.46, 0.48, 0.50, 0.50, 0.51, 0.52, 0.54, 0.56, 0.56, 0.57, 0.57, 0.60, 0.62, 0.63, 0.67, 0.69, 0.72, 0.74, 0.76, 0.79, 0.81, 0.82, 0.84, 0.89, 1.11, 1.13, 1.14, 1.14, 1.20, 1.33)
tccb_cleanup <- c(0.09, 0.09, 0.12, 0.12, 0.14, 0.16, 0.17, 0.17, 0.17, 0.18, 0.19, 0.20, 0.20, 0.21, 0.21, 0.22, 0.22, 0.22, 0.23, 0.24, 0.25, 0.25, 0.25, 0.25, 0.26, 0.28, 0.28, 0.29, 0.31, 0.33, 0.33, 0.33, 0.34, 0.37, 0.38, 0.39, 0.40, 0.43, 0.43, 0.47, 0.48, 0.48, 0.49, 0.51, 0.51, 0.54, 0.60, 0.61, 0.62, 0.75, 0.82, 0.85, 0.92, 0.94, 1.05, 1.10, 1.10, 1.19, 1.22, 1.33, 1.39, 1.39, 1.52, 1.53, 1.73, 2.35, 2.46, 2.59, 2.61, 3.06, 3.29, 5.56, 6.61, 18.40, 51.97, 168.64)



#Is the data normal or log normal?
if( 0.0001 < shapiro.test(tccb$TcCB)[2] ){print("It could be normal")}else{print("This data is non-normal")}
if( 0.0001 < shapiro.test(log(tccb$TcCB))[2] ){print("It could be log-normal")}else{print("This data is NOT log-normal")}

hist(tccb$TcCB)
hist(log(tccb$TcCB))

#The data appears slightly more log-normal than normal.



#Create an upper tolerance limit, with content coverage

ticonup <- tolIntLnorm(tccb_Ref, coverage = 0.95, cov.type = "content", ti.type = "upper", conf.level = 0.95)$interval$limits[2]

#Create an upper tolerance limit, with expectation coverage

tiexpup <- tolIntLnorm(tccb_Ref, coverage = 0.95, cov.type = "expectation", ti.type = "upper", conf.level = 0.95)$interval$limits[2]

#Create a prediction interval and use the upper limit

piup <- predIntLnorm(tccb_Ref, n.geomean = 1, k = length(tccb_cleanup), method = "exact", pi.type = "upper", conf.level = 0.95)$interval$limits[2]


#Is there contamination present at the clean up site?
if(!all(tccb$TcCB < ticonup)){print("The concentration value has exceeded the content tolerance limit")}else{print("The values are within the content tolerance limit")}
if(!all(tccb$TcCB < tiexpup)){print("The concentration value has exceeded the expectation tolerance limit")}else{print("The values are within the expectation tolerance limit")}

  # For a prediction interval, it seems to me that data falling outside may not be stong evidence of contamination, hence I will rely on the tolerance interval
  # and use a high proportion, say 10% of data to determine pass/fail

  if(all(tccb$TcCB < piup)){print("The values all fall within the prediction Interval")}else if(table(tccb_cleanup < piup)["TRUE"]*100/length(tccb_cleanup) > 90){print("more than 90% of the observations are within the prediction interval")}else{print("More than 10% of values fall outside the prediction interval")}



#As a conclusion, confirm whether contamination is present

if(table(tccb$TcCB < piup)["TRUE"]*100/124 < 90 | !all(tccb$TcCB < ticonup) | !all(tccb$TcCB > tiexpup)){
  print("There is evidence to suggest that the TcCB concentration has increased, contamination may be present")
  }else(
    print("There is not enough evidence to conclude that the TcCB concentration has significantly changed")
  )








































