#Data Science Assignment
#1. A linear regression analysis of Blood pressure against Age

setwd("J:/intro to data modelling/assignment/1")
mydata <- read.csv("diabetes.csv")
attach(mydata)
lm(BloodPressure~Age)



#a. A plot of the data
plot(BloodPressure~Age,ylab = "Blood Pressure",xlab ="Age")
abline(a=56.0009,b=0.3942)

# b.& c. A discussion of the significance of the slope and the R^2 statistic
fit=lm(BloodPressure~Age)
summary(fit)
par(mfrow=c(2,2))
plot(fit)

#d. the LOOCV estimate of the mean square error
require(boot)
glm.fit=glm(BloodPressure~Age,data = mydata)

#Leave-one-out-cv
cv.glm(mydata,glm.fit)$delta

#The 1st result being the cv result.
#The 2nd result being bias correct version of the 1st.
fit3=lm(BloodPressure~Age+I(Age^2))
summary(fit3)
fit4=lm(BloodPressure ~ poly(Age, 3, raw=TRUE))
summary(fit4)
fit5=lm(BloodPressure ~ poly(Age, 4, raw=TRUE))
summary(fit5)
fit6=lm(BloodPressure ~ poly(Age, 5, raw=TRUE))
summary(fit6)
fit7=lm(BloodPressure ~ poly(Age, 5, raw=TRUE))
summary(fit6)
cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(BloodPressure~poly(Age,i),data = mydata)
  cv.error[i]=cv.glm(mydata,glm.fit)$delta[1]
}
cv.error

