#Set working directory
setwd("J:/intro to data modelling/assignment")

mydata <- read.csv("diabetes.csv")
attach(mydata)

plot(BloodPressure~Age)
plot(fit)

fit=lm(BloodPressure~Age)
summary(fit)

par(mfrow=c(2,2))
plot(fit)

plot(BloodPressure~Age,ylab = "Blood Pressure",xlab ="Age")
abline(a=56.0009,b=0.3942)

install.packages("boot")
library(boot)


summary(fit)
par(mfrow=c(2,2))
plot(fit)

anova (fit)
anova (fit2)

library(boot)
set.seed (17)
cv.error.10= rep (0,10)
for (i in 1:10) {
  glm.fit=glm(BloodPressure~poly(Age ,i),data=mydata)
  cv.error.10[i]=cv.glm (mydata ,glm.fit ,K=10) $delta [1]
}
cv.error.10

head(mydata)

fit2<-lm(BloodPressure~Age+I(Age^2),data = mydata)
summary(fit2)
plot(fit2)
View(mydata$Age)
View(mydata$BloodPressure)


mydata$BloodPressure[is.na(mydata$BloodPressure)] <- median(mydata$BloodPressure,na.rm = TRUE)

head(mydata)

cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(BloodPressure~poly(Age,i),data=mydata)
  cv.error[i]=cv.glm(mydata,glm.fit)$delta[1]
}
cv.error

library (boot)
glm.fit=glm(BloodPressure~Age ,data=mydata)
cv.err =cv.glm(mydata ,glm.fit)
cv.err$delta

