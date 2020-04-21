#Set working directory
setwd("J:/intro to data modelling/assignment/4")

#Assign and attach file
mydata <- read.csv("diabetes.csv")
attach(mydata)

dim(mydata)

#A logistic regression analysis of the outcome against the other 5 numeric variables
fit1 = glm(Outcome~Pregnancies+Glucose+BloodPressure+BMI+Age,family = "binomial",data = mydata)

#Table of regression coefficients and p-values
summary(fit1)

coef(fit1)

summary(fit1$coefficients)

glm.probs = predict(fit1,type = "response")
glm.probs[1:10]


#???
##contrasts(Outcome)

#Convert predicted prob.s to 0 or 1
glm.pred = rep("0",768)
glm.pred[glm.probs>.5]="1"



#Confusion matrix
table(glm.pred, Outcome)
173/724
require(boot)
glm.fit=fit1

#Leave-one-out-cv
cv.glm(mydata,glm.fit)$delta

fit2=glm(Outcome~Pregnancies+Glucose+BMI+Age,family = "binomial",data = mydata)
glm.fit2=fit2

#Leave-one-out-cv
cv.glm(mydata,glm.fit2)$delta
