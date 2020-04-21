
##Set working directory and load dataset

setwd("M:/USB/NEWpredMOD/Week 8")
mydata<-read.csv("titanictrain.csv")


##Clean up data

mydata$Age[is.na(mydata$Age)]<-mean(mydata$Age, na.rm = TRUE)


##Standardise the dataset

StandPClass<-((mydata$Pclass-mean(mydata$Pclass))/sd(mydata$Pclass))
StandAge<-((mydata$Age-mean(mydata$Age))/sd(mydata$Age))
StandSipSp<-((mydata$SibSp-mean(mydata$SibSp))/sd(mydata$SibSp))
StandSex<-((as.numeric(mydata$Sex)-mean(as.numeric(mydata$Sex))/sd(as.numeric(mydata$Sex))))
StandParch<-((mydata$Parch-mean(mydata$Parch))/sd(mydata$Parch))


##Store means and Std Deviations

mymeans<-data.frame(Pclass=mean(mydata$Pclass), Age=mean(mydata$Age), SipSp=mean(mydata$SibSp), Sex=mean(as.numeric(mydata$Sex)), Parch=mean(mydata$Parch) )

mySds<-data.frame(Pclass=sd(mydata$Pclass), Age=sd(mydata$Age), SipSp=sd(mydata$SibSp), Parch=sd(mydata$Parch), Sex=sd(as.numeric(mydata$Sex) ) )


##Save standardised training and test set

newTrain <- read.csv("titanictrain.csv")

newTrain$Age <- (newTrain$Age - mymeans$Age)/mySds$Age
newTrain$Pclass <- (newTrain$Pclass - mymeans$Pclass)/mySds$Pclass
newTrain$SibSp <- (newTrain$SibSp - mymeans$SipSp)/mySds$SipSp
newTrain$Sex <- ((as.numeric(newTrain$Sex) - as.numeric(mymeans$Sex))/as.numeric(mySds$Sex) )
newTrain$Parch <- (newTrain$Parch - mymeans$Parch)/mySds$Parch

write.csv(newTrain, file ="new_train.csv")

newtest<-read.csv("titanictest.csv")
newtest$Age <- (newtest$Age - mymeans$Age)/mySds$Age
newtest$Pclass <- (newtest$Pclass - mymeans$Pclass)/mySds$Pclass
newtest$SibSp <- (newtest$SibSp - mymeans$SipSp)/mySds$SipSp
newtest$Sex <- (as.numeric(newtest$Sex) - as.numeric(mymeans$Sex))/as.numeric(mySds$Sex)
newtest$Parch <- (newtest$Parch - mymeans$Parch)/mySds$Parch

write.csv(newtest, file ="new_test.csv")

