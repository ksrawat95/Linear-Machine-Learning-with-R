#install.packages('caTools')

library(caTools)
library(dplyr)

#Import data

read.csv('C:\\Users\\rawat kundan\\OneDrive\\Documents\\R_Classes\\Assignment\\Customer-Churn.csv',stringsAsFactors = T)->Cust_data

#SPLIT DATA IN 70/30 RATIO
split_flag<-sample.split(Cust_data$MonthlyCharges,SplitRatio=0.75)
traindata<-subset(Cust_data,split_flag==TRUE)
testdata<-subset(Cust_data,split_flag==FALSE)


# PLOT AND CHECK MODEL IS LINEAR OR NOT
#plot(MonthlyCharges~tenure,data=traindata,col="green")

#abline(lm(MonthlyCharges~tenure,data=traindata))

#CREATE MODEL ON THE BASE OF TRAIN DATA
trainingmodel<-lm(MonthlyCharges~tenure+Contract+OnlineSecurity+MultipleLines+TechSupport+StreamingMovies+StreamingTV+OnlineBackup +InternetService,data=traindata)

#Residual standard error TRY TO MINIMIZE
#Adjusted R-squared TRY TO MAXIMIZE
summary(trainingmodel)

#FIT TEST DATA IN YOUR MODEL
Pridictmodel<-predict(trainingmodel,newdata=testdata)

#COMPARE THE ACTUAL AND PREDICTED VALUE OF TEST DATA
testresult<-cbind(Actual = testdata$MonthlyCharges,Predicted = Pridictmodel)
head(testresult)
finaldata<-data.frame(testresult)

#FIND THE ERROR BY ACTUAL - PREDICTED
error<-(finaldata$Actual-finaldata$Predicted)
head(error)

resultfinal<-cbind(finaldata$Actual,finaldata$Predicted,error)
resultfinal

#FIND ROOT MEAN SQUARE VALUE AND TRY TO MAKE IT MINIMUM
sqrt(mean((error)^2))

