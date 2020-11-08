library(caTools)
library(dplyr)

# READING FILE
read.csv('C:\\Users\\rawat kundan\\OneDrive\\Documents\\R_Classes\\Assignment\\Customer-Churn.csv',stringsAsFactors = T)->Cust_data
View(Cust_data)

#SPLIT DATA IN 70/30 RATIO
setf<-sample.split(Cust_data,SplitRatio = 0.70)

train<-subset(Cust_data,setf==T)
test<-subset(Cust_data,setf==F)

nrow(train)
nrow(test)

#CREATE GERNALIZE LINEAR MODEL WITH FAMILT = "binomail" WITH TRAIN DAtA 

model<-glm(Churn~tenure,data = train,family = "binomial")

#FIT TEST DATA IN YOUR MODEL WITH TYPE= "RESPONSE"
testmodel<-predict(model,newdata = test,type = "response")

# FOR SETTING THRESHHOLD VALUE TO CREATE A GOOD CONFUSION MATRIX SO RECALL,ACCURACY,PRECESION MORE THAN 75%
library("ROCR")

prediction(testmodel,test$Churn)->pre

## we gonna use true positive rate and false positive rate
rate<-performance(pre,'tpr','fpr')

#SELECT THE VALUE ON THE BASE OF PLOT 
plot(rate,colorize=T,print.cutoffs.at=seq(0.1,by=0.1))

#CREATE YOUR CONFUSION MATRIX AND CHECK THE ALL THREE PERCENTAGES RECALL , ACCURACY , PRECESION
table(test$Churn,testmodel>0.35)
