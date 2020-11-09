#install.packages('caTools')

library(caTools)
library(dplyr)

#Import data

read.csv('C:\\Users\\rawat kundan\\OneDrive\\Documents\\R_Classes\\Assignment\\diabetes.csv',stringsAsFactors = T)->diabetes_data

#SPLIT DATA IN 70/30 RATIO

split_flag<-sample.split(diabetes_data$Outcome,SplitRatio=0.80)

traindata<-subset(diabetes_data,split_flag==TRUE)
testdata<-subset(diabetes_data,split_flag==FALSE)

#IMPORT RPART MODEL FOR FITTING DATA
library(rpart)

#FIT DATA INSIDE RPART MODEL
dia_model<-rpart(traindata$Outcome~.,data = traindata)

#SUMMARY MODEL
summary(dia_model)

#IMPORT RATTEL FOR BETTER USER FRIENDLY DECISION TREE CHART
library(rattle)

#PLOT OUR MODEL
fancyRpartPlot(dia_model)

#FIT TEST DATA IN YOUR MODEL WITH TYPE= "class"
pre_dia<-predict(dia_model,newdata = testdata, type = "class")

#CREATE YOUR CONFUSION MATRIX AND CHECK THE ALL THREE PERCENTAGES RECALL , ACCURACY , PRECESION
#ACTUAL AND PREDICTED TEST DATA
table(testdata$Outcome,pre_dia)
