read.csv('C:\\Users\\rawat kundan\\OneDrive\\Documents\\R_Classes\\Assignment\\Customer-Churn.csv',stringsAsFactors = T)->customer_churn

library(caTools)  
sample.split(customer_churn$Churn,SplitRatio = 0.75)-> split_tag
subset(customer_churn, split_tag==T)->train
subset(customer_churn, split_tag==F)->test

library(randomForest)
# Number of variables randomly sampled as candidates at each split. 
# Note that the default values are different for classification (sqrt(p)
# where p is number of variables in x) 
# and regression (p/3)

randomForest(Churn~MonthlyCharges+tenure+gender+InternetService+Contract, data=train, mtry=3,ntree=100)-> mod_forest1
importance(mod_forest1)
varImpPlot(mod_forest1)
varImpPlot(mod_forest1, col="palegreen4")

predict(mod_forest1,newdata=test,type="class")->result_forest
head(result_forest)
table(test$Churn, result_forest)

