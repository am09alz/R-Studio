getwd()
setwd('C:\\Users\\guest-pc\\Desktop\\Data Science\\R Studio\\R\\New folder')
data=read.csv(file.choose())
str(data)
data$NSP=as.ordered(data$NSP)
data$Tendency=as.factor(data$Tendency)
summary(data)
xtabs(~NSP+Tendency,data)

#Partition data
ind=sample(2,nrow(data),replace=TRUE,prob = c(0.8,0.2))
train=data[ind==1,]
test=data[ind==2,]

#Ordinal Logistic Regression or Prportional Odds Logistic Regression
library(MASS)
model=polr(NSP~.-Max-LB-MSTV-Nmax-Nzeros-Median-FM-MLTV,train,Hess = TRUE)
summary(model)
#p-value calculation
ctable=coef(summary(model))
ctable
p=pnorm(abs(ctable[,'t value']),lower.tail = FALSE)*2
p
ctable=cbind(ctable,'p value'=p)
ctable

#Prediction
pred=predict(model,train[1:5,],type='prob')
pred=predict(model,train)
print(pred,digits=3)

#Confusion Matrix and Error for training data
tab=table(pred,train$NSP)
tab
1-sum(diag(tab))/sum(tab)

#Confusion Matrix and Error for training data
pred1=predict(model,test)
tab1=table(pred1,test$NSP)
tab1
1-sum(diag(tab1))/sum(tab1)

