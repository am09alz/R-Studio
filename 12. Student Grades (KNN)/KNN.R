setwd('C:\\Users\\guest-pc\\Desktop\\Data Science\\R Studio\\R\\12. Student Grades (KNN)')
library('caret')
library(pROC)
library(mlbench)

#Data
data=read.csv(file.choose())
str(data)
data$admit[data$admit==0]='No' #Since not factor yet, we can rename (no need to change to character first)
data$admit[data$admit==1]='Yes'
data$admit=as.factor(data$admit)

#Data Partition
set.seed(1234)
ind=sample(2,nrow(data),replace=T,prob=c(0.7,0.3))
training=data[ind==1,]
test=data[ind==2,]

#KNN Model
trControl=trainControl(method = 'repeatedcv',
                       number=10,
                       repeats = 3,
                       classProbs = TRUE, #calc predicted class probabilities (also needed for calculating ROC)
                       summaryFunction = twoClassSummary) #calcs sensitiv,specitiv and ROC area under curve

set.seed(222)
fit=train(admit~.,
          data=training,
          method='knn',
          tuneLength=20, #automatic k calc
          trControl=trControl,
          preProc=c('center','scale'), #scaling for equal calculation. first finds center(mean), then scale(sd)
          metric='ROC',
          tuneGrid=expand.grid(k=1:60)) #manual k calc

#Model Performance
fit
print(fit)
plot(fit)
varImp(fit)
pred=predict(fit, newdata = test)
confusionMatrix(pred,test$admit)

########################################################################################
#Example 2 Boston Housing
data=read.csv(file.choose())
str(data)

#Data Partition
set.seed(1234)
ind=sample(2,nrow(data),replace=T,prob=c(0.7,0.3))
training=data[ind==1,]
test=data[ind==2,]


#KNN Model
trControl=trainControl(method = 'repeatedcv',
                       number=10,
                       repeats = 3)

set.seed(333)
fit=train(medv~.,
          data=training,
          method='knn',
          metric='Rsquared',
          trControl=trControl,
          preProc=c('center','scale'),
          tuneGrid=expand.grid(k=1:70))

#Model Performance
fit
plot(fit)
varImp(fit)
pred=predict(fit,newdata=test)
RMSE(pred,test$medv)
plot(pred~test$medv)
