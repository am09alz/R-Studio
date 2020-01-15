install.packages('ROCR')

setwd()
getwd()
setwd('C:\\Users\\guest-pc\\Desktop\\Data Science\\R Studio\\R\\7. AUC')
binary=read.csv(file.choose())

str(binary)

#Logistic Regression Model
library(nnet)
mymodel=multinom(admit~.,data=binary)

#Misclassification Rate
p=predict(mymodel,binary)
tab=table(p,binary$admit)
tab
sum(diag(tab))/sum(tab)
1-sum(diag(tab))/sum(tab)
table(binary$admit)

#Model Performance Evaluation
pred=predict(mymodel,binary,type='prob')
head(pred)
head(binary)
hist(pred)
pred=prediction(pred,binary$admit)
eval=performance(pred,'acc')
plot(eval)
abline(h=0.71,v=0.45) #abline based on eyeballing graph

#Identify best values
max=which.max(slot(eval,'y.values')[[1]])
acc=slot(eval,'y.values')[[1]][max]
cut=slot(eval,'x.values')[[1]][max]
print(c(Accuracy=acc,Cutoff=cut))

#Reciever Operating Characteristic (ROC) Curve
pred=prediction(pred,binary$admit)
roc=performance(pred,'tpr','fpr')
plot(roc,
     colorize=T,
     main='ROC Curve',
     ylab='Sensitivity',
     xlab='1-Sensitivity')
abline(a=0,b=1)

#Area Under Curve (AUC)
auc=performance(pred,'auc')
auc=unlist(slot(auc,'y.values'))
auc=round(auc,4)
legend(.4,.4,auc,title='AUC',cex = 0.75,bty = 'n',text.col = 'black')


#Random Forest
binary$admit=as.factor(as.character(binary$admit))
library(randomForest)
set.seed(222)
rf=randomForest(admit~.,data = binary,
                ntree=300,
                importance=TRUE,
                proximity=TRUE)


confusionMatrix(pred2,binary$admit)
print(rf)
attributes(rf)
table(pred2,binary$admit)
#Prediction and Confusion Matrix
pred2=predict(rf,binary,type = 'prob')
table(Predicted=pred2,Actual=binary$admit)

pred_table=ifelse(pred2>0.5,1,0)
tab1=table(Predicted=pred_table,Actual=binary$admit)
tab1

pred2 = predict(rf,type="prob",newdata=binary)[,2]


#Error rate of random forest
plot(rf)
str(pred2)
#Plot roc
pred2=prediction(pred2,binary$admit)
roc2=performance(pred2,'tpr','fpr')
plot(roc2,colorize=T,add=TRUE)

#Area Under Curve (AUC)
auc2=performance(pred2,'auc')
auc2=unlist(slot(auc2,'y.values'))
auc2=round(auc2,4)
legend(.6,.4,auc2,title='AUC',cex = 0.75,bty = 'n',text.col = 'black')

#Cost
cost = performance(pred, "cost")
plot(cost),

#Identify best values
max=which.min(slot(cost,'y.values')[[1]])
acc=slot(cost,'y.values')[[1]][max]
cut=slot(cost,'x.values')[[1]][max]
print(c(Accuracy=acc,Cutoff=cut))


pred= predict(mymodel, data = binary, type = "probs")
pred= ifelse(pred > 0.4683497 , 1, 0)
confusionMatrix(data = pred, 
                reference = binary$admit, 
                positive = "1")
summary(table(pred,binary$admit))
