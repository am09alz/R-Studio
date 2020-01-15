install.packages('psych')
install.packages('caret')
install.packages('glmnet')
library('psych')

#Data
data=read.csv(file.choose())
str(data)
data$chas=as.factor(as.character(data$chas))
data$rad=as.numeric(data$rad)
data$tax=as.numeric(data$tax)
levels(data$chas)
head(data$chas)

pairs.panels(data[-4,-14],cex=2)

#Partition
set.seed(222)
ind=sample(2,nrow(data),replace=T,prob=c(0.7,0.3))
train=data[ind==1,]
test=data[ind==2,]

#Custom control parameters
custom=trainControl(method='repeatedcv',
                    number=10,
                    repeats=5,
                    verboseIter = T)

#Linear Model
set.seed(1234)
lm=train(medv~.,
         train,
         method='lm',
         trControl=custom)
lm
lm$results
summary(lm)
plot(lm$finalModel)

#Ridge Regression
set.seed(1234)
ridge=train(medv~.,
            train,
            method='glmnet',
            tuneGrid=expand.grid(alpha=0,
                                 lambda=seq(0.0001,1,length=100)),
            trControl=custom)

ridge
ridge$results
coef(ridge$finalModel, ridge$bestTune$lambda)

plot(ridge)
ridge$bestTune
plot(ridge$finalModel,xvar = 'lambda',label = T)
plot(ridge$finalModel,xvar='dev',label=T)
plot(varImp(ridge,scale=T))

x=(train[,1:13])
predictions=predict(ridge,x,s=ridge$bestTune$lambda)
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, train$medv),
  Rsquare = R2(predictions, train$medv))

#Non caret method
grid = 10^ seq (10,-1, length =100) ## set lambda sequence
x=data.matrix(train[,1:13]) #OR x=model.matrix(train$medv~.,data = train)[,-1] 
y=data.matrix(train[,14])  #OR y=train$medv

ridge.cv = cv.glmnet(x,y, alpha = 0), lambda = grid)
plot(ridge.cv)
ridge.cv$lambda.min
ridge.cv$lambda.1se
coef(ridge.cv,ridge.cv$lambda.min)
min(ridge.cv$cvm)

predictions=predict(ridge.cv,x,s=ridge.cv$lambda.min)
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, train$medv),
  Rsquare = R2(predictions, train$medv))






#Lasso Regression
set.seed(1234)
lasso=train(medv~.,
            train,
            method='glmnet',
            tuneGrid=expand.grid(alpha=1,
                                 lambda=seq(0.0001,0.2,length=5)),
            trControl=custom)
lasso
lasso$results
summary(lasso)
plot(lasso)
plot(lasso$finalModel,xvar = 'lambda',label=T)
plot(lasso$finalModel,xvar = 'dev',label=T)
plot(varImp(lasso,scale=T))

grid = 10^seq(5,-2,length=100) ## set lambda sequence
x2=data.matrix(train[,1:13])
y2=data.matrix(train[,14])
lasso.cv = cv.glmnet(x,y, alpha = 1, lambda = grid)
plot(lasso.cv)
lasso.cv$lambda.min
lasso.cv$lambda.1se
coef(lasso.cv,lasso.cv$lambda.min)
min(lasso.cv$cvm)

#Elastic Net Regression
set.seed(1234)
en=train(medv~.,
         train,
         method='glmnet',
         tuneGrid=expand.grid(alpha=seq(0,1,length=10),
                                        lambda=seq(0.0001,1,length=5)),
         trControl=custom)
en
en$results
summary(en)
plot(en)
plot(en$finalModel,xvar = 'lambda',label=T)
plot(en$finalModel,xvar = 'dev',label = T)
plot(varImp(en,scale=T))
coef(en$finalModel, en$bestTune$lambda)

x=(train[,1:13])
predictions=predict(en,train,s=en$bestTune)
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, train$medv),
  Rsquare = R2(predictions, train$medv))



#Comparing the models
model_list=list(LinearModel=lm,Ridge=ridge,Lasso=lasso,ElasticNet=en)
res=resamples(model_list)
summary(res)

#xy plots for comparison (MAE, RMSE, R^2)
xyplot(res,metric="RMSE")
xyplot(res,models=c("LinearModel","ElasticNet"),metric="RMSE")
splom(res,metric="RMSE")
parallelplot(res)

scales = list(x=list(relation="free"), y=list(relation="free"))
dotplot(res,scales=scales)

scales = list(x=list(relation="free"), y=list(relation="free"))
densityplot(res, scales=scales, pch = "|")

scales = list(x=list(relation="free"), y=list(relation="free"))
bwplot(res, scales=scales)

#Example of comparing p values
compare_models(lm,en)

#Finding the best model
en$bestTune
best=en$finalModel
coef(best,s=en$bestTune$lambda)

#Save final model for later use
saveRDS(en, 'final_model.rds')
fm=readRDS('final_model.rds')
print(fm)

#Prediction
p1=predict(fm,train)
RMSEpredict= sqrt(mean((train$medv-p1)^2))

p2=predict(fm,test)
RMSEtest= sqrt(mean((test$medv-p2)^2))
