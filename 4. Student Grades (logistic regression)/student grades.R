getwd()
setwd('C:\\Users\\guest-pc\\Desktop\\Data Science\\R Studio\\R\\4. Student Grades')
mydata=read.csv(file.choose())
str(mydata)
mydata$admit=as.factor(as.character(mydata$admit))
mydata$rank=as.factor(as.character(mydata$rank))
xtabs(~admit+rank,data=mydata)

set.seed(1234)
ind=sample(2,nrow(mydata),replace=T,prob=c(0.8,0.2))
train=mydata[ind==1,]
test=mydata[ind==2,]

mymodel=glm(admit~gre+gpa+rank,data=train,family='binomial')
summary(mymodel)

p1=predict(mymodel,train,type='response')
head(p1)

mymodel=glm(admit~gpa+rank,data=train,family='binomial')
summary(mymodel)

p1=predict(mymodel,train,type='response')
head(p1)

head(train)
y=-4.7270+(1.3735*3.61)+(1*-1.1645)
y
exp(y)/(1+exp(y))
y=-4.7270+(1.3735*4)
y
exp(y)/(1+exp(y))

pred1=ifelse(p1>0.5,1,0)

tab1=table(Predicted=pred1,Actual=train$admit)
tab1
sum(diag(tab1))/sum(tab1)
1-sum(diag(tab1))/sum(tab1)

p2=predict(mymodel,test,type='response')
pred2=ifelse(p2>0.5,1,0)

tab2=table(Predicted=pred2,Actual=test$admit)
tab2
sum(diag(tab2))/sum(tab2)
1-(sum(diag(tab2))/sum(tab2))

#Use chi-squared test to predict goodness of fit (p-value)
with(mymodel,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail=F))

#Find the McFaddden's Pseudo R^2
modelchi=mymodel$null.deviance-mymodel$deviance
pseudo.r2=modelchi/mymodel$null.deviance
pseudo.r2




predicted.data=data.frame(
  probability.of.admit=mymodel$fitted.values,
  admit=train$admit)

predicted.data=predicted.data[
  order(predicted.data$probability.of.admit,decreasing = FALSE),]
predicted.data$rank=1:nrow(predicted.data)

library(ggplot2)
ggplot(data=predicted.data,aes(x=rank,y=probability.of.admit))+
  geom_point(aes(color=admit),alpha=1,shape=4,stroke=2)+
  xlab('Index')+ylab('Predicted probability of being admitted')+
  coord_cartesian(xlim = c(0,350), ylim = c(0,1), expand = TRUE)



setwd('C:\\Users\\guest-pc\\Desktop\\Data Science\\R Studio\\R\\4. Student Grades (logistic regression)')
